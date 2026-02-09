// This file implements the Hybrid language parser that builds AST nodes and manages parsing state.

#include "parser/parser_internal.h"
#include "parser.h"
#include "lexer.h"
#include "compiler_session.h"
#include "toplevel.h"
#include <cstdio>
#include <cmath>
#include <cctype>
#include <limits>
#include <set>
#include <vector>
#include <optional>
#include <utility>
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Function.h"


enum class AccessSpecifier {
  Default,
  Public,
  Private,
  Protected
};

struct PendingMemberModifiers {
  AccessSpecifier access = AccessSpecifier::Default;
  bool hasExplicitAccess = false;
  bool isStatic = false;
  bool isConst = false;
  bool isAbstract = false;
  bool isVirtual = false;
  bool isOverride = false;
};

static bool ParsePendingMemberModifiers(PendingMemberModifiers &out) {
  PendingMemberModifiers pending;
  bool parsing = true;

  while (parsing) {
    switch (CurTok) {
    case tok_public:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Public;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_private:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Private;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_protected:
      if (pending.hasExplicitAccess) {
        reportCompilerError("Duplicate access modifier", "Only one of public, private, or protected may be specified");
        return false;
      }
      pending.access = AccessSpecifier::Protected;
      pending.hasExplicitAccess = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_static:
      if (pending.isStatic) {
        reportCompilerError("Duplicate 'static' modifier");
        return false;
      }
      pending.isStatic = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_const:
      if (pending.isConst) {
        reportCompilerError("Duplicate 'const' modifier");
        return false;
      }
      pending.isConst = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_abstract:
      if (pending.isAbstract) {
        reportCompilerError("Duplicate 'abstract' modifier");
        return false;
      }
      pending.isAbstract = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_virtual:
      if (pending.isVirtual) {
        reportCompilerError("Duplicate 'virtual' modifier");
        return false;
      }
      pending.isVirtual = true;
      getNextToken();
      SkipNewlines();
      break;
    case tok_override:
      if (pending.isOverride) {
        reportCompilerError("Duplicate 'override' modifier");
        return false;
      }
      pending.isOverride = true;
      getNextToken();
      SkipNewlines();
      break;
    default:
      parsing = false;
      break;
    }
  }

  out = pending;
  return true;
}

static MemberModifiers FinalizeMemberModifiers(const PendingMemberModifiers &pending,
                                              AggregateKind kind) {
  MemberModifiers modifiers;
  if (kind == AggregateKind::Struct) {
    modifiers.access = MemberAccess::PublicReadWrite();
  } else {
    modifiers.access = MemberAccess::ReadPublicWritePrivate();
  }

  switch (pending.access) {
  case AccessSpecifier::Public:
    modifiers.access = MemberAccess::PublicReadWrite();
    break;
  case AccessSpecifier::Private:
    modifiers.access = MemberAccess::PrivateOnly();
    break;
  case AccessSpecifier::Protected:
    modifiers.access = MemberAccess::ProtectedReadWrite();
    break;
  case AccessSpecifier::Default:
    break;
  }

  if (pending.isStatic)
    modifiers.storage |= StorageFlag::Static;
  if (pending.isConst)
    modifiers.storage |= StorageFlag::Const;
  modifiers.isAbstract = pending.isAbstract;
  modifiers.isVirtual = pending.isVirtual;
  modifiers.isOverride = pending.isOverride;
  if (modifiers.isAbstract)
    modifiers.isVirtual = true;

  return modifiers;
}

class ScopedValueKeyword {
  ParserContext &Parser;
  bool Previous = false;

public:
  explicit ScopedValueKeyword(bool enable)
      : Parser(currentParser()), Previous(Parser.allowValueIdentifier) {
    Parser.allowValueIdentifier = enable;
  }

  ~ScopedValueKeyword() { Parser.allowValueIdentifier = Previous; }
};

static bool isPascalCase(const std::string &name) {
  if (name.empty())
    return false;

  if (!std::isalpha(static_cast<unsigned char>(name.front())) ||
      !std::isupper(static_cast<unsigned char>(name.front())))
    return false;

  for (char c : name) {
    if (c == '_')
      return false;
  }

  return true;
}

static void setDiagnosticLocation(SourceLocation loc) {
  if (!loc.isValid())
    return;
  currentParser().currentTokenLocation = loc;
  currentLexer().setTokenStart(loc);
}

static bool ValidateParameterDefaults(const std::vector<Parameter> &Args) {
  bool sawDefault = false;
  for (const auto &param : Args) {
    if (param.IsParams)
      return true;
    if (param.HasDefault) {
      sawDefault = true;
      continue;
    }

    if (sawDefault) {
      setDiagnosticLocation(param.NameLocation);
      reportCompilerError(
          "Parameters with default values must be the trailing parameters");
      return false;
    }
  }

  return true;
}

static bool ConvertParenInitializerToConstructor(std::unique_ptr<ExprAST> &Initializer,
                                                 const TypeInfo &declInfo) {
  if (declInfo.pointerDepth > 0 || declInfo.isArray)
    return false;
  if (declInfo.typeName.empty())
    return false;

  auto *paren = dynamic_cast<ParenExprAST *>(Initializer.get());
  if (!paren)
    return false;

  auto isKnownComposite = [](const std::string &name) {
    return StructNames.contains(name) ||
           ClassNames.contains(name) ||
           IsActiveStructName(name) ||
           IsActiveClassName(name);
  };

  const std::string &fullTypeName = declInfo.typeName;
  std::string constructorName = fullTypeName;

  bool knownComposite = isKnownComposite(fullTypeName);
  if (!knownComposite && !declInfo.baseTypeName.empty() &&
      declInfo.baseTypeName != fullTypeName) {
    knownComposite = isKnownComposite(declInfo.baseTypeName);
    if (knownComposite)
      constructorName = fullTypeName;
  }

  if (!knownComposite)
    return false;

  std::vector<std::unique_ptr<ExprAST>> args;
  if (paren->isTuple()) {
    args = paren->takeElements();
  } else {
    auto single = paren->takeSingleElement();
    if (single)
      args.push_back(std::move(single));
  }

  Initializer = std::make_unique<CallExprAST>(constructorName, std::move(args));
  return true;
}

/// prototype
///   ::= id id '(' (id id (',' id id)*)? ')'  // C-style: returntype name(type param, type param, ...)
std::unique_ptr<PrototypeAST> ParsePrototype(bool isUnsafe) {
  // Check for ref return type
  bool returnsByRef = false;
  if (CurTok == tok_ref) {
    returnsByRef = true;
    getNextToken(); // eat 'ref'
  }

  // Parse return type
  if (!IsValidType() && CurTok != '(')
    return LogErrorP("Expected return type in prototype");

  TypeInfo ReturnTypeInfo;
  if (!ParseCompleteTypeInfo(ReturnTypeInfo, returnsByRef))
    return LogErrorP("Failed to parse return type");

  std::string FnName;
  if (CurTok == tok_identifier) {
    FnName = IdentifierStr;
    getNextToken();
  } else if (CurTok == tok_string_literal) {
    FnName = StringVal;
    getNextToken();
  } else {
    return LogErrorP("Expected function name in prototype");
  }

  std::vector<std::string> GenericParams;
  if (ParseGenericParameterList(GenericParams))
    SkipNewlines();

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");

  GenericParameterScope parameterScope(GenericParams);

  std::vector<Parameter> Args;
  getNextToken(); // eat '('
  SkipNewlines();

  if (CurTok != ')') {
    // Parse parameters
    while (true) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      // Check for ref parameter
      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(')
        return LogErrorP("Expected parameter type");

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef))
        return LogErrorP("Failed to parse parameter type");

      if (CurTok != tok_identifier)
        return LogErrorP("Expected parameter name");

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;

      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return nullptr;
      }

      Args.push_back(std::move(param));

      if (CurTok == ')') {
        getNextToken(); // eat ')'
        break;
      }

      if (CurTok != ',')
        return LogErrorP("Expected ',' or ')' in parameter list");

      getNextToken(); // eat ','
      SkipNewlines();
    }
  } else {
    getNextToken(); // eat ')'
  }

  if (!ValidateParameterDefaults(Args))
    return nullptr;

  return std::make_unique<PrototypeAST>(std::move(ReturnTypeInfo), FnName,
                                        std::move(Args), isUnsafe, returnsByRef,
                                        std::move(GenericParams));
}

/// definition ::= prototype block
/// Note: 'unsafe' is handled by the caller (HandleUnsafe), which sets the context
std::unique_ptr<FunctionAST> ParseDefinition() {
  // Check if we're in an unsafe context (set by HandleUnsafe)
  bool isUnsafe = isInUnsafeContext();

  auto Proto = ParsePrototype(isUnsafe);
  if (!Proto) {
    return nullptr;
  }

  auto Body = ParseBlock();
  if (!Body) {
    return nullptr;
  }

  return std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
}

/// external ::= 'extern' ['unsafe'] prototype
std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken(); // eat extern.
  SkipNewlines();

  bool explicitUnsafe = false;
  if (CurTok == tok_unsafe) {
    explicitUnsafe = true;
    getNextToken(); // eat 'unsafe'
    SkipNewlines();
    enterUnsafeContext();
  }

  bool prototypeIsUnsafe = explicitUnsafe || isInUnsafeContext();
  auto Proto = ParsePrototype(prototypeIsUnsafe);
  if (Proto)
    Proto->markAsExtern();

  if (explicitUnsafe)
    exitUnsafeContext();
  return Proto;
}

/// delegate ::= 'delegate' ['ref'] type identifier '(' params? ')'
std::unique_ptr<DelegateDeclAST> ParseDelegateDefinition() {
  getNextToken(); // eat 'delegate'
  SkipNewlines();

  bool returnsByRef = false;
  if (CurTok == tok_ref) {
    returnsByRef = true;
    getNextToken(); // eat 'ref'
    SkipNewlines();
  }

  if (!IsValidType() && CurTok != '(') {
    reportCompilerError("Expected return type in delegate declaration");
    return nullptr;
  }

  TypeInfo returnTypeInfo;
  if (!ParseCompleteTypeInfo(returnTypeInfo, returnsByRef)) {
    reportCompilerError("Failed to parse delegate return type");
    return nullptr;
  }

  if (CurTok != tok_identifier) {
    reportCompilerError("Expected delegate name after return type");
    return nullptr;
  }

  std::string delegateName = IdentifierStr;
  SourceLocation nameLoc = currentParser().currentTokenLocation;
  getNextToken();

  if (StructNames.contains(delegateName) || ClassNames.contains(delegateName) ||
      DelegateNames.contains(delegateName)) {
    reportCompilerError("Type '" + delegateName + "' is already defined");
    return nullptr;
  }

  SkipNewlines();
  if (CurTok != '(') {
    reportCompilerError("Expected '(' in delegate declaration");
    return nullptr;
  }

  std::vector<Parameter> params;
  getNextToken(); // eat '('
  SkipNewlines();

  if (CurTok != ')') {
    while (true) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        reportCompilerError("Expected parameter type");
        return nullptr;
      }

      TypeInfo paramTypeInfo;
      if (!ParseCompleteTypeInfo(paramTypeInfo, paramIsRef)) {
        reportCompilerError("Failed to parse parameter type");
        return nullptr;
      }

      if (CurTok != tok_identifier) {
        reportCompilerError("Expected parameter name");
        return nullptr;
      }

      std::string paramName = IdentifierStr;
      SourceLocation paramNameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(paramTypeInfo);
      param.Name = paramName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(paramTypeInfo);
      param.NameLocation = paramNameLoc;
      param.ParamsLocation = paramsLoc;

      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return nullptr;
      }

      params.push_back(std::move(param));

      if (CurTok == ')') {
        getNextToken(); // eat ')'
        break;
      }

      if (CurTok != ',') {
        reportCompilerError("Expected ',' or ')' in delegate parameter list");
        return nullptr;
      }

      getNextToken(); // eat ','
      SkipNewlines();
    }
  } else {
    getNextToken(); // eat ')'
  }

  if (!ValidateParameterDefaults(params))
    return nullptr;

  DelegateNames.insert(delegateName);
  return std::make_unique<DelegateDeclAST>(
      std::move(returnTypeInfo), delegateName, std::move(params),
      returnsByRef, nameLoc);
}

/// returnstmt ::= 'return' ['ref'] expression?
std::unique_ptr<ReturnStmtAST> ParseReturnStatement() {
  getNextToken(); // eat 'return'

  // Check if this is a void return (no expression)
  if (CurTok == tok_newline || CurTok == ';' || CurTok == '}') {
    // Return with no value
    return std::make_unique<ReturnStmtAST>(nullptr, false);
  }

  // Check for ref return
  bool returnsByRef = false;
  if (CurTok == tok_ref) {
    returnsByRef = true;
    getNextToken(); // eat 'ref'
  }

  auto ReturnValue = ParseExpression();
  if (!ReturnValue)
    return nullptr;

  return std::make_unique<ReturnStmtAST>(std::move(ReturnValue), returnsByRef);
}

/// variabledecl ::= ['ref'] type identifier '=' expression
std::unique_ptr<VariableDeclarationStmtAST> ParseVariableDeclaration(bool isRef) {
  ParserContext parserBackup = currentParser();
  LexerContext lexerBackup = currentLexer();

  if (!IsValidType() && CurTok != '(')
    return nullptr;

  TypeInfo declInfo;
  if (!ParseCompleteTypeInfo(declInfo, isRef)) {
    currentParser() = parserBackup;
    currentLexer() = lexerBackup;
    return nullptr;
  }

  if (CurTok != tok_identifier) {
    currentParser() = parserBackup;
    currentLexer() = lexerBackup;
    return nullptr;
  }

  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier

  if (CurTok == ',') {
    LogError("Expected '=' after variable name. Multiple variable declarations in a single statement are not allowed");
  }

  if (CurTok != '=') {
    LogError("Variable must be initialized",
             "Add an initializer, for example: `int count = 0`.");
    return nullptr;
  }

  getNextToken(); // eat '='

  // Check if the initializer has 'ref' keyword (for linking two variables)
  bool initializerIsRef = false;
  if (CurTok == tok_ref) {
    initializerIsRef = true;
    getNextToken(); // eat 'ref'
  }

  auto Initializer = ParseExpression();
  if (!Initializer)
    return nullptr;

  if (!initializerIsRef)
    ConvertParenInitializerToConstructor(Initializer, declInfo);

  // If initializer has ref, wrap it in RefExprAST
  if (initializerIsRef) {
    Initializer = std::make_unique<RefExprAST>(std::move(Initializer));
  }

  return std::make_unique<VariableDeclarationStmtAST>(std::move(declInfo), Name, std::move(Initializer), isRef);
}

static bool TryParseForTypeHeader(TypeInfo &outInfo, bool declaredRef) {
  ParserContext parserBackup = currentParser();
  LexerContext lexerBackup = currentLexer();

  TypeInfo speculativeType;
  if (ParseCompleteTypeInfo(speculativeType, declaredRef) &&
      CurTok == tok_identifier) {
    outInfo = std::move(speculativeType);
    return true;
  }

  currentParser() = parserBackup;
  currentLexer() = lexerBackup;
  return false;
}

/// foreachstmt ::= 'for' type identifier 'in' expression block
std::unique_ptr<ForEachStmtAST> ParseForEachStatement() {
  getNextToken(); // eat 'for'
  
  // Skip newlines after 'for'
  while (CurTok == tok_newline)
    getNextToken();

  bool isRef = false;
  if (CurTok == tok_ref) {
    isRef = true;
    getNextToken(); // eat 'ref'
    while (CurTok == tok_newline)
      getNextToken();
  }

  TypeInfo declInfo;
  if (!TryParseForTypeHeader(declInfo, isRef)) {
    if (isRef)
      LogError("Expected type after 'ref' in foreach loop");
    else
      LogError("Expected type after 'for'");
    return nullptr;
  }
  
  // Parse variable name
  if (CurTok != tok_identifier) {
    LogError("Expected identifier after type in foreach loop");
    return nullptr;
  }
  
  std::string VarName = IdentifierStr;
  getNextToken(); // eat identifier
  
  // Parse 'in'
  if (CurTok != tok_in) {
    LogError("Expected 'in' after variable name in foreach loop");
    return nullptr;
  }
  getNextToken(); // eat 'in'
  
  // Parse collection expression
  auto Collection = ParseExpression();
  if (!Collection) {
    LogError("Expected expression after 'in' in foreach loop");
    return nullptr;
  }
  
  // Parse body block
  LoopNestingDepth++;
  auto Body = ParseBlock();
  LoopNestingDepth--;
  if (!Body) {
    LogError("Expected block after foreach loop header");
    return nullptr;
  }

  return std::make_unique<ForEachStmtAST>(std::move(declInfo), VarName, std::move(Collection), std::move(Body));
}

/// usestmt ::= 'use' identifier
std::unique_ptr<UseStmtAST> ParseUseStatement() {
  getNextToken(); // eat 'use'
  
  // Skip newlines after 'use'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse module name
  if (CurTok != tok_identifier) {
    LogError("Expected module name after 'use'");
    return nullptr;
  }
  
  std::string Module = IdentifierStr;
  getNextToken(); // eat module name
  
  return std::make_unique<UseStmtAST>(Module);
}

/// ifstmt ::= 'if' expression block ('else' (ifstmt | block))?
std::unique_ptr<IfStmtAST> ParseIfStatement() {
  getNextToken(); // eat 'if'
  
  // Skip newlines after 'if'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse condition expression
  auto Condition = ParseConditionExpression();
  if (!Condition) {
    LogError("Expected condition after 'if'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse then block
  if (CurTok != '{') {
    LogError("Expected '{' after if condition");
    return nullptr;
  }
  
  auto ThenBranch = ParseBlock();
  if (!ThenBranch)
    return nullptr;
  
  // Skip newlines after then block
  while (CurTok == tok_newline)
    getNextToken();
  
  // Check for else branch
  std::unique_ptr<StmtAST> ElseBranch = nullptr;
  if (CurTok == tok_else) {
    getNextToken(); // eat 'else'
    
    // Skip newlines after 'else'
    while (CurTok == tok_newline)
      getNextToken();
    
    if (CurTok == tok_if) {
      // else if case
      ElseBranch = ParseIfStatement();
    } else if (CurTok == '{') {
      // else block case
      ElseBranch = ParseBlock();
    } else {
      LogError("Expected '{' or 'if' after 'else'");
      return nullptr;
    }
    
    if (!ElseBranch)
      return nullptr;
  }
  
  return std::make_unique<IfStmtAST>(std::move(Condition), std::move(ThenBranch), std::move(ElseBranch));
}

/// whilestmt ::= 'while' expression block
std::unique_ptr<WhileStmtAST> ParseWhileStatement() {
  getNextToken(); // eat 'while'
  
  // Skip newlines after 'while'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse condition expression
  auto Condition = ParseConditionExpression();
  if (!Condition) {
    LogError("Expected condition after 'while'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse body block
  if (CurTok != '{') {
    LogError("Expected '{' after while condition");
    return nullptr;
  }

  LoopNestingDepth++;
  auto Body = ParseBlock();
  LoopNestingDepth--;
  if (!Body)
    return nullptr;

  return std::make_unique<WhileStmtAST>(std::move(Condition), std::move(Body));
}

/// breakstmt ::= 'break'
std::unique_ptr<BreakStmtAST> ParseBreakStatement() {
  if (LoopNestingDepth == 0) {
    LogError("'break' statement can only be used inside a loop");
    return nullptr;
  }
  getNextToken(); // eat 'break'
  return std::make_unique<BreakStmtAST>();
}

/// skipstmt ::= 'skip'
std::unique_ptr<SkipStmtAST> ParseSkipStatement() {
  if (LoopNestingDepth == 0) {
    LogError("'skip' statement can only be used inside a loop");
    return nullptr;
  }
  getNextToken(); // eat 'skip'
  return std::make_unique<SkipStmtAST>();
}

/// assertstmt ::= 'assert' expression
std::unique_ptr<AssertStmtAST> ParseAssertStatement() {
  SourceLocation assertLoc = currentLexer().tokenStart();
  getNextToken(); // eat 'assert'

  // Skip newlines after 'assert'
  while (CurTok == tok_newline)
    getNextToken();

  // Parse the condition expression
  auto Condition = ParseConditionExpression();
  if (!Condition) {
    LogError("expected expression after 'assert'");
    return nullptr;
  }

  // Try to evaluate the condition as a constant expression
  ConstantValue constVal(false);
  if (EvaluateConstantExpression(Condition.get(), constVal)) {
    // Check if the condition is a boolean
    if (constVal.type == ConstantValue::BOOLEAN) {
      if (!constVal.boolVal) {
        LogError("Assert condition evaluates to false at compile time");
        return nullptr;
      }
    } else {
      // Convert to boolean for evaluation
      bool boolVal = false;
      if (constVal.type == ConstantValue::INTEGER) {
        boolVal = constVal.intVal != 0;
      } else if (constVal.type == ConstantValue::FLOAT) {
        boolVal = constVal.floatVal != 0.0;
      }

      if (!boolVal) {
        LogError("Assert condition evaluates to false at compile time");
        return nullptr;
      }
    }
  }

  return std::make_unique<AssertStmtAST>(std::move(Condition),
                                         assertLoc.line,
                                         assertLoc.column);
}

/// unsafeblock ::= 'unsafe' block
std::unique_ptr<UnsafeBlockStmtAST> ParseUnsafeBlock() {
  getNextToken(); // eat 'unsafe'

  // Skip newlines after 'unsafe'
  while (CurTok == tok_newline)
    getNextToken();

  // Enter unsafe context
  enterUnsafeContext();

  // Parse the block
  auto Body = ParseBlock();
  if (!Body) {
    LogError("expected block after 'unsafe'");
    exitUnsafeContext();
    return nullptr;
  }

  // Exit unsafe context
  exitUnsafeContext();

  return std::make_unique<UnsafeBlockStmtAST>(std::move(Body));
}

/// switchstmt ::= 'switch' expression '{' case* '}'
std::unique_ptr<SwitchStmtAST> ParseSwitchStatement() {
  getNextToken(); // eat 'switch'
  
  // Skip newlines after 'switch'
  while (CurTok == tok_newline)
    getNextToken();
  
  // Parse switch expression
  auto Condition = ParseExpression();
  if (!Condition) {
    LogError("Expected expression after 'switch'");
    return nullptr;
  }
  
  // Skip newlines before '{'
  while (CurTok == tok_newline)
    getNextToken();
  
  if (CurTok != '{') {
    LogError("Expected '{' after switch condition");
    return nullptr;
  }
  
  getNextToken(); // eat '{'
  
  // Parse cases
  std::vector<std::unique_ptr<CaseAST>> Cases;
  
  // Skip newlines after opening brace
  while (CurTok == tok_newline)
    getNextToken();
  
  while (CurTok != '}' && CurTok != tok_eof) {
    // Skip newlines between cases
    if (CurTok == tok_newline) {
      getNextToken();
      continue;
    }
    
    if (CurTok == tok_case || CurTok == tok_default) {
      auto Case = ParseCase(false); // false = statement case
      if (!Case)
        return nullptr;
      Cases.push_back(std::move(Case));
    } else {
      LogError("Expected 'case' or 'default' in switch statement");
      return nullptr;
    }
    
    // Skip newlines after case
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}') {
    LogError("Expected '}' at end of switch statement");
    return nullptr;
  }
  
  getNextToken(); // eat '}'
  
  return std::make_unique<SwitchStmtAST>(std::move(Condition), std::move(Cases));
}

/// case ::= 'case' values '{' statements '}' | value* '=>' expression
/// default ::= 'default' '{' statements '}' | 'default' '=>' expression
std::unique_ptr<CaseAST> ParseCase(bool isExpression) {
  bool isDefault = false;
  std::vector<std::unique_ptr<ExprAST>> Values;
  
  if (CurTok == tok_default) {
    isDefault = true;
    getNextToken(); // eat 'default'
  } else if (CurTok == tok_case) {
    getNextToken(); // eat 'case'
    
    // Parse comma-separated values for the case
    do {
      // Skip newlines
      while (CurTok == tok_newline)
        getNextToken();
        
      auto Value = ParseExpression();
      if (!Value) {
        LogError("Expected case value");
        return nullptr;
      }
      Values.push_back(std::move(Value));
      
      // Check for comma (multiple values)
      if (CurTok == ',') {
        getNextToken(); // eat ','
      } else {
        break;
      }
    } while (true);
  } else {
    // In switch expressions, might have direct values without 'case'
    if (isExpression) {
      do {
        // Skip newlines
        while (CurTok == tok_newline)
          getNextToken();
          
        auto Value = ParseExpression();
        if (!Value) {
          LogError("Expected case value");
          return nullptr;
        }
        Values.push_back(std::move(Value));
        
        // Check for comma (multiple values)
        if (CurTok == ',') {
          getNextToken(); // eat ','
        } else {
          break;
        }
      } while (true);
    } else {
      LogError("Expected 'case' or 'default'");
      return nullptr;
    }
  }
  
  // Skip newlines
  while (CurTok == tok_newline)
    getNextToken();
  
  if (isExpression) {
    // Expression case: expect => expression
    if (CurTok != tok_lambda) {
      LogError("Expected '=>' in switch expression case");
      return nullptr;
    }
    
    getNextToken(); // eat '=>'
    
    // Skip newlines after '=>'
    while (CurTok == tok_newline)
      getNextToken();
    
    auto Expression = ParseExpression();
    if (!Expression) {
      LogError("Expected expression after '=>'");
      return nullptr;
    }
    
    return std::make_unique<CaseAST>(std::move(Values), std::move(Expression), isDefault);
  } else {
    // Statement case: expect { statements }
    if (CurTok != '{') {
      LogError("Expected '{' after case in switch statement");
      return nullptr;
    }
    
    auto Body = ParseBlock();
    if (!Body)
      return nullptr;
    
    return std::make_unique<CaseAST>(std::move(Values), std::move(Body), isDefault);
  }
}

/// Helper to determine which type of for loop and parse accordingly
std::unique_ptr<StmtAST> ParseForStatement() {
  getNextToken(); // eat 'for'
  
  // Skip newlines after 'for'
  while (CurTok == tok_newline)
    getNextToken();

  bool isRef = false;
  if (CurTok == tok_ref) {
    isRef = true;
    getNextToken(); // eat 'ref'
    while (CurTok == tok_newline)
      getNextToken();
  }

  TypeInfo declInfo;
  bool parsedTypeHeader = false;

  if (isRef || IsValidType())
    parsedTypeHeader = TryParseForTypeHeader(declInfo, isRef);

  if (isRef && !parsedTypeHeader) {
    LogError("Expected type after 'ref' in for loop");
    return nullptr;
  }

  if (parsedTypeHeader) {
    if (CurTok != tok_identifier) {
      LogError("Expected identifier after type in for loop");
      return nullptr;
    }

    std::string type = typeNameFromInfo(declInfo);
    std::string varName = IdentifierStr;
    getNextToken(); // eat identifier

    // Now check what comes next: 'in' for foreach, '=' for for-to
    if (CurTok == tok_in) {
      getNextToken(); // eat 'in'

      // Parse collection expression
      auto Collection = ParseExpression();
      if (!Collection) {
        LogError("Expected expression after 'in' in foreach loop");
        return nullptr;
      }

      // Parse body block
      LoopNestingDepth++;
      auto Body = ParseBlock();
      LoopNestingDepth--;
      if (!Body) {
        LogError("Expected block after foreach loop header");
        return nullptr;
      }

      return std::make_unique<ForEachStmtAST>(std::move(declInfo), varName,
                                              std::move(Collection), std::move(Body));
    } else if (CurTok == '=') {
      if (isRef) {
        LogError("ref is not supported for for-to loops");
        return nullptr;
      }

      // It's a for-to loop, parse it
      getNextToken(); // eat '='

      // Parse initialization expression
      auto InitExpr = ParseExpression();
      if (!InitExpr) {
        LogError("Expected initialization expression after '=' in for loop");
        return nullptr;
      }

      // Expect 'to' keyword
      if (CurTok != tok_to) {
        LogError("Expected 'to' after initialization in for loop");
        return nullptr;
      }
      getNextToken(); // eat 'to'

      // Check if next is the loop variable (for condition syntax)
      std::unique_ptr<ExprAST> LimitExpr = nullptr;
      std::unique_ptr<ExprAST> CondExpr = nullptr;

      if (CurTok == tok_identifier && IdentifierStr == varName) {
        // This is a condition expression like "i < size"
        // Parse the full condition expression
        CondExpr = ParseExpression();
        if (!CondExpr) {
          LogError("Expected condition expression after 'to' in for loop");
          return nullptr;
        }
      } else {
        // This is a regular limit expression
        LimitExpr = ParseExpression();
        if (!LimitExpr) {
          LogError("Expected limit expression after 'to' in for loop");
          return nullptr;
        }
      }

      // Check for optional 'by' clause
      std::unique_ptr<ExprAST> StepExpr = nullptr;
      char StepOp = '+';  // Default to addition
      if (CurTok == tok_by) {
        getNextToken(); // eat 'by'

        // Check for optional operator (* / % -)
        if (CurTok == '*' || CurTok == '/' || CurTok == '%' || CurTok == '-') {
          StepOp = CurTok;
          getNextToken(); // eat operator
        }

        StepExpr = ParseExpression();
        if (!StepExpr) {
          LogError("Expected step expression after 'by' in for loop");
          return nullptr;
        }
      }

      // Parse body block
      LoopNestingDepth++;
      auto Body = ParseBlock();
      LoopNestingDepth--;
      if (!Body) {
        LogError("Expected block after for loop header");
        return nullptr;
      }

      return std::make_unique<ForLoopStmtAST>(type, varName, std::move(InitExpr),
                                               std::move(LimitExpr), std::move(Body),
                                               std::move(StepExpr), StepOp,
                                               std::move(CondExpr));
    } else {
      LogError("Expected 'in' or '=' after variable name in for loop");
      return nullptr;
    }
  }
  
  // Check for anonymous loop: for <expr> to <expr>
  // Reset to parse as anonymous loop
  // Need to re-parse from after 'for'
  // Current token should be the first expression
  
  // Parse initial expression (could be a number or identifier)
  auto InitExpr = ParseExpression();
  if (!InitExpr) {
    LogError("Expected expression or type after 'for'");
    return nullptr;
  }
  
  // Expect 'to' keyword
  if (CurTok != tok_to) {
    LogError("Expected 'to' after initial expression in anonymous for loop");
    return nullptr;
  }
  getNextToken(); // eat 'to'
  
  // Parse limit expression
  auto LimitExpr = ParseExpression();
  if (!LimitExpr) {
    LogError("Expected limit expression after 'to' in anonymous for loop");
    return nullptr;
  }
  
  // Check for optional 'by' clause
  std::unique_ptr<ExprAST> StepExpr = nullptr;
  char StepOp = '+';  // Default to addition
  if (CurTok == tok_by) {
    getNextToken(); // eat 'by'
    
    // Check for optional operator (* / % -)
    if (CurTok == '*' || CurTok == '/' || CurTok == '%' || CurTok == '-') {
      StepOp = CurTok;
      getNextToken(); // eat operator
    }
    
    StepExpr = ParseExpression();
    if (!StepExpr) {
      LogError("Expected step expression after 'by' in anonymous for loop");
      return nullptr;
    }
  }
  
  // Parse body block
  LoopNestingDepth++;
  auto Body = ParseBlock();
  LoopNestingDepth--;
  if (!Body) {
    LogError("Expected block after anonymous for loop header");
    return nullptr;
  }
  
  // Create an anonymous variable name
  static int AnonCounter = 0;
  std::string anonVarName = "__anon_loop_var_" + std::to_string(AnonCounter++);
  
  // Determine the type from the init expression (default to int)
  std::string varType = "int";
  
  return std::make_unique<ForLoopStmtAST>(varType, anonVarName, std::move(InitExpr), 
                                           std::move(LimitExpr), std::move(Body),
                                           std::move(StepExpr), StepOp,
                                           nullptr);
}

/// statement ::= returnstmt | variabledecl | foreachstmt | usestmt | ifstmt | whilestmt | expressionstmt
std::unique_ptr<StmtAST> ParseStatement() {
  switch (CurTok) {
  case tok_return:
    return ParseReturnStatement();
  case tok_for:
    return ParseForStatement();
  case tok_use:
    return ParseUseStatement();
  case tok_if:
    return ParseIfStatement();
  case tok_while:
    return ParseWhileStatement();
  case tok_break:
    return ParseBreakStatement();
  case tok_skip:
    return ParseSkipStatement();
  case tok_assert:
    return ParseAssertStatement();
  case tok_switch:
    return ParseSwitchStatement();
  case tok_unsafe:
    return ParseUnsafeBlock();
  case '{': {
    auto Block = ParseBlock();
    if (!Block)
      return nullptr;
    return Block;
  }
  case tok_autoreleasepool: {
    getNextToken(); // eat '@autoreleasepool'
    while (CurTok == tok_newline)
      getNextToken();
    if (CurTok == '{') {
      auto Body = ParseBlock();
      if (!Body)
        return nullptr;
    }
    return LogErrorS("'@autoreleasepool' is reserved for upcoming ARC support",
                     "Autorelease pools will lower to runtime helpers in a later ARC phase.");
  }
  case tok_free: {
    getNextToken(); // eat 'free'
    while (CurTok == tok_newline)
      getNextToken();
    if (CurTok == tok_newline || CurTok == ';' || CurTok == '}' ||
        CurTok == tok_eof) {
      return LogErrorS("Expected expression after 'free'",
                       "Provide a reference expression to release.");
    }
    auto Expr = ParseExpression();
    if (!Expr)
      return nullptr;
    return std::make_unique<ExpressionStmtAST>(
        std::make_unique<FreeExprAST>(std::move(Expr)));
  }
  case tok_ref:
    // Handle ref variable declarations
    getNextToken(); // eat 'ref'
    if (!IsValidType() && CurTok != '(') {
      LogError("Expected type after 'ref'");
      return nullptr;
    }
    return ParseVariableDeclaration(true);
  case '}':
    // End of block - not a statement, but not an error either
    // Let the caller (ParseBlock) handle this
    return nullptr;
  default:
    // Check for valid types (built-in and struct) for variable declarations
    if (CurTok == '(') {
      if (auto Decl = ParseVariableDeclaration())
        return Decl;
    } else if (IsValidType()) {
      if (currentLexer().lastChar != '.')
        if (auto Decl = ParseVariableDeclaration())
          return Decl;
    }
    // Try to parse as an expression statement
    auto Expr = ParseExpression();
    if (!Expr)
      return nullptr;
    return std::make_unique<ExpressionStmtAST>(std::move(Expr));
  }
}

/// block ::= '{' statement* '}'
std::unique_ptr<BlockStmtAST> ParseBlock() {
  // Skip newlines before opening brace (for Allman style)
  while (CurTok == tok_newline)
    getNextToken();
    
  if (CurTok != '{')
    return nullptr;
    
  getNextToken(); // eat '{'
  
  std::vector<std::unique_ptr<StmtAST>> Statements;
  
  // Skip newlines after opening brace
  while (CurTok == tok_newline)
    getNextToken();
  
  while (CurTok != '}' && CurTok != tok_eof) {
    if (currentParser().hadError) {
      // Stop cascading errors inside this block; fast-forward to the end.
      while (CurTok != '}' && CurTok != tok_eof)
        getNextToken();
      break;
    }

    // Skip newlines between statements
    if (CurTok == tok_newline) {
      getNextToken();
      continue;
    }
    
    int StartTok = CurTok;
    auto Stmt = ParseStatement();
    if (!Stmt) {
      if (currentParser().hadError) {
        // After a reported error, skip the rest of the block to avoid cascades.
        while (CurTok != '}' && CurTok != tok_eof)
          getNextToken();
        break;
      }
      // Check if hit closing brace
      if (CurTok == '}')
        break;  // Normal end of block
      if (CurTok == tok_eof)
        return nullptr;
      if (CurTok == StartTok)
        getNextToken();  // Ensure progress on error to avoid infinite loops
      return nullptr;  // Actual error
    }
      
    Statements.push_back(std::move(Stmt));
    
    // Skip optional semicolon after statement
    if (CurTok == ';')
      getNextToken();
    
    // Skip newlines after statements
    while (CurTok == tok_newline)
      getNextToken();
  }
  
  if (CurTok != '}')
    return nullptr;
    
  getNextToken(); // eat '}'
  
  return std::make_unique<BlockStmtAST>(std::move(Statements));
}

/// ParseTypeIdentifier - Handles top-level declarations that start with a type
/// Could be either a variable declaration or function definition
bool ParseTypeIdentifier(bool isRef) {
  // Parse the complete type (including arrays and pointers)
  TypeInfo typeInfo;
  if (!ParseCompleteTypeInfo(typeInfo, isRef)) {
    return false;
  }

  SkipNewlines();

  std::string Type = typeNameFromInfo(typeInfo);

  // Handle constructor call (struct name followed directly by '(')
  if (CurTok == '(' && (StructNames.contains(Type) || ClassNames.contains(Type))) {
    std::vector<std::unique_ptr<ExprAST>> Args;
    std::vector<std::string> ArgNames;
    std::vector<SourceLocation> ArgNameLocs;
    std::vector<SourceLocation> ArgEqualLocs;
    if (!ParseArgumentList(Args, ArgNames, ArgNameLocs, ArgEqualLocs))
      return false;

    auto Call = std::make_unique<CallExprAST>(Type, std::move(Args),
                                              std::move(ArgNames),
                                              std::move(ArgNameLocs),
                                              std::move(ArgEqualLocs));
    if (auto CallIR = Call->codegen()) {
      fprintf(stderr, "Generated struct instantiation IR:\n");
      CallIR->print(llvm::errs());
      fprintf(stderr, "\n");
    } else {
      reportCompilerError("Failed to generate struct instantiation");
      return false;
    }
    return true;
  }

  if (CurTok != tok_identifier) {
    reportCompilerError("Expected identifier after type");
    return false;
  }

  std::string Name = IdentifierStr;
  getNextToken(); // eat identifier

  SkipNewlines();

  std::vector<std::string> genericParams;
  if (ParseGenericParameterList(genericParams))
    SkipNewlines();

  // Check what follows
  if (CurTok == '(') {
    // It's a function definition
    // Need to parse the full prototype and then the body
    GenericParameterScope parameterScope(genericParams);
    getNextToken(); // eat '('
    SkipNewlines();
    
    std::vector<Parameter> Args;
    
    // Parse parameters
    if (CurTok != ')') {
      while (true) {
        SkipNewlines();
        bool paramIsParams = false;
        SourceLocation paramsLoc{};
        if (CurTok == tok_params) {
          paramIsParams = true;
          paramsLoc = currentParser().currentTokenLocation;
          getNextToken(); // eat 'params'
          SkipNewlines();
        }

        // Check for ref parameter
        bool paramIsRef = false;
        if (CurTok == tok_ref) {
          paramIsRef = true;
          getNextToken(); // eat 'ref'
          SkipNewlines();
        }

        if (!IsValidType() && CurTok != '(') {
          reportCompilerError("Expected parameter type");
          return false;
        }

        TypeInfo ParamTypeInfo;
        if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
          reportCompilerError("Failed to parse parameter type");
          return false;
        }

        if (CurTok != tok_identifier) {
          reportCompilerError("Expected parameter name");
          return false;
        }

        std::string ParamName = IdentifierStr;
        SourceLocation nameLoc = currentParser().currentTokenLocation;
        getNextToken();

        Parameter param;
        param.Type = typeNameFromInfo(ParamTypeInfo);
        param.Name = ParamName;
        param.IsRef = paramIsRef;
        param.IsParams = paramIsParams;
        param.DeclaredType = std::move(ParamTypeInfo);
        param.NameLocation = nameLoc;
        param.ParamsLocation = paramsLoc;
        SkipNewlines();
        if (CurTok == '=') {
          param.HasDefault = true;
          param.DefaultEqualsLocation = currentParser().currentTokenLocation;
          getNextToken(); // eat '='
          SkipNewlines();
          param.DefaultValue = ParseExpression();
          if (!param.DefaultValue)
            return false;
        }
        Args.push_back(std::move(param));

        SkipNewlines();

        if (CurTok == ')') {
          getNextToken(); // eat ')'
          break;
        }

        if (CurTok != ',') {
          reportCompilerError("Expected ',' or ')' in parameter list");
          return false;
        }

        getNextToken(); // eat ','
        SkipNewlines();
      }
    } else {
      getNextToken(); // eat ')'
    }
    
    if (!ValidateParameterDefaults(Args))
      return false;

    // Create prototype
    auto Proto = std::make_unique<PrototypeAST>(std::move(typeInfo), Name,
                                                std::move(Args), false, isRef,
                                                std::move(genericParams));
    
    // Parse the body
    auto Body = ParseBlock();
    if (!Body) {
      if (!currentParser().hadError)
        reportCompilerError("Expected function body");
      return false;
    }

    if (currentParser().hadError)
      return false;
    
    auto Fn = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    if (Fn->getProto()->getGenericParameters().empty()) {
      fprintf(stderr, "Parsed function successfully, generating code...\n");
      if (auto FnIR = Fn->codegen()) {
        fprintf(stderr, "Generated function IR:\n");
        FnIR->print(llvm::errs());
        fprintf(stderr, "\n");
      } else {
        reportCompilerError("Failed to generate IR for function");
      }
    } else {
      RegisterGenericFunctionTemplate(std::move(Fn));
    }
    return true;
  } else if (CurTok == '=') {
    // It's a variable declaration
    getNextToken(); // eat '='
    SkipNewlines();

    TypeInfo declInfo = typeInfo;

    // Check if the initializer has 'ref' keyword (for linking two variables)
    bool initializerIsRef = false;
    if (CurTok == tok_ref) {
      initializerIsRef = true;
      getNextToken(); // eat 'ref'
      SkipNewlines();
    }

    auto Initializer = ParseExpression();
    if (!Initializer) {
      reportCompilerError("Expected expression after '='");
      return false;
    }

    if (!initializerIsRef)
      ConvertParenInitializerToConstructor(Initializer, declInfo);

    // If initializer has ref, wrap it in RefExprAST
    if (initializerIsRef) {
      Initializer = std::make_unique<RefExprAST>(std::move(Initializer));
    }

    // Create the variable declaration AST but don't generate code here
    // Code generation will be handled by HandleVariableDeclaration in toplevel.cpp
    auto VarDecl = std::make_unique<VariableDeclarationStmtAST>(std::move(declInfo), Name, std::move(Initializer), isRef);

    if (IsInteractiveMode())
      fprintf(stderr, "Parsed variable declaration, generating code...\n");

    if (auto VarIR = VarDecl->codegen()) {
      (void)VarIR;
      NoteTopLevelStatementEmitted();
      if (IsInteractiveMode()) {
        fprintf(stderr, "Generated variable declaration IR:\n");
        if (auto *TopFunc = getModule()->getFunction("__hybrid_top_level"))
          TopFunc->print(llvm::errs());
        fprintf(stderr, "\n");
      }
    } else {
      reportCompilerError("Failed to generate IR for variable declaration");
      return false;
    }
    return true;
  } else {
    reportCompilerError("Expected '(' or '=' after identifier in top-level declaration");
    return false;
  }
}

/// structdef ::= ('struct' | 'class' | 'interface') identifier ('inherits' type (',' type)*)? '{' (member)* '}'
std::unique_ptr<StructAST> ParseStructDefinition(AggregateKind kind, bool isAbstract) {
  getNextToken(); // eat 'struct', 'class', or 'interface'

  while (CurTok == tok_newline)
    getNextToken();

  if (CurTok != tok_identifier) {
    std::string kindDescription;
    switch (kind) {
    case AggregateKind::Struct:
      kindDescription = "struct";
      break;
    case AggregateKind::Class:
      kindDescription = "class";
      break;
    case AggregateKind::Interface:
      kindDescription = "interface";
      break;
    }
    LogError("Expected " + kindDescription + " name after '" + kindDescription + "'");
    return nullptr;
  }

  std::string compositeName = IdentifierStr;
  getNextToken();

  if (StructNames.contains(compositeName) || ClassNames.contains(compositeName)) {
    reportCompilerError("Type '" + compositeName + "' is already defined");
    return nullptr;
  }

  SkipNewlines();

  std::vector<std::string> genericParameters;
  if (ParseGenericParameterList(genericParameters))
    SkipNewlines();
  if (!genericParameters.empty()) {
    std::string kindLabel = "Struct";
    if (kind == AggregateKind::Class)
      kindLabel = "Class";
    else if (kind == AggregateKind::Interface)
      kindLabel = "Interface";
    maybeWarnGenericArity(genericParameters, compositeName, kindLabel);
  }

  GenericParameterScope compositeGenerics(genericParameters);

  std::optional<std::string> baseClass;
  std::vector<std::string> interfaceTypes;
  std::vector<std::string> baseTypes;
  std::optional<TypeInfo> baseClassInfo;
  std::vector<TypeInfo> interfaceTypeInfos;
  std::vector<TypeInfo> baseTypeInfos;

  if ((kind == AggregateKind::Class || kind == AggregateKind::Interface) &&
      CurTok == tok_inherits) {
    getNextToken(); // eat 'inherits'
    SkipNewlines();

    bool expectBaseClass = (kind == AggregateKind::Class);
    bool sawType = false;
    while (true) {
      if (!IsValidType() && CurTok != '(') {
        LogError("Expected type name after 'inherits'");
        return nullptr;
      }

      TypeInfo clauseInfo;
      if (!ParseCompleteTypeInfo(clauseInfo, false))
        return nullptr;
      sawType = true;
      SkipNewlines();

      std::string typeName = typeNameFromInfo(clauseInfo);
      baseTypeInfos.push_back(clauseInfo);

      if (expectBaseClass) {
        baseClass = typeName;
        baseClassInfo = clauseInfo;
        expectBaseClass = false;
      } else {
        interfaceTypes.push_back(typeName);
        interfaceTypeInfos.push_back(clauseInfo);
      }

      if (CurTok != ',')
        break;

      getNextToken();
      SkipNewlines();
    }

    if (!sawType) {
      LogError("Expected at least one type after 'inherits'");
      return nullptr;
    }
  } else if ((kind == AggregateKind::Class || kind == AggregateKind::Interface) &&
             CurTok == tok_colon) {
    reportCompilerError(
        "Use 'inherits' to declare base types",
        "Replace ':' with 'inherits' before listing base interfaces or classes.");
    return nullptr;
  }

  if (baseClass)
    baseTypes.push_back(*baseClass);
  baseTypes.insert(baseTypes.end(), interfaceTypes.begin(), interfaceTypes.end());

  bool isAbstractComposite = isAbstract || kind == AggregateKind::Interface;

  if (CurTok != '{') {
    LogError("Expected '{' after type name");
    return nullptr;
  }

  auto &definitionStack = (kind == AggregateKind::Struct) ? StructDefinitionStack
                                                          : ClassDefinitionStack;

  struct ScopedCompositeMarker {
    std::vector<std::string> &stack;
    ScopedCompositeMarker(std::vector<std::string> &s, const std::string &name)
        : stack(s) {
      stack.push_back(name);
    }
    ~ScopedCompositeMarker() {
      if (!stack.empty())
        stack.pop_back();
    }
  } scope(definitionStack, compositeName);

  getNextToken(); // eat '{'

  while (CurTok == tok_newline)
    getNextToken();

  std::vector<std::unique_ptr<FieldAST>> Fields;
  std::vector<std::unique_ptr<PropertyAST>> Properties;
  std::vector<MethodDefinition> Methods;
  std::vector<std::unique_ptr<DelegateDeclAST>> Delegates;
  bool seenThisOverride = false;
  bool hasConstructor = false;
  bool hasDestructor = false;
  int destructorIndex = -1;
  bool hasIndexer = false;

  auto parseConstructor = [&](MemberModifiers modifiers,
                              std::vector<std::string> ctorGenericParams) -> bool {
    GenericParameterScope ctorScope(ctorGenericParams);
    getNextToken(); // eat '('
    SkipNewlines();

    std::vector<Parameter> Args;
    while (CurTok != ')' && CurTok != tok_eof) {
      SkipNewlines();
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, false)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = false;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return false;
      }
      Args.push_back(std::move(param));

      SkipNewlines();

      if (CurTok == ')')
        break;

      if (CurTok != ',') {
        LogError("Expected ')' or ',' in parameter list");
        return false;
      }
      getNextToken();
      SkipNewlines();
    }

    if (CurTok != ')') {
      LogError("Expected ')' after parameters");
      return false;
    }
    getNextToken();

    if (!ValidateParameterDefaults(Args))
      return false;

    std::vector<ConstructorInitializer> ctorInitializers;

    if (CurTok == tok_colon) {
      LogError("Expected '{' after constructor declaration");
      int braceDepth = 0;
      do {
        getNextToken();
        if (CurTok == '{')
          ++braceDepth;
        else if (CurTok == '}') {
          if (braceDepth == 0)
            break;
          --braceDepth;
        }
      } while (CurTok != tok_eof);
      return false;
    }

    while (CurTok == tok_newline)
      getNextToken();

    if (CurTok != '{') {
      LogError("Expected '{' after constructor declaration");
      return false;
    }

    auto Body = ParseBlock();
    if (!Body)
      return false;

    TypeInfo ctorReturn = buildDeclaredTypeInfo(compositeName, false);
    auto Proto = std::make_unique<PrototypeAST>(
        std::move(ctorReturn), compositeName, std::move(Args),
        false, false, std::move(ctorGenericParams));
    auto Constructor = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
    Methods.emplace_back(std::move(Constructor), modifiers, MethodKind::Constructor, compositeName);
    Methods.back().setConstructorInitializers(std::move(ctorInitializers));
    hasConstructor = true;
    return true;
  };

  auto parseCompositeMethod = [&](TypeInfo ReturnTypeInfo,
                                  const std::string &MethodName,
                                  MemberModifiers modifiers,
                                  MethodKind methodKind,
                                  std::vector<std::string> methodGenericParams) -> bool {
    GenericParameterScope methodScope(methodGenericParams);
    getNextToken(); // eat '('

    std::vector<Parameter> Args;
    while (CurTok != ')' && CurTok != tok_eof) {
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken();
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        param.HasDefault = true;
        param.DefaultEqualsLocation = currentParser().currentTokenLocation;
        getNextToken(); // eat '='
        SkipNewlines();
        param.DefaultValue = ParseExpression();
        if (!param.DefaultValue)
          return false;
      }
      Args.push_back(std::move(param));

      if (CurTok == ')')
        break;

      if (CurTok != ',') {
        LogError("Expected ')' or ',' in parameter list");
        return false;
      }
      getNextToken();
    }

    if (CurTok != ')') {
      LogError("Expected ')' after parameters");
      return false;
    }
    getNextToken();

    if (!ValidateParameterDefaults(Args))
      return false;

    while (CurTok == tok_newline)
      getNextToken();

    if (kind == AggregateKind::Interface && !modifiers.isAbstract)
      modifiers.isAbstract = true;

    const bool requiresBody = !modifiers.isAbstract && kind != AggregateKind::Interface;
    std::string QualifiedName = compositeName + "." + MethodName;
    auto Proto = std::make_unique<PrototypeAST>(std::move(ReturnTypeInfo), QualifiedName,
                                                std::move(Args), false, false,
                                                std::move(methodGenericParams));

    if (requiresBody) {
      if (CurTok != '{') {
        LogError("Expected '{' after method declaration");
        return false;
      }

      auto Body = ParseBlock();
      if (!Body)
        return false;

      auto Method = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
      Methods.emplace_back(std::move(Method), modifiers, methodKind, MethodName);
    } else {
      if (CurTok == '{') {
        reportCompilerError("Abstract methods cannot declare a body");
        return false;
      }
      Methods.emplace_back(std::move(Proto), modifiers, methodKind, MethodName);
    }

    return true;
  };

  auto memberNameCollides = [&](const std::string &name) -> bool {
    for (const auto &field : Fields) {
      if (field && field->getName() == name)
        return true;
    }
    for (const auto &prop : Properties) {
      if (prop && prop->getName() == name)
        return true;
    }
    for (const auto &method : Methods) {
      if (method.getDisplayName() == name)
        return true;
    }
    return false;
  };

  auto parseIndexerParameters = [&](std::vector<Parameter> &params) -> bool {
    if (CurTok != '[') {
      reportCompilerError("Expected '[' after 'this' in indexer declaration");
      return false;
    }
    getNextToken(); // eat '['
    SkipNewlines();
    while (CurTok != ']' && CurTok != tok_eof) {
      bool paramIsParams = false;
      SourceLocation paramsLoc{};
      if (CurTok == tok_params) {
        paramIsParams = true;
        paramsLoc = currentParser().currentTokenLocation;
        getNextToken(); // eat 'params'
        SkipNewlines();
      }

      bool paramIsRef = false;
      if (CurTok == tok_ref) {
        paramIsRef = true;
        getNextToken(); // eat 'ref'
        SkipNewlines();
      }

      if (!IsValidType() && CurTok != '(') {
        LogError("Expected parameter type");
        return false;
      }

      TypeInfo ParamTypeInfo;
      if (!ParseCompleteTypeInfo(ParamTypeInfo, paramIsRef)) {
        LogError("Failed to parse parameter type");
        return false;
      }

      if (CurTok != tok_identifier) {
        LogError("Expected parameter name");
        return false;
      }

      std::string ParamName = IdentifierStr;
      SourceLocation nameLoc = currentParser().currentTokenLocation;
      getNextToken();

      if (ParamName == "value") {
        reportCompilerError("Indexer parameter name 'value' is reserved");
        return false;
      }

      if (paramIsParams) {
        reportCompilerError("Indexers cannot declare 'params' parameters");
        return false;
      }

      Parameter param;
      param.Type = typeNameFromInfo(ParamTypeInfo);
      param.Name = ParamName;
      param.IsRef = paramIsRef;
      param.IsParams = paramIsParams;
      param.DeclaredType = std::move(ParamTypeInfo);
      param.NameLocation = nameLoc;
      param.ParamsLocation = paramsLoc;
      SkipNewlines();
      if (CurTok == '=') {
        reportCompilerError("Indexer parameters cannot declare default values");
        return false;
      }
      params.push_back(std::move(param));

      SkipNewlines();
      if (CurTok == ']')
        break;
      if (CurTok != ',') {
        LogError("Expected ']' or ',' in indexer parameter list");
        return false;
      }
      getNextToken(); // eat ','
      SkipNewlines();
    }

    if (CurTok != ']') {
      LogError("Expected ']' after indexer parameters");
      return false;
    }
    getNextToken(); // eat ']'

    if (!ValidateParameterDefaults(params))
      return false;

    if (params.empty()) {
      reportCompilerError("Indexers must declare at least one parameter");
      return false;
    }

    return true;
  };

  auto parseAccessorBlock =
      [&](std::unique_ptr<AccessorAST> &getter,
          std::unique_ptr<AccessorAST> &setter) -> bool {
    if (CurTok != '{') {
      reportCompilerError("Expected '{' to begin accessor block");
      return false;
    }
    getNextToken(); // eat '{'
    SkipNewlines();

    while (CurTok != '}' && CurTok != tok_eof) {
      if (CurTok == tok_newline) {
        getNextToken();
        continue;
      }

      bool isGetter = false;
      if (CurTok == tok_get) {
        isGetter = true;
      } else if (CurTok == tok_set) {
        isGetter = false;
      } else {
        reportCompilerError("Expected 'get' or 'set' accessor");
        return false;
      }

      SourceLocation keywordLoc = currentParser().currentTokenLocation;
      getNextToken(); // eat accessor keyword
      SkipNewlines();

      std::unique_ptr<ExprAST> exprBody;
      std::unique_ptr<BlockStmtAST> blockBody;
      bool isImplicit = false;

      if (CurTok == '{') {
        ScopedValueKeyword valueScope(!isGetter);
        blockBody = ParseBlock();
        if (!blockBody)
          return false;
      } else if (CurTok == tok_get || CurTok == tok_set || CurTok == '}' ||
                 CurTok == tok_newline) {
        isImplicit = true;
      } else {
        ScopedValueKeyword valueScope(!isGetter);
        exprBody = ParseExpression();
        if (!exprBody)
          return false;
      }

      if (isGetter) {
        if (getter) {
          reportCompilerError("Duplicate 'get' accessor");
          return false;
        }
        getter = std::make_unique<AccessorAST>(
            AccessorKind::Get, keywordLoc, std::move(exprBody),
            std::move(blockBody), isImplicit);
      } else {
        if (setter) {
          reportCompilerError("Duplicate 'set' accessor");
          return false;
        }
        setter = std::make_unique<AccessorAST>(
            AccessorKind::Set, keywordLoc, std::move(exprBody),
            std::move(blockBody), isImplicit);
      }

      SkipNewlines();
    }

    if (CurTok != '}') {
      LogError("Expected '}' after accessor block");
      return false;
    }
    getNextToken(); // eat '}'
    return true;
  };

  auto applyAccessorAccessDefaults =
      [&](MemberModifiers &modifiers,
          const PendingMemberModifiers &pending,
          const std::string &memberName,
          bool hasGetter,
          bool hasSetter,
          bool isIndexer) -> bool {
    if (!isIndexer && pending.hasExplicitAccess &&
        (pending.access == AccessSpecifier::Private ||
         pending.access == AccessSpecifier::Protected)) {
      const char *accessName =
          pending.access == AccessSpecifier::Private ? "private" : "protected";
      std::string effect;
      if (hasGetter && hasSetter)
        effect = "getting and setting";
      else if (hasGetter)
        effect = "getting";
      else if (hasSetter)
        effect = "setting";
      else
        effect = "accessing";
      reportCompilerError("Property '" + memberName + "' cannot be declared " +
                              accessName + " because it prevents " + effect,
                          std::string("Remove the ") + accessName +
                              " modifier or the accessor block.");
      return false;
    }

    if (!pending.hasExplicitAccess && hasSetter)
      modifiers.access = MemberAccess::PublicReadWrite();

    return true;
  };

  auto validateDelegateModifiers =
      [&](const PendingMemberModifiers &pending) -> bool {
        if (pending.hasExplicitAccess || pending.isStatic || pending.isConst ||
            pending.isAbstract || pending.isVirtual || pending.isOverride) {
          reportCompilerError(
              "Delegate declarations cannot use member modifiers");
          return false;
        }
        return true;
      };

  while (CurTok != '}' && CurTok != tok_eof) {
    SkipNewlines();
    if (CurTok == '}')
      break;

    PendingMemberModifiers pendingMods;
    if (!ParsePendingMemberModifiers(pendingMods))
      return nullptr;

    if (CurTok == tok_delegate) {
      if (!validateDelegateModifiers(pendingMods))
        return nullptr;
      auto delegateDecl = ParseDelegateDefinition();
      if (!delegateDecl)
        return nullptr;
      if (delegateDecl->getName() == compositeName) {
        reportCompilerError("Delegate '" + delegateDecl->getName() +
                            "' conflicts with enclosing type '" +
                            compositeName + "'");
        return nullptr;
      }
      Delegates.push_back(std::move(delegateDecl));
      continue;
    }

    if (CurTok == tok_tilde_identifier) {
      if (kind == AggregateKind::Interface) {
        reportCompilerError("Interfaces cannot declare destructors");
        return nullptr;
      }
      if (hasDestructor) {
        reportCompilerError("Type '" + compositeName + "' already declares a destructor");
        return nullptr;
      }

      std::string dtorTarget = IdentifierStr;
      getNextToken(); // eat '~Name'
      SkipNewlines();

      if (dtorTarget != compositeName) {
        reportCompilerError("Destructor name must match its enclosing type",
                            "Use '~" + compositeName + "()' to declare a destructor for '" +
                                compositeName + "'.");
        return nullptr;
      }

      std::string decoratedName = dtorTarget;
      if (!ParseOptionalGenericArgumentList(decoratedName, true))
        return nullptr;
      if (decoratedName != dtorTarget) {
        reportCompilerError("Destructors cannot declare generic parameters");
        return nullptr;
      }

      if (CurTok != '(') {
        LogError("Expected '(' after destructor name");
        return nullptr;
      }
      getNextToken(); // eat '('
      SkipNewlines();
      if (CurTok != ')') {
        reportCompilerError("Destructors cannot declare parameters");
        return nullptr;
      }
      getNextToken(); // eat ')'
      SkipNewlines();

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("Destructors cannot be static");
        return nullptr;
      }
      if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Destructors cannot be const-qualified");
        return nullptr;
      }
      if (modifiers.isAbstract) {
        reportCompilerError("Destructors cannot be abstract");
        return nullptr;
      }
      if (modifiers.isVirtual || modifiers.isOverride) {
        reportCompilerError("Destructors do not support virtual or override modifiers");
        return nullptr;
      }

      if (CurTok != '{') {
        LogError("Expected '{' after destructor declaration");
        return nullptr;
      }

      auto Body = ParseBlock();
      if (!Body)
        return nullptr;

      TypeInfo dtorReturn = buildDeclaredTypeInfo("void", false);
      std::string functionName = compositeName + ".~" + compositeName;
      auto Proto = std::make_unique<PrototypeAST>(
          std::move(dtorReturn), functionName, std::vector<Parameter>{},
          false, false);
      auto Dtor = std::make_unique<FunctionAST>(std::move(Proto), std::move(Body));
      Methods.emplace_back(std::move(Dtor), modifiers, MethodKind::Destructor,
                           "~" + compositeName);
      destructorIndex = static_cast<int>(Methods.size()) - 1;
      hasDestructor = true;
      continue;
    }

    TypeInfo memberTypeInfo;
    std::string Type;
    bool hasMemberTypeInfo = false;

    if (CurTok == tok_identifier && IdentifierStr == compositeName) {
      getNextToken();
      SkipNewlines();

      std::vector<std::string> ctorGenericParams;
      if (ParseGenericParameterList(ctorGenericParams))
        SkipNewlines();

      if (!ctorGenericParams.empty() && CurTok != '(') {
        reportCompilerError("Generic parameter list must be followed by '(' in constructor declarations");
        return nullptr;
      }

      if (CurTok == '(') {
        if (kind == AggregateKind::Interface) {
          reportCompilerError("Interfaces cannot declare constructors");
          return nullptr;
        }
        MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
        if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
          reportCompilerError("Constructors cannot be static");
          return nullptr;
        }
        if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
          reportCompilerError("Constructors cannot be const");
          return nullptr;
        }

        if (!parseConstructor(modifiers, std::move(ctorGenericParams)))
          return nullptr;
        continue;
      }

      Type = compositeName;
      bool pointerSeen = false;
      if (!AppendTypeSuffix(Type, pointerSeen))
        return nullptr;
      memberTypeInfo = buildDeclaredTypeInfo(Type, false);
      hasMemberTypeInfo = true;
    } else if (IsValidType() || CurTok == '(') {
      if (!ParseCompleteTypeInfo(memberTypeInfo, false)) {
        LogError("Failed to parse member type");
        return nullptr;
      }
      Type = typeNameFromInfo(memberTypeInfo);
      hasMemberTypeInfo = true;
    } else {
      LogError("Expected field or method declaration inside type definition");
      return nullptr;
    }

    std::string MemberName;
    SourceLocation memberNameLoc = currentParser().currentTokenLocation;
    if (CurTok == tok_identifier) {
      MemberName = IdentifierStr;
      getNextToken();
    } else if (CurTok == tok_this) {
      MemberName = "this";
      getNextToken();
    } else {
      LogError("Expected identifier after type");
      return nullptr;
    }

    while (CurTok == tok_newline)
      getNextToken();

    std::vector<std::string> methodGenericParams;
    if (ParseGenericParameterList(methodGenericParams))
      SkipNewlines();
    if (!methodGenericParams.empty())
      maybeWarnGenericArity(methodGenericParams, MemberName, "Method");

    if (MemberName == "this" && CurTok == '[') {
      if (!methodGenericParams.empty()) {
        reportCompilerError("Indexers cannot declare generic parameter lists");
        return nullptr;
      }
      if (memberNameCollides(MemberName)) {
        reportCompilerError("Type '" + compositeName +
                            "' already declares member '" + MemberName + "'");
        return nullptr;
      }
      if (hasIndexer) {
        reportCompilerError("Type '" + compositeName +
                            "' already declares an indexer");
        return nullptr;
      }

      std::vector<Parameter> indexerParams;
      if (!parseIndexerParameters(indexerParams))
        return nullptr;

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      modifiers.isProperty = true;

      if ((modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("Indexers must be instance members");
        return nullptr;
      }

      if (kind == AggregateKind::Interface && !modifiers.isAbstract)
        modifiers.isAbstract = true;

      SkipNewlines();
      std::unique_ptr<AccessorAST> getter;
      std::unique_ptr<AccessorAST> setter;
      if (!parseAccessorBlock(getter, setter))
        return nullptr;

      if (!getter && !setter) {
        reportCompilerError("Indexers must declare at least one accessor");
        return nullptr;
      }

      if (setter && !getter) {
        reportCompilerError("Indexer on type '" + compositeName +
                            "' does not define a getter");
        return nullptr;
      }

      if (!applyAccessorAccessDefaults(modifiers, pendingMods, MemberName,
                                       getter != nullptr, setter != nullptr,
                                       true))
        return nullptr;

      auto hasAccessorBody = [](const std::unique_ptr<AccessorAST> &accessor) {
        return accessor &&
               (accessor->hasBlockBody() || accessor->hasExpressionBody());
      };

      if ((kind == AggregateKind::Interface || modifiers.isAbstract) &&
          (hasAccessorBody(getter) || hasAccessorBody(setter))) {
        reportCompilerError("Abstract indexers cannot declare accessor bodies");
        return nullptr;
      }

      if (!modifiers.isAbstract &&
          ((getter && getter->isImplicit()) ||
           (setter && setter->isImplicit()))) {
        reportCompilerError("Indexer accessors must declare a body");
        return nullptr;
      }

      if (setter &&
          (modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Const indexers cannot declare a setter");
        return nullptr;
      }

      TypeInfo indexerInfo =
          hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
      auto Indexer = std::make_unique<PropertyAST>(
          typeNameFromInfo(indexerInfo), std::move(indexerInfo), MemberName,
          modifiers, nullptr, std::move(indexerParams), memberNameLoc,
          std::move(getter), std::move(setter));
      Properties.push_back(std::move(Indexer));
      hasIndexer = true;
      continue;
    }

    if (!methodGenericParams.empty() && CurTok != '(') {
      reportCompilerError("Generic parameter list must be followed by '(' in method declarations");
      return nullptr;
    }

    if (CurTok == '(') {
      MethodKind methodKind = MethodKind::Regular;
      if (MemberName == "this") {
        if (kind != AggregateKind::Class) {
          reportCompilerError("Only classes may define a this() formatter");
          return nullptr;
        }
        if (seenThisOverride) {
          reportCompilerError("Duplicate this() override",
                              "Only one this() method may be defined per class");
          return nullptr;
        }
        seenThisOverride = true;
        methodKind = MethodKind::ThisOverride;
      }

      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if ((modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
        reportCompilerError("Methods cannot be declared const yet",
                            "Use readonly state or immutable patterns instead");
        return nullptr;
      }

      if (methodKind == MethodKind::ThisOverride &&
          (modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
        reportCompilerError("this() override cannot be static");
        return nullptr;
      }

      if (kind == AggregateKind::Class && methodKind == MethodKind::Regular &&
          !isPascalCase(MemberName)) {
        reportCompilerWarning("Class methods should use PascalCase names",
                              "Rename '" + MemberName + "' to PascalCase for consistency");
      }

      if (!parseCompositeMethod(memberTypeInfo, MemberName, modifiers, methodKind,
                                std::move(methodGenericParams)))
        return nullptr;
    } else {
      if (!methodGenericParams.empty()) {
        reportCompilerError("Only methods may declare generic parameter lists");
        return nullptr;
      }
      if (MemberName == "this") {
        reportCompilerError("'this' can only be used as a formatter method name or indexer");
        return nullptr;
      }
      std::unique_ptr<ExprAST> MemberInitializer;
      if (CurTok == '=') {
        getNextToken(); // eat '='
        SkipNewlines();
        MemberInitializer = ParseExpression();
        if (!MemberInitializer)
          return nullptr;
      }
      SkipNewlines();

      if (CurTok == '{') {
        if (memberNameCollides(MemberName)) {
          reportCompilerError("Type '" + compositeName +
                              "' already declares member '" + MemberName + "'");
          return nullptr;
        }

        std::unique_ptr<AccessorAST> getter;
        std::unique_ptr<AccessorAST> setter;
        if (!parseAccessorBlock(getter, setter))
          return nullptr;

        if (!getter && !setter) {
          reportCompilerError("Properties must declare at least one accessor");
          return nullptr;
        }

        MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
        modifiers.isProperty = true;

        if (!applyAccessorAccessDefaults(modifiers, pendingMods, MemberName,
                                         getter != nullptr, setter != nullptr,
                                         false))
          return nullptr;

        if (kind == AggregateKind::Interface && !modifiers.isAbstract)
          modifiers.isAbstract = true;

        if (kind == AggregateKind::Interface &&
            (modifiers.storage & StorageFlag::Static) != StorageFlag::None) {
          reportCompilerError("Interfaces cannot declare static properties");
          return nullptr;
        }

        auto hasAccessorBody = [](const std::unique_ptr<AccessorAST> &accessor) {
          return accessor &&
                 (accessor->hasBlockBody() || accessor->hasExpressionBody());
        };

        if ((kind == AggregateKind::Interface || modifiers.isAbstract) &&
            (hasAccessorBody(getter) || hasAccessorBody(setter))) {
          reportCompilerError("Abstract properties cannot declare accessor bodies");
          return nullptr;
        }

        if (setter &&
            (modifiers.storage & StorageFlag::Const) != StorageFlag::None) {
          reportCompilerError("Const properties cannot declare a setter");
          return nullptr;
        }

        if (MemberInitializer &&
            (modifiers.storage & StorageFlag::Static) == StorageFlag::None) {
          reportCompilerError("Only static members may specify declaration initializers",
                              "Assign non-static members inside constructors instead");
          return nullptr;
        }

        if (kind == AggregateKind::Interface && MemberInitializer) {
          reportCompilerError("Interfaces cannot declare property initializers");
          return nullptr;
        }

        TypeInfo propInfo =
            hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
        std::string propTypeName = typeNameFromInfo(propInfo);
        if (kind != AggregateKind::Interface) {
          auto Field = std::make_unique<FieldAST>(propTypeName, propInfo, MemberName,
                                                  modifiers,
                                                  std::move(MemberInitializer));
          Fields.push_back(std::move(Field));
        } else {
          MemberInitializer.reset();
        }

        auto Property = std::make_unique<PropertyAST>(
            propTypeName, std::move(propInfo), MemberName, modifiers, nullptr,
            std::vector<Parameter>{}, memberNameLoc,
            std::move(getter), std::move(setter));
        Properties.push_back(std::move(Property));
        continue;
      }

      if (kind == AggregateKind::Interface) {
        reportCompilerError("Interfaces cannot declare fields");
        return nullptr;
      }
      MemberModifiers modifiers = FinalizeMemberModifiers(pendingMods, kind);
      if (MemberInitializer &&
          (modifiers.storage & StorageFlag::Static) == StorageFlag::None) {
        reportCompilerError("Only static members may specify declaration initializers",
                            "Assign non-static members inside constructors instead");
        return nullptr;
      }
      TypeInfo fieldInfo =
          hasMemberTypeInfo ? memberTypeInfo : buildDeclaredTypeInfo(Type, false);
      std::string fieldTypeName = typeNameFromInfo(fieldInfo);
      auto Field = std::make_unique<FieldAST>(fieldTypeName, std::move(fieldInfo),
                                              MemberName, modifiers,
                                              std::move(MemberInitializer));
      Fields.push_back(std::move(Field));
    }
  }

  if (kind != AggregateKind::Interface && !isAbstractComposite && !hasConstructor) {
    const char *kindDescription =
        (kind == AggregateKind::Struct) ? "Struct" : "Class";
    reportCompilerError(std::string(kindDescription) + " '" + compositeName +
                            "' must declare at least one constructor",
                        "Provide at least one constructor so that all members can be initialized explicitly.");
    return nullptr;
  }

  if (CurTok != '}') {
    LogError("Expected '}' at end of type definition");
    return nullptr;
  }

  getNextToken();

  if (kind == AggregateKind::Struct) {
    StructNames.insert(compositeName);
  } else {
    ClassNames.insert(compositeName);
    StructNames.insert(compositeName);
  }

  auto Result = std::make_unique<StructAST>(
      kind, compositeName, std::move(Delegates), std::move(Fields),
      std::move(Properties),
      std::move(Methods),
      std::move(baseTypes), std::move(genericParameters));
  Result->setBaseClass(std::move(baseClass));
  Result->setInterfaces(std::move(interfaceTypes));
  Result->setBaseTypeInfos(std::move(baseTypeInfos));
  Result->setBaseClassInfo(std::move(baseClassInfo));
  Result->setInterfaceTypeInfos(std::move(interfaceTypeInfos));
  Result->setAbstract(isAbstractComposite);
  ClassInheritanceMetadata inheritance;
  inheritance.baseClassName = Result->getBaseClass();
  inheritance.interfaceNames = Result->getInterfaces();
  if (kind == AggregateKind::Struct) {
    inheritance.defaultMemberAccess = MemberAccess::PublicReadWrite();
  } else {
    inheritance.defaultMemberAccess = MemberAccess::ReadPublicWritePrivate();
  }
  for (const auto &method : Result->getMethods()) {
    if (method.getKind() == MethodKind::Constructor)
      inheritance.constructorAccesses.push_back(method.getModifiers().access);
  }
  Result->setInheritanceMetadata(std::move(inheritance));
  if (destructorIndex >= 0)
    Result->setDestructorIndex(destructorIndex);
  return Result;
}
// Evaluate constant expressions at compile time
bool EvaluateConstantExpression(const ExprAST* expr, ConstantValue& result,
                                SourceLocation *firstErrorLocation) {
  if (!expr) return false;

  auto noteFailure = [&](const ExprAST *node) {
    if (!firstErrorLocation || !node)
      return;
    if (firstErrorLocation->isValid())
      return;
    SourceLocation loc = node->getSourceLocation();
    if (loc.isValid())
      *firstErrorLocation = loc;
  };

  auto toDouble = [](const ConstantValue &cv) -> double {
    switch (cv.type) {
      case ConstantValue::INTEGER:
        return static_cast<double>(cv.intVal);
      case ConstantValue::UNSIGNED_INTEGER:
        return static_cast<double>(cv.uintVal);
      case ConstantValue::FLOAT:
        return cv.floatVal;
      case ConstantValue::BOOLEAN:
        return cv.boolVal ? 1.0 : 0.0;
    }
    return 0.0;
  };

  if (auto numExpr = dynamic_cast<const NumberExprAST*>(expr)) {
    const NumericLiteral &literal = numExpr->getLiteral();
    if (literal.isInteger()) {
      if (literal.fitsInSignedBits(64)) {
        result = ConstantValue(static_cast<long long>(literal.getIntegerValue().getSExtValue()));
      } else if (literal.fitsInUnsignedBits(64)) {
        result = ConstantValue(static_cast<unsigned long long>(literal.getIntegerValue().getZExtValue()));
      } else {
        result = ConstantValue(literal.toDouble());
      }
    } else {
      result = ConstantValue(literal.toDouble());
    }
    return true;
  }

  if (auto boolExpr = dynamic_cast<const BoolExprAST*>(expr)) {
    result = ConstantValue(boolExpr->getValue());
    return true;
  }

  if (auto parenExpr = dynamic_cast<const ParenExprAST*>(expr)) {
    if (parenExpr->isTuple() || parenExpr->size() != 1) {
      noteFailure(parenExpr);
      return false;
    }
    return EvaluateConstantExpression(parenExpr->getElement(0), result,
                                      firstErrorLocation);
  }

  if (auto binExpr = dynamic_cast<const BinaryExprAST*>(expr)) {
    ConstantValue lhs(0LL), rhs(0LL);
    if (!EvaluateConstantExpression(binExpr->getLHS(), lhs, firstErrorLocation) ||
        !EvaluateConstantExpression(binExpr->getRHS(), rhs, firstErrorLocation)) {
      noteFailure(binExpr);
      return false;
    }

    const std::string &op = binExpr->getOp();

    auto isSignedInt = [](const ConstantValue &cv) { return cv.type == ConstantValue::INTEGER; };
    auto isUnsignedInt = [](const ConstantValue &cv) { return cv.type == ConstantValue::UNSIGNED_INTEGER; };
    if (op == "<" || op == ">" || op == "==" || op == "!=" ||
        op == "<=" || op == ">=") {
      double leftVal = toDouble(lhs);
      double rightVal = toDouble(rhs);

      bool compResult = false;
      if (op == "<") {
        compResult = leftVal < rightVal;
      } else if (op == ">") {
        compResult = leftVal > rightVal;
      } else if (op == "<=") {
        compResult = leftVal <= rightVal;
      } else if (op == ">=") {
        compResult = leftVal >= rightVal;
      } else if (op == "==") {
        compResult = leftVal == rightVal;
      } else if (op == "!=") {
        compResult = leftVal != rightVal;
      } else {
        noteFailure(binExpr);
        return false;
      }

      result = ConstantValue(compResult);
      return true;
    }

    if (op == "&&" || op == "||") {
      if (lhs.type != ConstantValue::BOOLEAN || rhs.type != ConstantValue::BOOLEAN) {
        noteFailure(binExpr);
        return false;
      }

      bool logicalResult = (op == "&&") ? (lhs.boolVal && rhs.boolVal)
                                         : (lhs.boolVal || rhs.boolVal);
      result = ConstantValue(logicalResult);
      return true;
    }

    if (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") {
      if (isSignedInt(lhs) && isSignedInt(rhs)) {
        long long arithResult = 0;
        if (op == "+") {
          arithResult = lhs.intVal + rhs.intVal;
        } else if (op == "-") {
          arithResult = lhs.intVal - rhs.intVal;
        } else if (op == "*") {
          arithResult = lhs.intVal * rhs.intVal;
        } else if (op == "/") {
          if (rhs.intVal == 0) {
            noteFailure(binExpr);
            return false;
          }
          arithResult = lhs.intVal / rhs.intVal;
        } else if (op == "%") {
          if (rhs.intVal == 0) {
            noteFailure(binExpr);
            return false;
          }
          arithResult = lhs.intVal % rhs.intVal;
        }
        result = ConstantValue(arithResult);
        return true;
      }

      if (isUnsignedInt(lhs) && isUnsignedInt(rhs)) {
        unsigned long long arithResult = 0;
        if (op == "+") {
          arithResult = lhs.uintVal + rhs.uintVal;
        } else if (op == "-") {
          arithResult = lhs.uintVal - rhs.uintVal;
        } else if (op == "*") {
          arithResult = lhs.uintVal * rhs.uintVal;
        } else if (op == "/") {
          if (rhs.uintVal == 0) {
            noteFailure(binExpr);
            return false;
          }
          arithResult = lhs.uintVal / rhs.uintVal;
        } else if (op == "%") {
          if (rhs.uintVal == 0) {
            noteFailure(binExpr);
            return false;
          }
          arithResult = lhs.uintVal % rhs.uintVal;
        }
        result = ConstantValue(arithResult);
        return true;
      }

      if (op == "%") {
        noteFailure(binExpr);
        return false;
      }

      double leftVal = toDouble(lhs);
      double rightVal = toDouble(rhs);
      if (op == "/" && rightVal == 0.0) {
        noteFailure(binExpr);
        return false;
      }

      double floatResult = 0.0;
      if (op == "+") {
        floatResult = leftVal + rightVal;
      } else if (op == "-") {
        floatResult = leftVal - rightVal;
      } else if (op == "*") {
        floatResult = leftVal * rightVal;
      } else if (op == "/") {
        floatResult = leftVal / rightVal;
      }

      result = ConstantValue(floatResult);
      return true;
    }

    if (op == "<<" || op == ">>") {
      if (lhs.type == ConstantValue::INTEGER && rhs.type == ConstantValue::INTEGER) {
        if (op == "<<")
          result = ConstantValue(lhs.intVal << rhs.intVal);
        else
          result = ConstantValue(lhs.intVal >> rhs.intVal);
        return true;
      }
      if (lhs.type == ConstantValue::UNSIGNED_INTEGER &&
          rhs.type == ConstantValue::UNSIGNED_INTEGER) {
        if (op == "<<")
          result = ConstantValue(lhs.uintVal << rhs.uintVal);
        else
          result = ConstantValue(lhs.uintVal >> rhs.uintVal);
        return true;
      }
      noteFailure(binExpr);
      return false;
    }

    if (op == "&" || op == "|" || op == "^") {
      if (lhs.type == ConstantValue::INTEGER && rhs.type == ConstantValue::INTEGER) {
        long long value = lhs.intVal;
        if (op == "&")
          value &= rhs.intVal;
        else if (op == "|")
          value |= rhs.intVal;
        else
          value ^= rhs.intVal;
        result = ConstantValue(value);
        return true;
      }
      if (lhs.type == ConstantValue::UNSIGNED_INTEGER &&
          rhs.type == ConstantValue::UNSIGNED_INTEGER) {
        unsigned long long value = lhs.uintVal;
        if (op == "&")
          value &= rhs.uintVal;
        else if (op == "|")
          value |= rhs.uintVal;
        else
          value ^= rhs.uintVal;
        result = ConstantValue(value);
        return true;
      }
      noteFailure(binExpr);
      return false;
    }

    noteFailure(binExpr);
    return false;
  }

  if (auto unaryExpr = dynamic_cast<const UnaryExprAST*>(expr)) {
    ConstantValue operand(0LL);
    if (!EvaluateConstantExpression(unaryExpr->getOperand(), operand,
                                    firstErrorLocation)) {
      noteFailure(unaryExpr);
      return false;
    }

    const std::string &op = unaryExpr->getOp();

    if (op == "-") {
      if (operand.type == ConstantValue::INTEGER) {
        result = ConstantValue(-operand.intVal);
        return true;
      }

      if (operand.type == ConstantValue::UNSIGNED_INTEGER) {
        if (operand.uintVal <= static_cast<unsigned long long>(std::numeric_limits<long long>::max())) {
          result = ConstantValue(-static_cast<long long>(operand.uintVal));
        } else {
          result = ConstantValue(-static_cast<double>(operand.uintVal));
        }
        return true;
      }

      if (operand.type == ConstantValue::FLOAT) {
        result = ConstantValue(-operand.floatVal);
        return true;
      }
    } else if (op == "!") {
      if (operand.type == ConstantValue::BOOLEAN) {
        result = ConstantValue(!operand.boolVal);
        return true;
      }
    }
  }

  noteFailure(expr);
  return false;
}

#undef CharVal
#undef StringVal
#undef LexedNumericLiteral
#undef IdentifierStr
#undef UnsafeContextLevel
#undef LoopNestingDepth
#undef StructNames
#undef ClassNames
#undef DelegateNames
#undef StructDefinitionStack
#undef ClassDefinitionStack
#undef BinopPrecedence
#undef CurTok
