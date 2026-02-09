#include "parser/parser_internal.h"

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

  return std::make_unique<VariableDeclarationStmtAST>(
      std::move(declInfo), Name, std::move(Initializer), isRef);
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

  return std::make_unique<ForEachStmtAST>(std::move(declInfo), VarName,
                                          std::move(Collection),
                                          std::move(Body));
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

  return std::make_unique<IfStmtAST>(std::move(Condition), std::move(ThenBranch),
                                     std::move(ElseBranch));
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

  return std::make_unique<AssertStmtAST>(std::move(Condition), assertLoc.line,
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

  return std::make_unique<SwitchStmtAST>(std::move(Condition),
                                         std::move(Cases));
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

    return std::make_unique<CaseAST>(std::move(Values), std::move(Expression),
                                     isDefault);
  } else {
    // Statement case: expect { statements }
    if (CurTok != '{') {
      LogError("Expected '{' after case in switch statement");
      return nullptr;
    }

    auto Body = ParseBlock();
    if (!Body)
      return nullptr;

    return std::make_unique<CaseAST>(std::move(Values), std::move(Body),
                                     isDefault);
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
                                              std::move(Collection),
                                              std::move(Body));
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
      char StepOp = '+'; // Default to addition
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
  char StepOp = '+'; // Default to addition
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
        break; // Normal end of block
      if (CurTok == tok_eof)
        return nullptr;
      if (CurTok == StartTok)
        getNextToken(); // Ensure progress on error to avoid infinite loops
      return nullptr;   // Actual error
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
