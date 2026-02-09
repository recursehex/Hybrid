// This file implements the Hybrid language parser that builds AST nodes and manages parsing state.

#include "parser/parser_internal.h"

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
