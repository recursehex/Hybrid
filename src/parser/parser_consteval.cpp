#include "parser/parser_internal.h"
#include <limits>

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
