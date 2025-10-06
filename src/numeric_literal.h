#ifndef NUMERIC_LITERAL_H
#define NUMERIC_LITERAL_H

#include <limits>
#include <string>

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/StringRef.h"

enum class NumericLiteralKind {
  Integer,
  Floating
};

struct NumericLiteral {
  NumericLiteralKind kind;
  llvm::APInt intValue;
  llvm::APFloat floatValue;
  unsigned base;
  bool hadDecimalPoint;
  bool hadExponent;
  bool isFloat32;
  std::string spelling;
  unsigned requiredBitWidth;

  NumericLiteral()
      : kind(NumericLiteralKind::Integer),
        intValue(llvm::APInt(64, 0)),
        floatValue(0.0),
        base(10),
        hadDecimalPoint(false),
        hadExponent(false),
        isFloat32(false),
        spelling("0"),
        requiredBitWidth(1) {}

  static NumericLiteral fromSigned(int64_t value) {
    llvm::APInt apint(64, static_cast<uint64_t>(value), true);
    unsigned bits = std::max(1u, apint.getActiveBits());
    return makeInteger(apint, 10, std::to_string(value), bits);
  }

  static NumericLiteral fromUnsigned(uint64_t value) {
    llvm::APInt apint(64, value, false);
    unsigned bits = std::max(1u, apint.getActiveBits());
    return makeInteger(apint, 10, std::to_string(value), bits);
  }

  static NumericLiteral makeInteger(const llvm::APInt &value,
                                    unsigned radix,
                                    std::string literalSpelling,
                                    unsigned requiredBits) {
    NumericLiteral literal;
    literal.kind = NumericLiteralKind::Integer;
    literal.intValue = value.zextOrTrunc(64);
    literal.floatValue = llvm::APFloat(0.0);
    literal.base = radix;
    literal.hadDecimalPoint = false;
    literal.hadExponent = false;
    literal.isFloat32 = false;
    literal.spelling = std::move(literalSpelling);
    literal.requiredBitWidth = std::max(1u, requiredBits);
    return literal;
  }

  static NumericLiteral makeFloating(const llvm::APFloat &value,
                                     std::string literalSpelling,
                                     bool sawDecimal,
                                     bool sawExponent) {
    NumericLiteral literal;
    literal.kind = NumericLiteralKind::Floating;
    literal.intValue = llvm::APInt(64, 0);
    literal.floatValue = value;
    literal.base = 10;
    literal.hadDecimalPoint = sawDecimal;
    literal.hadExponent = sawExponent;
    literal.isFloat32 = false;
    literal.spelling = std::move(literalSpelling);
    literal.requiredBitWidth = 0;
    return literal;
  }

  bool isInteger() const { return kind == NumericLiteralKind::Integer; }
  bool isFloating() const { return kind == NumericLiteralKind::Floating; }

  const llvm::APInt &getIntegerValue() const { return intValue; }
  const llvm::APFloat &getFloatValue() const { return floatValue; }

  const std::string &getSpelling() const { return spelling; }
  unsigned getRequiredBitWidth() const { return requiredBitWidth; }

  bool fitsInSignedBits(unsigned bits) const {
    return intValue.isSignedIntN(bits);
  }

  bool fitsInUnsignedBits(unsigned bits) const {
    return intValue.isIntN(bits);
  }

  uint64_t getUnsignedValue() const {
    return intValue.getZExtValue();
  }

  int64_t getSignedValue() const {
    return intValue.getSExtValue();
  }

  double toDouble() const {
    if (isFloating()) {
      llvm::APFloat copy = floatValue;
      bool losesInfo = false;
      copy.convert(llvm::APFloat::IEEEdouble(),
                   llvm::APFloat::rmNearestTiesToEven,
                   &losesInfo);
      (void)losesInfo;
      return copy.convertToDouble();
    }

    if (intValue.isSignedIntN(64)) {
      return static_cast<double>(intValue.getSExtValue());
    }

    if (intValue.isIntN(64)) {
      return static_cast<double>(intValue.getZExtValue());
    }

    llvm::APFloat asFloat(llvm::APFloat::IEEEdouble(), intValue);
    return asFloat.convertToDouble();
  }
};

#endif // NUMERIC_LITERAL_H
