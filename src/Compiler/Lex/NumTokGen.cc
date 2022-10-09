/***********************************
 * File:     NumTokGen.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "NumTokGen.h"
#include <sstream>

namespace lcc {
namespace lexer {
Token NumTokGen::ParseToken() {
  uint64_t line = mLine;
  uint64_t column = GetColumn();

  uint8_t *start = mCursor;
  int base = 10;

  if (*mCursor == '.') {
    return ScanFloatLiteral(start, line, column);
  }
  if (*mCursor == '0' && (mCursor[1] == 'x' || mCursor[1] == 'X')) { // hex
    mCursor += 2;
    start = mCursor;
    base = 16;
    if (!IsHexDigit(mCursor)) {
      std::runtime_error("Expect hex digit");
    }
    while (IsHexDigit(mCursor)) {
      mCursor++;
    }
  } else if (*mCursor == '0') { // octal
    mCursor++;
    base = 8;
    while (IsOctDigit(mCursor)) {
      mCursor++;
    }
  } else { // decimal
    mCursor++;
    while (IsDigit(mCursor)) {
      mCursor++;
    }
  }

  if (base == 16 || (*mCursor != '.' && *mCursor != 'e' && *mCursor != 'E')) {
    return ScanIntLiteral(start, (int)(mCursor - start), base, line, column);
  } else {
    return ScanFloatLiteral(start, line, column);
  }
}

Token NumTokGen::ScanIntLiteral(uint8_t *start, int len, int base,
                                uint64_t line, uint64_t column) {
  std::stringstream ss;
  if (base == 16) {
    ss << std::hex;
  } else if (base == 8) {
    ss << std::oct;
  }
  std::string val(start, start + len);
  ss << val;
  if (*mCursor == 'u' || *mCursor == 'U') {
    mCursor++;
    if (*mCursor == 'l' || *mCursor == 'L') {
      mCursor++;
      if (*mCursor == 'l' || *mCursor == 'L') { // ull uLL
        mCursor++;
        uint64_t number;
        ss >> number;
        return Token{line, column, numeric_constant, number};
      }
    }
    // ul or u
    uint64_t number;
    ss >> number;
    if (number > UINT32_MAX) {
      return Token{line, column, numeric_constant, number};
      ;
    } else {
      return Token{line, column, numeric_constant,
                   static_cast<uint32_t>(number)};
      ;
    }
  } else {
    if (*mCursor == 'l' || *mCursor == 'L') {
      mCursor++;
      if (*mCursor == 'l' || *mCursor == 'L') { // ll LL
        mCursor++;
        int64_t number;
        ss >> number;
        return Token{line, column, numeric_constant, number};
      }
    }
    /// no suffix or l
    std::int64_t number;
    ss >> number;
    if (number > INT32_MAX) {
      if (number <= UINT32_MAX) {
        return Token(line, column, numeric_constant,
                     static_cast<std::uint32_t>(number));
      } else {
        return Token(line, column, numeric_constant, number);
      }
    } else {
      return Token(line, column, numeric_constant,
                   static_cast<std::int32_t>(number));
    }
  }
}

Token NumTokGen::ScanFloatLiteral(uint8_t *Start, uint64_t line,
                                  uint64_t column) {
  double d;
  if (*mCursor == '.') {
    mCursor++;
    while (IsDigit(mCursor)) {
      mCursor++;
    }
  }

  if (*mCursor == 'e' || *mCursor == 'E') {
    mCursor++;
    if (*mCursor == '+' || *mCursor == '-') {
      mCursor++;
    }
    if (!IsDigit(mCursor)) {
      std::runtime_error("Expect exponent value");
    } else {
      while (IsDigit(mCursor)) {
        mCursor++;
      }
    }
  }
  d = strtod((char *)Start, NULL);
  if (*mCursor == 'f' || *mCursor == 'F') {
    mCursor++;
    return Token{line, column, numeric_constant, (float)d};
  } else if (*mCursor == 'L' || *mCursor == 'l') {
    mCursor++;
    return Token{line, column, numeric_constant, d};
  } else {
    return Token{line, column, numeric_constant, d};
  }
}

bool NumTokGen::CanParseToken() {
  return IsDigit(mCursor) || (*mCursor == '.' && IsDigit(mCursor + 1));
}
} // namespace Lexer
} // namespace lcc