/***********************************
 * File:     ChaLitTokGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "ChaLitTokGen.h"

namespace lcc {
namespace lexer {
Token ChaLitTokGen::ParseToken() {
  // skip '
  mCursor++;
  char ch;
  uint64_t line = mLine;
  uint64_t column = GetColumn();

  if (*mCursor == '\n' || IsEOF(mCursor)) {
    throw std::runtime_error("missing terminating ' character");
  } else if (*mCursor == '\'') {
    throw std::runtime_error("empty character constant");
  } else {
    if (*mCursor == '\\') {
      ch = ScanEscapeChar();
    } else {
      ch = *mCursor;
      mCursor++;
    }
  }
  if (*mCursor != '\'') {
    std::runtime_error("missing terminating ' character");
  }
  // skip '
  mCursor++;
  return Token{line, column, char_constant, ch};
}

bool ChaLitTokGen::CanParseToken() { return *mCursor == '\''; }
} // namespace Lexer
} // namespace lcc