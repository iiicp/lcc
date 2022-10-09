/***********************************
 * File:     StrLitTokGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "StrLitTokGen.h"
#include <string>

namespace lcc {
namespace lexer {
Token StrLitTokGen::ParseToken() {
  uint64_t line = mLine;
  uint64_t column = GetColumn();
  mCursor++; //->  skip "
  std::string tmp;
  tmp.reserve(512);
  int32_t ch = 0;
  while (*mCursor != '"') {
    if (*mCursor == '\n' || IsEOF(mCursor))
      break;
    if (*mCursor == '\\') {
      ch = ScanEscapeChar();
    } else {
      ch = *mCursor;
      mCursor++;
    }
    tmp.push_back(ch);
  }
  mCursor++; //-> skip "
  tmp.shrink_to_fit();
  return Token{line, column, string_literal, tmp};
}

bool StrLitTokGen::CanParseToken() { return *mCursor == '"'; }
} // namespace Lexer
} // namespace lcc
