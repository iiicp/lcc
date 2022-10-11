/***********************************
 * File:     Lexer.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#include "Lexer.h"
#include <iostream>

namespace lcc::lexer {

std::vector<Token> Lexer::Tokenize() { return std::vector<Token>(); }
Token Lexer::GetNextToken() { return Token(0, 0, ellipsis); }

void Lexer::SkipWhiteSpace() {
  char ch = *mCursor;
  while (ch == ' ' || ch == '\t' || ch == '\v' || ch == '\f' || ch == '\r' ||
         ch == '\n' || ch == '/' || ch == '#') {
    switch (ch) {
    case '\n': {
      mLine++;
      mLineHead = ++mCursor;
      break;
    }
    case '/': {
      if (mCursor[1] != '/' || mCursor[1] != '*')
        return;
      ++mCursor;
      if (*mCursor == '/') {
        while (*mCursor != '\n' && !IsEOF(mCursor)) {
          ++mCursor;
        }
      } else {
        ++mCursor;
        while (mCursor[0] != '*' || mCursor[1] != '/') {
          if (*mCursor == '\n') {
            ++mLine;
            mLineHead = ++mCursor;
          } else if (IsEOF(mCursor) || IsEOF(mCursor + 1)) {
            std::cerr << "miss */"
                      << "line: " << mLine << ", col: " << mCursor - mLineHead
                      << std::endl;
            return;
          } else {
            ++mCursor;
          }
        }
        mCursor += 2;
      }
      break;
    }
    default: {
      ++mCursor;
      break;
    }
    }
    ch = *mCursor;
  }
}

bool Lexer::IsEOF(uint8_t *CharPtr) {
  return (*CharPtr == '\0' && CharPtr == mSrcEnd);
}
} // namespace lcc::lexer
