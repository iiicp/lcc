/***********************************
 * File:     Lexer.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#ifndef LCC_LEXER_H
#define LCC_LEXER_H

#include "Token.h"
#include <vector>
namespace lcc::lexer {
class Lexer {
private:
  uint8_t *mCursor;
  uint8_t *mLineHead;
  uint8_t *mSrcEnd;
  uint32_t mLine;

public:
  Lexer(uint8_t *SrcStart, uint32_t SrcSize)
      : mCursor(SrcStart), mLineHead(SrcStart), mSrcEnd(SrcStart + SrcSize),
        mLine(1){};
  std::vector<Token> Tokenize();

private:
  Token GetNextToken();
  bool IsEOF(uint8_t *CharPtr);
  void SkipWhiteSpace();
};
} // namespace lcc::lexer

#endif // LCC_LEXER_H
