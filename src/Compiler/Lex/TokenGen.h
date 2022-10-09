/***********************************
 * File:     TokenGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/2
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_TOKENGEN_H
#define LCC_TOKENGEN_H

#include "Token.h"

namespace lcc {
namespace lexer {
class TokenGen {
public:
  uint64_t GetColumn();

  bool IsEOF(uint8_t *Cur);

  bool IsLetter(uint8_t *Cur);

  bool IsDigit(uint8_t *Cur);

  bool IsLetterOrDigit(uint8_t *Cur);

  bool IsHexDigit(uint8_t *Cur);

  bool IsOctDigit(uint8_t *Cur);

  char ToUpper(uint8_t *Cur);

  char PeekChar(int distance);

  void SkipWhiteSpace();

  virtual bool CanParseToken() = 0;

  virtual Token ParseToken() = 0;

  virtual ~TokenGen() {}

  static void InitTokenGen(uint8_t *BasePtr, uint64_t FileSize);

public:
  static uint8_t *mBase;
  static uint8_t *mLineHead;
  static uint8_t *mCursor;
  static uint64_t mLine;
  static uint64_t mSize;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_TOKENGEN_H
