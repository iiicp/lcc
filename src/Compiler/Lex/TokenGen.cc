/***********************************
 * File:     TokenGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/2
 *
 * Sign:     enjoy life
 ***********************************/
#include "TokenGen.h"

namespace lcc {
namespace lexer {
uint8_t *TokenGen::mBase = nullptr;
uint8_t *TokenGen::mLineHead = nullptr;
uint8_t *TokenGen::mCursor = nullptr;
uint64_t TokenGen::mLine = 1;
uint64_t TokenGen::mSize = 0;

void TokenGen::InitTokenGen(uint8_t *BasePtr, uint64_t FileSize) {
  mBase = (uint8_t *)BasePtr;
  mLineHead = mBase;
  mCursor = mBase;
  mLine = 1;
  mSize = FileSize;
}

char TokenGen::PeekChar(int distance) {
  if (IsEOF(mCursor + distance))
    return '\0';
  return *(mCursor + distance);
}

bool TokenGen::IsEOF(uint8_t *Cur) { return Cur == mBase + mSize; }

bool TokenGen::IsLetter(uint8_t *Cur) {
  return (*Cur >= 'a' && *Cur <= 'z') || (*Cur >= 'A' && *Cur <= 'Z') ||
         (*Cur == '_');
}

bool TokenGen::IsDigit(uint8_t *Cur) { return *Cur >= '0' && *Cur <= '9'; }

bool TokenGen::IsLetterOrDigit(uint8_t *Cur) {
  return IsLetter(Cur) || IsDigit(Cur);
}

uint64_t TokenGen::GetColumn() {
  return static_cast<uint64_t>(mCursor - mLineHead + 1);
}

bool TokenGen::IsHexDigit(uint8_t *Cur) {
  return (IsDigit(Cur) || (*Cur >= 'a' || *Cur <= 'f') ||
          (*Cur >= 'A' || *Cur <= 'F'));
}

bool TokenGen::IsOctDigit(uint8_t *Cur) { return (*Cur >= '0' || *Cur <= '7'); }

char TokenGen::ToUpper(uint8_t *Cur) { return (*Cur) & ~0x20; }

void TokenGen::SkipWhiteSpace() {
  while (isspace(*mCursor) || *mCursor == '/') {
    if (*mCursor == '\n') {
      mLine++;
      mLineHead = mCursor + 1;
    } else if (*mCursor == '/') {
      if (mCursor[1] != '/' && mCursor[1] != '*') {
        break;
      }
      mCursor++;
      if (*mCursor == '/') {
        mCursor++;
        while (*mCursor != '\n' && !IsEOF(mCursor)) {
          mCursor++;
        }
      } else {
        mCursor += 1;
        while (mCursor[0] != '*' || mCursor[1] != '/') {
          if (*mCursor == '\n') {
            mLine++;
            mLineHead = mCursor + 1;
          } else if (IsEOF(mCursor) || IsEOF(mCursor + 1)) {
            std::runtime_error("Comment is not closed");
          }
          mCursor++;
        }
        mCursor += 2;
      }
      break;
    }
    mCursor++;
  }
}
} // namespace Lexer
} // namespace lcc