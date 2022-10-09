/***********************************
 * File:     LitTokGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "LitTokGen.h"

namespace lcc {
namespace lexer {
int32_t LitTokGen::ScanEscapeChar() {
  int32_t ch = 0;
  mCursor++;
  switch (*mCursor++) {
  case 'a': {
    ch = '\a';
    break;
  }
  case 'b': {
    ch = '\b';
    break;
  }
  case 'f': {
    ch = '\f';
    break;
  }
  case 'n': {
    ch = '\n';
    break;
  }
  case 'r': {
    ch = '\r';
    break;
  }
  case 't': {
    ch = '\t';
    break;
  }
  case 'v': {
    ch = '\v';
    break;
  }
  case '\'': { /// '\''
    ch = '\'';
    break;
  }
  case '"': { /// '\"'
    ch = '"';
    break;
  }
  case '\\': { /// '\\\\'
    ch = '\\';
    break;
  }
  case 'x': { /// \xhh
    if (!IsHexDigit(mCursor)) {
      std::runtime_error("Expect hex digit");
    }
    uint8_t *b = mCursor;
    while (IsHexDigit(mCursor)) {
      if (IsDigit(mCursor)) {
        ch = (ch << 4) + *mCursor - '0';
      } else {
        ch = (ch << 4) + ToUpper(mCursor) - 'A' + 10;
      }
      mCursor++;
    }
    break;
  }
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7': { /// \ddd octal
    ch = *(mCursor - 1) - '0';
    if (IsOctDigit(mCursor)) {
      ch = (ch << 3) + *mCursor++ - '0';
      if (IsOctDigit(mCursor)) {
        ch = (ch << 3) + *mCursor++ - '0';
      }
    }
    break;
  }
  default:
    std::runtime_error("Unrecognized escape sequence");
    break;
  }
  return ch;
}
} // namespace Lexer
} // namespace lcc
