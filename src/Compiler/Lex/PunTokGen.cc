/***********************************
 * File:     PunTokGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "PunTokGen.h"

namespace lcc {
namespace lexer {
Token PunTokGen::ParseToken() {
  TokenType type = unknown;
  uint64_t line = mLine;
  uint64_t column = GetColumn();
  switch (*mCursor) {
  case '[': {
    mCursor++;
    type = l_square;
    break;
  }
  case ']': {
    mCursor++;
    type = r_square;
    break;
  }
  case '(': {
    mCursor++;
    type = l_paren;
    break;
  }
  case ')': {
    mCursor++;
    type = r_paren;
    break;
  }
  case '{': {
    mCursor++;
    type = l_brace;
    break;
  }
  case '}': {
    mCursor++;
    type = r_brace;
    break;
  }
  case '~': {
    mCursor++;
    type = tilde;
    break;
  }
  case '?': {
    mCursor++;
    type = question;
    break;
  }
  case ':': {
    mCursor++;
    type = colon;
    break;
  }
  case ';': {
    mCursor++;
    type = semi;
    break;
  }
  case ',': {
    mCursor++;
    type = comma;
    break;
  }
  case '.': {
    mCursor++;
    if (PeekChar(0) == '.' && PeekChar(1) == '.') {
      mCursor += 2;
      type = ellipsis;
      break;
    } else {
      type = period;
      break;
    }
  }
  case '&': {
    mCursor++;
    if (PeekChar(0) == '&') {
      mCursor++;
      type = amp_amp;
      break;
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = amp_equal;
      break;
    } else {
      type = amp;
      break;
    }
  }
  case '*': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = star_equal;
      break;
    } else {
      type = star;
      break;
    }
  }
  case '+': {
    mCursor++;
    if (PeekChar(0) == '+') {
      mCursor++;
      type = plus_plus;
      break;
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = plus_equal;
      break;
    } else {
      type = plus;
      break;
    }
  }
  case '-': {
    mCursor++;
    if (PeekChar(0) == '>') {
      mCursor++;
      type = arrow;
      break;
    } else if (PeekChar(0) == '-') {
      mCursor++;
      type = minus_minus;
      break;
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = minus_equal;
      break;
    } else {
      type = minus;
      break;
    }
  }
  case '!': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = exclaim_equal;
      break;
    } else {
      type = exclaim;
      break;
    }
  }
  case '/': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = slash_equal;
      break;
    } else {
      type = slash;
      break;
    }
  }
  case '%': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = percent_equal;
      break;
    } else {
      type = percent;
      break;
    }
  }
  case '<': {
    mCursor++;
    if (PeekChar(0) == '<') {
      mCursor++;
      if (PeekChar(0) == '=') {
        mCursor++;
        type = less_less_equal;
        break;
      } else {
        type = less_less;
        break;
      }
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = less_equal;
      break;
    } else {
      type = less;
      break;
    }
  }
  case '>': {
    mCursor++;
    if (PeekChar(0) == '>') {
      mCursor++;
      if (PeekChar(0) == '=') {
        mCursor++;
        type = greater_greater_equal;
        break;
      } else {
        type = greater_greater;
        break;
      }
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = greater_equal;
      break;
    } else {
      type = greater;
      break;
    }
  }
  case '^': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = caret_equal;
      break;
    } else {
      type = caret;
      break;
    }
  }
  case '|': {
    mCursor++;
    if (PeekChar(0) == '|') {
      mCursor++;
      type = pipe_pipe;
      break;
    } else if (PeekChar(0) == '=') {
      mCursor++;
      type = pipe_equal;
      break;
    } else {
      type = pipe;
      break;
    }
  }
  case '=': {
    mCursor++;
    if (PeekChar(0) == '=') {
      mCursor++;
      type = equal_equal;
      break;
    } else {
      type = equal;
      break;
    }
  }
  case '#': {
    mCursor++;
    if (PeekChar(0) == '#') {
      mCursor++;
      type = hash_hash;
      break;
    } else {
      type = hash;
      break;
    }
  }
  case '\\': {
    mCursor++;
    type = back_slash;
    break;
  }
  default:
    assert(0);
  }
  return Token{line, column, type};
}

bool PunTokGen::CanParseToken() {
  if (FirstCharSet.find(*mCursor) == FirstCharSet.end()) {
    return false;
  }
  return true;
}

PunTokGen::PunTokGen() {
  FirstCharSet = {'[', ']', '(', ')', '{', '}', '.', '&', '*',
                  '=', '+', '-', '/', '%', '^', '|', '~', '!',
                  '<', '>', ':', ';', ',', '#', '\\'};
}
} // namespace Lexer
} // namespace lcc
