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
#include <cassert>
namespace lcc {

std::vector<Token> Lexer::Tokenize() {
  std::vector<Token> tokens;
  while (!IsEOF(mCursor)) {
    Token tok = GetNextToken();
    if (tok.GetTokenType() != tok::eof)
      tokens.push_back(tok);
  }
  return std::move(tokens);
}

Token Lexer::GetNextToken() {
  ScanWhiteSpace();
  mColumn = mCursor - mLineHead + 1;
  if (IsLetter()) {
    return ScanIdentifier();
  } else if (IsNumericStart()) { // .1f
    return ScanNumeric();
  } else if (IsPunctuatorStart()) {
    return ScanPunctuator();
  } else if (IsCharStart()) {
    return ScanCharacter();
  } else if (IsStringStart()) {
    return ScanStringLiteral();
  } else {
    while (!IsEOF(mCursor)) {
      ++mCursor;
    }
    return Token{mLine, mColumn, tok::eof};
  }
}

bool Lexer::IsEOF(uint8_t *CharPtr) const {
  return (*CharPtr == '\0' && CharPtr == mSrcEnd);
}

bool Lexer::IsLetter() const {
  uint8_t ch = *mCursor;
  return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '_');
}
bool Lexer::IsDigit() const {
  uint8_t ch = *mCursor;
  return '0' <= ch && ch <= '9';
}
bool Lexer::IsHexDigit() const {
  uint8_t ch = *mCursor;
  return IsDigit() || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F';
}
bool Lexer::IsOctDigit() const {
  uint8_t ch = *mCursor;
  return '0' <= ch && ch <= '7';
}

bool Lexer::IsXDigit(int base) const {
  if (base == 8) {
    return IsOctDigit();
  } else if (base == 16) {
    return IsHexDigit();
  } else {
    return IsDigit();
  }
}

bool Lexer::IsDigit(uint8_t *CharPtr) const {
  uint8_t ch = *CharPtr;
  return '0' << ch && ch <= '9';
}
bool Lexer::IsLetterOrDigit() const { return IsLetter() || IsDigit(); }

bool Lexer::IsPunctuatorStart() const {
  uint8_t ch = *mCursor;
  return ch == '[' || ch == ']' || ch == '(' || ch == ')' || ch == '{' ||
         ch == '}' || ch == '.' || ch == '&' || ch == '*' || ch == '+' ||
         ch == '-' || ch == '~' || ch == '!' || ch == '/' || ch == '%' ||
         ch == '<' || ch == '>' || ch == '^' || ch == '|' || ch == '?' ||
         ch == ':' || ch == ';' || ch == '=' || ch == ',';
}

bool Lexer::IsNumericStart() const {
  return IsDigit() || (*mCursor == '.' && IsDigit(mCursor + 1));
}

bool Lexer::IsCharStart() const { return *mCursor == '\''; }

bool Lexer::IsStringStart() const { return *mCursor == '"'; }

void Lexer::ScanWhiteSpace() {
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
      if (mCursor[1] != '/' && mCursor[1] != '*')
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

Token Lexer::ScanIdentifier() {
  uint8_t *b = mCursor++;
  while (IsLetterOrDigit()) {
    ++mCursor;
  }
  std::string value(b, mCursor);
  if (mKeywordTypeMap.find(value) != mKeywordTypeMap.end()) {
    return Token{mLine, mColumn, mKeywordTypeMap[value], value};
  } else {
    return Token{mLine, mColumn, tok::identifier, value};
  }
}

Token Lexer::ScanCharacter() {
  ++mCursor;
  int32_t ch;
  if (*mCursor == '\'') {
    std::cerr << "empty character '"
              << "line: " << mLine << ", col: " << mCursor - mLineHead
              << std::endl;
    assert(0);
  } else if (*mCursor == '\n' || IsEOF(mCursor)) {
    std::cerr << "empty character, miss '"
              << "line: " << mLine << ", col: " << mCursor - mLineHead
              << std::endl;
    assert(0);
  } else {
    if (*mCursor == '\\') {
      ch = ScanEscapeChar();
    } else {
      ch = *mCursor++;
    }
  }
  if (*mCursor != '\'') {
    std::cerr << "unclosed '" << mLine << ", col: " << mCursor - mLineHead
              << std::endl;
    assert(0);
  }
  ++mCursor;
  return Token{mLine, mColumn, tok::char_constant, ch};
}

Token Lexer::ScanStringLiteral() {
  ++mCursor;
  std::string value;
  while (*mCursor != '"') {
    if (*mCursor == '\n' || IsEOF(mCursor)) {
      std::cerr << "legal str " << mLine << ", col: " << mCursor - mLineHead
                << std::endl;
      assert(0);
    } else if (*mCursor == '\\') {
      value += (char)ScanEscapeChar();
    } else {
      value += (char)*mCursor++;
    }
  }
  ++mCursor;
  return Token{mLine, mColumn, tok::string_literal, value};
}

Token Lexer::ScanNumeric() {
  uint8_t *start = mCursor;
  int base = 10;

  if (*mCursor == '.') {
    return ScanFloatNumeric();
  }

  if (*mCursor == '0') {
    base = 8;
    mCursor++;
    if (*mCursor == 'x' || *mCursor == 'X') {
      base = 16;
      mCursor++;
    }
  }

  while (IsXDigit(base)) {
    ++mCursor;
  }

  if (*mCursor == '.' || *mCursor == 'E' || *mCursor == 'e') {
    if (base == 8) {
      std::cerr << "no oct float " << mLine << ", col: " << mCursor - mLineHead
                << std::endl;
      assert(0);
    }
    mCursor = start;
    return ScanFloatNumeric();
  }

  mCursor = start;
  return ScanIntegerNumeric(base);
}

Token Lexer::ScanIntegerNumeric(int base) {
  if (base == 16) {
    mCursor += 2;
  }
  uint64_t value = (*mCursor++ - '0');
  while (IsXDigit(base)) {
    value = value * base + (*mCursor++ - '0');
  }
  IntegerType type = ScanIntegerSuffix();
  if (type == IntegerType::U) {
    return Token{mLine, mColumn, tok::numeric_constant, (uint32_t)value};
  } else if (type == IntegerType::UL || type == IntegerType::ULL) {
    return Token{mLine, mColumn, tok::numeric_constant, (uint64_t)value};
  } else if (type == IntegerType::L || type == IntegerType::LL) {
    return Token{mLine, mColumn, tok::numeric_constant, (int64_t)value};
  } else {
    return Token{mLine, mColumn, tok::numeric_constant, (int32_t)value};
  }
}

Token Lexer::ScanFloatNumeric() {
  const char *start = (const char *)mCursor;
  char *end = nullptr;
  double value = strtod(start, &end);
  if (end == nullptr)
    assert(0);
  mCursor = (uint8_t *)end;
  if (*mCursor == 'f' || *mCursor == 'F') {
    ++mCursor;
    return Token{mLine, mColumn, tok::numeric_constant, (float)value};
  } else if (*mCursor == 'L' || *mCursor == 'l') {
    ++mCursor;
    return Token{mLine, mColumn, tok::numeric_constant, value};
  } else {
    return Token{mLine, mColumn, tok::numeric_constant, value};
  }
}

IntegerType Lexer::ScanIntegerSuffix() {
  if (*mCursor == 'u' || *mCursor == 'U') {
    ++mCursor;
    if (*mCursor == 'l' || *mCursor == 'L') {
      ++mCursor;
      if (*mCursor == 'L' || *mCursor == 'L') {
        ++mCursor;
        return IntegerType::ULL;
      } else {
        return IntegerType::UL;
      }
    } else {
      return IntegerType::U;
    }
  } else if (*mCursor == 'l' || *mCursor == 'L') {
    ++mCursor;
    if (*mCursor == 'l' || *mCursor == 'L') {
      ++mCursor;
      if (*mCursor == 'u' || *mCursor == 'U') {
        ++mCursor;
        return IntegerType::ULL;
      } else {
        return IntegerType::LL;
      }
    } else {
      return IntegerType::L;
    }
  } else {
    return IntegerType::Default;
  }
};

int32_t Lexer::ScanEscapeChar() {
  ++mCursor;
  int32_t ch;
  switch (*mCursor++) {
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 't':
    return '\t';
  case 'v':
    return '\v';
  case 'r':
    return '\r';
  case 'f':
    return '\f';
  case 'n':
    return '\n';
  case '\'':
  case '\"':
  case '\?':
  case '\\':
    return *(mCursor - 1);
  case 'x': {
    if (!IsHexDigit()) {
      std::cerr << "warning: miss hexdigit "
                << "line: " << mLine << ", col: " << mCursor - mLineHead
                << std::endl;
      return 'x';
    }
    ch = *mCursor++;
    // \xffa
    // (f*16+f)*16+a
    while (IsHexDigit()) {
      if ('a' <= *mCursor && *mCursor <= 'f') {
        ch = (ch << 4) + (*mCursor - 'a') + 10;
      } else if ('A' <= *mCursor && *mCursor <= 'F') {
        ch = (ch << 4) + (*mCursor - 'A') + 10;
      } else {
        ch = (ch << 4) + (*mCursor - '0');
      }
    }
    return ch;
  }
  case 0:
  case 1:
  case 2:
  case 3:
  case 4:
  case 5:
  case 6:
  case 7: {
    ch = *mCursor++;
    if (IsOctDigit()) {
      ch = (ch << 3) + *mCursor++ - '0';
      if (IsOctDigit()) {
        ch = (ch << 3) + *mCursor++ - '0';
      }
    }
    return ch;
  }
  default:
    std::cerr << "warning illegal escape: " << *(mCursor - 1)
              << "line: " << mLine << ", col: " << mCursor - mLineHead
              << std::endl;
    return *(mCursor - 1);
  }
}

Token Lexer::ScanPunctuator() {
  tok::TokenKind type = tok::unknown;
  switch (*mCursor) {
  case '[': {
    type = tok::l_square;
    ++mCursor;
    break;
  }
  case ']': {
    type = tok::r_square;
    ++mCursor;
    break;
  }
  case '(': {
    type = tok::l_paren;
    ++mCursor;
    break;
  }
  case ')': {
    type = tok::r_paren;
    ++mCursor;
    break;
  }
  case '{': {
    type = tok::l_brace;
    ++mCursor;
    break;
  }
  case '}': {
    type = tok::r_brace;
    ++mCursor;
    break;
  }
  case '.': {
    mCursor++;
    if (*mCursor == '.' && !IsEOF(mCursor + 1) && mCursor[1] == '.') {
      mCursor += 2;
      type = tok::ellipsis;
    } else {
      type = tok::period;
    }
    break;
  }
  case '&': {
    ++mCursor;
    if (*mCursor == '&') {
      ++mCursor;
      type = tok::amp_amp;
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::amp_equal;
    } else {
      type = tok::amp;
    }
    break;
  }
  case '*': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::star_equal;
    } else {
      type = tok::star;
    }
    break;
  }
  case '+': {
    ++mCursor;
    if (*mCursor == '+') {
      ++mCursor;
      type = tok::plus_plus;
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::plus_equal;
    } else {
      type = tok::plus;
    }
    break;
  }
  case '-': {
    ++mCursor;
    if (*mCursor == '>') {
      ++mCursor;
      type = tok::arrow;
    } else if (*mCursor == '-') {
      ++mCursor;
      type = tok::minus_minus;
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::minus_equal;
    } else {
      type = tok::minus;
    }
    break;
  }
  case '~': {
    ++mCursor;
    type = tok::tilde;
    break;
  }
  case '!': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::exclaim_equal;
    } else {
      type = tok::exclaim;
    }
    break;
  }
  case '/': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::slash_equal;
    } else {
      type = tok::slash;
    }
    break;
  }
  case '%': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::percent_equal;
    } else {
      type = tok::percent;
    }
    break;
  }
  case '<': {
    ++mCursor;
    if (*mCursor == '<') {
      ++mCursor;
      if (*mCursor == '=') {
        ++mCursor;
        type = tok::less_less_equal;
      } else {
        type = tok::less_less;
      }
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::less_equal;
    } else {
      type = tok::less;
    }
    break;
  }
  case '>': {
    ++mCursor;
    if (*mCursor == '>') {
      ++mCursor;
      if (*mCursor == '=') {
        ++mCursor;
        type = tok::greater_greater_equal;
      } else {
        type = tok::greater_greater;
      }
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::greater_equal;
    } else {
      type = tok::greater;
    }
    break;
  }
  case '^': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::caret_equal;
    } else {
      type = tok::caret;
    }
    break;
  }
  case '|': {
    ++mCursor;
    if (*mCursor == '|') {
      ++mCursor;
      type = tok::pipe_pipe;
    } else if (*mCursor == '=') {
      ++mCursor;
      type = tok::pipe_equal;
    } else {
      type = tok::pipe;
    }
    break;
  }
  case '?': {
    ++mCursor;
    type = tok::question;
    break;
  }
  case ':': {
    ++mCursor;
    type = tok::colon;
    break;
  }
  case ';': {
    ++mCursor;
    type = tok::semi;
    break;
  }
  case ',': {
    ++mCursor;
    type = tok::comma;
    break;
  }
  case '=': {
    ++mCursor;
    if (*mCursor == '=') {
      ++mCursor;
      type = tok::equal_equal;
    } else {
      type = tok::equal;
    }
    break;
  }
  }
  return Token{mLine, mColumn, type};
}

void Lexer::InitKeyWordTypeMap() {
  mKeywordTypeMap = {{"auto", tok::kw_auto},         {"break", tok::kw_break},
                     {"case", tok::kw_case},         {"char", tok::kw_char},
                     {"const", tok::kw_const},       {"continue", tok::kw_continue},
                     {"default", tok::kw_default},   {"do", tok::kw_do},
                     {"double", tok::kw_double},     {"else", tok::kw_else},
                     {"enum", tok::kw_enum},         {"extern", tok::kw_extern},
                     {"float", tok::kw_float},       {"for", tok::kw_for},
                     {"goto", tok::kw_goto},         {"if", tok::kw_if},
                     {"inline", tok::kw_inline},     {"int", tok::kw_int},
                     {"long", tok::kw_long},         {"register", tok::kw_register},
                     {"restrict", tok::kw_restrict}, {"return", tok::kw_return},
                     {"short", tok::kw_short},       {"signed", tok::kw_signed},
                     {"sizeof", tok::kw_sizeof},     {"static", tok::kw_static},
                     {"struct", tok::kw_struct},     {"switch", tok::kw_switch},
                     {"typedef", tok::kw_typedef},   {"union", tok::kw_union},
                     {"unsigned", tok::kw_unsigned}, {"void", tok::kw_void},
                     {"volatile", tok::kw_volatile}, {"while", tok::kw_while}};
}
} // namespace lcc::lexer
