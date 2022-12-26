/***********************************
 * File:     Lexer.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#include "lcc/Lexer/Lexer.h"
#include <cassert>
#include <iostream>
namespace lcc {

namespace charinfo {
LLVM_READNONE inline bool isASCII(char Ch) {
  return static_cast<unsigned char>(Ch) <= 127;
}

LLVM_READNONE inline bool isVerticalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == '\r' || Ch == '\n');
}

LLVM_READNONE inline bool isHorizontalWhitespace(char Ch) {
  return isASCII(Ch) && (Ch == ' ' || Ch == '\t' || Ch == '\f' || Ch == '\v');
}

LLVM_READNONE inline bool isWhitespace(char Ch) {
  return isHorizontalWhitespace(Ch) || isVerticalWhitespace(Ch);
}

LLVM_READNONE inline bool isDigit(char Ch) {
  return isASCII(Ch) && Ch >= '0' && Ch <= '9';
}

LLVM_READNONE inline bool isOctDigit(char Ch) {
  return isASCII(Ch) && Ch >= '0' && Ch <= '7';
}

LLVM_READNONE inline bool isHexDigit(char Ch) {
  return isASCII(Ch) && (isDigit(Ch) || (Ch >= 'A' && Ch <= 'F'));
}

LLVM_READNONE inline bool isIdentifierHead(char Ch) {
  return isASCII(Ch) &&
         (Ch == '_' || (Ch >= 'A' && Ch <= 'Z') || (Ch >= 'a' && Ch <= 'z'));
}
LLVM_READNONE inline bool isIdentifierBody(char Ch) {
  return isIdentifierHead(Ch) || isDigit(Ch);
}
LLVM_READNONE inline bool isPunctuatorHead(char Ch) {
  return isASCII(Ch) &&
         (Ch == '[' || Ch == ']' || Ch == '(' || Ch == ')' || Ch == '{' ||
          Ch == '}' || Ch == '.' || Ch == '&' || Ch == '*' || Ch == '+' ||
          Ch == '-' || Ch == '~' || Ch == '!' || Ch == '/' || Ch == '%' ||
          Ch == '<' || Ch == '>' || Ch == '^' || Ch == '|' || Ch == '?' ||
          Ch == ':' || Ch == ';' || Ch == '=' || Ch == ',');
}

LLVM_READNONE inline bool isNumericHead(char Ch, char PeekCh) {
  return isASCII(Ch) &&
         (charinfo::isDigit(Ch) || (Ch == '.' && charinfo::isDigit(PeekCh)));
}
LLVM_READNONE inline bool isCharHead(char Ch) {
  return isASCII(Ch) && (Ch == '\'');
}

LLVM_READNONE inline bool isStringHead(char Ch) {
  return isASCII(Ch) && (Ch == '"');
}
} // namespace charinfo

std::vector<Token> Lexer::Tokenize() {
  std::vector<Token> tokens;
  while (*CurPtr) {
    Token tok;
    next(tok);
    if (tok.getKind() != tok::eof)
      tokens.push_back(tok);
  }
  return tokens;
}

void Lexer::next(Token &Result) {
  SkipWhiteSpace();
  if (charinfo::isIdentifierHead(*CurPtr)) {
    LexIdentifier(Result);
  } else if (charinfo::isNumericHead(*CurPtr, CurPtr[1])) { // .1f
    LexNumeric(Result);
  } else if (charinfo::isPunctuatorHead(*CurPtr)) {
    LexPunctuator(Result);
  } else if (charinfo::isCharHead(*CurPtr)) {
    LexCharacter(Result);
  } else if (charinfo::isStringHead(*CurPtr)) {
    LexStringLiteral(Result);
  } else {
    while (*CurPtr != '\0') {
      ++CurPtr;
      Diags.report(getSMLoc(), diag::warning_skip_unknown_char);
    }
    Result.setKind(tok::eof);
  }
}

void Lexer::SkipWhiteSpace() {
  char ch = *CurPtr;
  while (charinfo::isWhitespace(ch) || ch == '/') {
    switch (ch) {
    case '/': {
      if (CurPtr[1] != '/' && CurPtr[1] != '*')
        return;
      ++CurPtr;
      if (*CurPtr == '/') {
        while (*CurPtr != '\n' && *CurPtr != '\0') {
          ++CurPtr;
        }
      } else {
        ++CurPtr;
        while (CurPtr[0] != '*' || CurPtr[1] != '/') {
          if (CurPtr[0] == '\0' || CurPtr[1] == '\0') {
            Diags.report(getSMLoc(), diag::err_unterminated_block_comment);
            return;
          }
          ++CurPtr;
        }
        CurPtr += 2;
      }
      break;
    }
    default: {
      ++CurPtr;
      break;
    }
    }
    ch = *CurPtr;
  }
}

void Lexer::LexIdentifier(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isIdentifierBody(*End)) {
    ++End;
  }
  llvm::StringRef Name(Start, End - Start);
  formToken(Result, End, getKeyword(Name, tok::identifier));
}

void Lexer::LexCharacter(Token &Result) {
  const char *End = CurPtr + 1;

  if (*End == '\0') {
    Diags.report(getSMLoc(), diag::err_unclosed_char_literal);
    formToken(Result, End, tok::char_constant);
    return;
  }

  if (*End == '\'') {
    ++End;
    Diags.report(getSMLoc(), diag::err_empty_char_literal);
    formToken(Result, End, tok::char_constant);
    return;
  }

  int c;
  if (*End == '\\') {
    c = ScanEscapeChar(End);
  }else {
    c = *End++;
  }

  if (*End != '\'') {
    Diags.report(getSMLoc(), diag::err_unclosed_char_literal);
    formToken(Result, End, tok::char_constant);
    return;
  }
  ++End;
  formToken(Result, End, tok::char_constant, c);
}

void Lexer::LexStringLiteral(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  std::string value;
  while (*End != '"') {
    if (*End == '\n' || *End == '\0') {
      Diags.report(getSMLoc(), diag::err_unclosed_string_literal);
      return;
    } else if (*End == '\\') {
      ScanEscapeChar(End);
    } else {
      ++End;
    }
  }
  // skip "
  ++End;
  formToken(Result, End, tok::string_literal,
            llvm::StringRef(CurPtr, End - Start));
}

void Lexer::LexNumeric(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  int32_t value = (*Start - '0');
  while (charinfo::isDigit(*End)) {
    value = value * 10 + (*End++ - '0');
  }
  formToken(Result, End, tok::numeric_constant, value);
}

int Lexer::ScanEscapeChar(const char *&Ptr) {
  Ptr += 2;
  switch (*Ptr) {
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
    return '\'';
  case '\"':
    return '\"';
  case '\\':
    return '\\';
  default: {
    return *(Ptr - 1);
  }
  }
}

void Lexer::LexPunctuator(Token &Result) {
  switch (*CurPtr) {
#define CASE(ch, tok)                                                          \
  case ch:                                                                     \
    formToken(Result, CurPtr + 1, tok);                                        \
    break;
    CASE('[', tok::l_square);
    CASE(']', tok::r_square);
    CASE('(', tok::l_paren);
    CASE(')', tok::r_paren);
    CASE('{', tok::l_brace);
    CASE('}', tok::r_brace);
    CASE('~', tok::tilde);
    CASE('?', tok::question);
    CASE(':', tok::colon);
    CASE(';', tok::semi);
    CASE(',', tok::comma);
#undef CASE
  case '.': {
    if (CurPtr[1] == '.' && CurPtr[2] == '.') {
      formToken(Result, CurPtr + 3, tok::ellipsis);
    } else {
      formToken(Result, CurPtr + 1, tok::period);
    }
    break;
  }
  case '&': {
    if (CurPtr[1] == '&') {
      formToken(Result, CurPtr + 2, tok::amp_amp);
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::amp_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::amp);
    }
    break;
  }
  case '*': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::star_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::star);
    }
    break;
  }
  case '+': {
    if (CurPtr[1] == '+') {
      formToken(Result, CurPtr + 2, tok::plus_plus);
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::plus_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::plus);
    }
    break;
  }
  case '-': {
    if (CurPtr[1] == '>') {
      formToken(Result, CurPtr + 2, tok::arrow);
    } else if (CurPtr[1] == '-') {
      formToken(Result, CurPtr + 2, tok::minus_minus);
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::minus_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::minus);
    }
    break;
  }
  case '!': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::exclaim_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::exclaim);
    }
    break;
  }
  case '/': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::slash_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::slash);
    }
    break;
  }
  case '%': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::percent_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::percent);
    }
    break;
  }
  case '<': {
    if (CurPtr[1] == '<') {
      if (CurPtr[2] == '=') {
        formToken(Result, CurPtr + 3, tok::less_less_equal);
      } else {
        formToken(Result, CurPtr + 2, tok::less_less);
      }
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::less_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::less);
    }
    break;
  }
  case '>': {
    if (CurPtr[1] == '>') {
      if (CurPtr[2] == '=') {
        formToken(Result, CurPtr + 3, tok::greater_greater_equal);
      } else {
        formToken(Result, CurPtr + 2, tok::greater_greater);
      }
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::greater_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::greater);
    }
    break;
  }
  case '^': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::caret_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::caret);
    }
    break;
  }
  case '|': {
    if (CurPtr[1] == '|') {
      formToken(Result, CurPtr + 2, tok::pipe_pipe);
    } else if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::pipe_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::pipe);
    }
    break;
  }

  case '=': {
    if (CurPtr[1] == '=') {
      formToken(Result, CurPtr + 2, tok::equal_equal);
    } else {
      formToken(Result, CurPtr + 1, tok::equal);
    }
    break;
  }
  }
}

void Lexer::addKeywords() {
#define KEYWORD(NAME) addKeyword(llvm::StringRef(#NAME), tok::kw_##NAME);
#include "lcc/Basic/TokenKinds.def"
}

void Lexer::addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode) {
  HashTable.insert(std::make_pair(Keyword, TokenCode));
}

tok::TokenKind Lexer::getKeyword(llvm::StringRef Name,
                                 tok::TokenKind DefaultTokenCode) {
  auto Result = HashTable.find(Name);
  if (Result != HashTable.end())
    return Result->second;
  return DefaultTokenCode;
}

void Lexer::formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind,
                      Token::Variant Value) {
  size_t TokLen = TokEnd - CurPtr;
  Result.Ptr = CurPtr;
  Result.Length = TokLen;
  Result.Kind = Kind;
  Result.Loc = llvm::SMLoc::getFromPointer(CurPtr);
  Result.Value = Value;
  CurPtr = TokEnd;
}
} // namespace lcc
