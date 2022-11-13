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
  SkipComment();

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
    while (!*CurPtr) {
      ++CurPtr;
    }
    Result.setKind(tok::eof);
  }
}

void Lexer::SkipWhiteSpace() {
  while (*CurPtr && charinfo::isWhitespace(*CurPtr)) {
    ++CurPtr;
  }
}

void Lexer::SkipComment() {
  if (*CurPtr == '/' && CurPtr[1] == '/') {
      CurPtr += 2;
      while (*CurPtr && *CurPtr != '\n') {
        ++CurPtr;
      }
  } else if (*CurPtr == '/' && CurPtr[1] == '*') {
      CurPtr += 2;
      while (CurPtr[0] != '*' || CurPtr[1] != '/') {
        if (!CurPtr[0] || !CurPtr[1]) {
          Diags.report(getLoc(), diag::err_unterminated_block_comment);
          return;
        }
        ++CurPtr;
      }
      CurPtr += 2;
  }
}

void Lexer::LexIdentifier(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  while (charinfo::isIdentifierBody(*End)) {
    ++End;
  }
  llvm::StringRef Name(Start, End-Start);
  formToken(Result, End, getKeyword(Name, tok::identifier));
}

void Lexer::LexCharacter(Token &Result) {
  const char *End = CurPtr + 1;
  if (*End == '\'') {
    Diags.report(getLoc(),diag::err_empty_char_constant);
    formToken(Result, End, tok::char_constant);
    return;
  } else if (*End == '\n' || !*End) {
    Diags.report(getLoc(), diag::err_unterminated_char_constant);
    formToken(Result, End, tok::char_constant);
    return;
  } else {
    if (*End == '\\') {
      ScanEscapeChar(End);
    } else {
      ++End;
    }
  }
  if (*End != '\'') {
    Diags.report(getLoc(), diag::err_unterminated_char_constant);
    formToken(Result, End, tok::char_constant);
  }else {
    ++End;
    formToken(Result, End, tok::char_constant);
  }
}

void Lexer::LexStringLiteral(Token &Result) {
  const char *Start = CurPtr;
  const char *End = CurPtr + 1;
  std::string value;
  while (*End != '"') {
    if (*End == '\n' || !*End) {
      Diags.report(getLoc(), diag::err_unterminated_string_constant);
      return;
    } else if (*End == '\\') {
      ScanEscapeChar(End);
    } else {
      ++End;
    }
  }
  // skip "
  ++End;
  formToken(Result, End, tok::string_literal, llvm::StringRef(CurPtr, End-Start));
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

void Lexer::ScanEscapeChar(const char *&Ptr) {
  ++Ptr;
  switch (*Ptr) {
  case 'a':
  case 'b':
  case 't':
  case 'v':
  case 'r':
  case 'f':
  case 'n':
  case '\'':
  case '\"':
  case '\\': {
    ++Ptr;
    break;
  }
  default:
    break;
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
#define KEYWORD(NAME, FLAGS) addKeyword(llvm::StringRef(#NAME), tok::kw_##NAME);
#include "TokenKinds.def"
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

void Lexer::formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind, Token::Variant Value) {
  size_t TokLen = TokEnd - CurPtr;
  Result.Ptr = CurPtr;
  Result.Length = TokLen;
  Result.Kind = Kind;
  Result.Loc = llvm::SMLoc::getFromPointer(CurPtr);
  Result.Value = Value;
}
} // namespace lcc::lexer
