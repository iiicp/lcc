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
#include <string>
#include <vector>
#include <optional>
#include "LanguageOption.h"
#include "Diagnostic.h"

namespace lcc {

enum class State {
  Start,
  CharacterLiteral,
  StringLiteral,
  Identifier,
  Number,
  Punctuator,
  LineComment,
  BlockComment,
  AfterInclude
};

class Lexer {
private:
  LanguageOption mLangOption{LanguageOption::C99};
  State state = State::Start;
  llvm::SourceMgr &Mgr;
  DiagnosticEngine &Diag;
  std::string mSourceCode;
  const char *P{nullptr};
  const char *Ep{nullptr};
public:
  explicit Lexer(llvm::SourceMgr &mgr, DiagnosticEngine &diag, std::string &&sourceCode, std::string_view sourcePath = "<stdin>", LanguageOption option = LanguageOption::C99);
  std::vector<Token> tokenize();
  std::vector<Token> toCTokens(std::vector<Token>&& ppTokens);

private:
  void RegularSourceCode();
  static bool IsLetter(char ch) ;
  static bool IsWhiteSpace(char ch) ;
  static bool IsDigit(char ch) ;
  static bool IsOctDigit(char ch) ;
  static bool IsHexDigit(char ch) ;
  static bool IsPunctuation(char ch) ;
  static uint32_t OctalToNum(std::string_view value) ;
  static tok::TokenKind ParsePunctuation(const char* &offset, char curChar, char nextChar, char nnChar);

  Token::ValueType ParseNumber(const Token &ppToken);
  std::vector<char> ParseCharacters(const Token &ppToken, bool handleCharMode);
  std::uint32_t ParseEscapeChar(const char *p, char escape);
  static bool IsJudgeNumber(const std::string &preCharacters, char curChar);
};
}

#endif // LCC_LEXER_H
