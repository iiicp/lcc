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

enum class LanguageOption {
  PreProcess,
  C
};

class Lexer {
private:
  LanguageOption mLangOption{LanguageOption::C};
  uint32_t tokenStartOffset{0};
  bool leadingWhiteSpace = false;
  char delimiter{' '};
  std::string characters;
  State state = State::Start;
  SourceFile mSourceFile;
public:
  explicit Lexer(std::string &sourceCode, std::string_view sourcePath = "<stdin>", LanguageOption option = LanguageOption::C);
  std::vector<Token> tokenize();
  static std::vector<Token> toCTokens(std::vector<Token>&& ppTokens);

private:
  static void RegularSourceCode(std::string &sourceCode);
  static bool IsLetter(char ch) ;
  static bool IsWhiteSpace(char ch) ;
  static bool IsDigit(char ch) ;
  static bool IsOctDigit(char ch) ;
  static bool IsHexDigit(char ch) ;
  static bool IsPunctuation(char ch) ;
  [[nodiscard]] uint32_t GetLine(uint32_t offset) const;
  [[nodiscard]] uint32_t GetColumn(uint32_t offset) const;
  static uint32_t OctalToNum(std::string_view value) ;
  static Token::ValueType ParseNumber(const Token &ppToken);
  static tok::TokenKind ParsePunctuation(uint32_t &offset, char curChar, char nextChar, char nnChar);
  static Token::ValueType ParseStringLiteral(const Token &ppToken);
  static std::optional<std::vector<char>> ProcessCharacters(std::string_view characters);
  static std::optional<std::uint32_t> EscapeCharToValue(char escape);
  static bool IsJudgeNumber(const std::string &preCharacters, char curChar);
};
}

#endif // LCC_LEXER_H
