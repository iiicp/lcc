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
#include "Utilities.h"
#include <cassert>
#include <iostream>
#include <sstream>
#include <unordered_map>

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

bool IsLetter(char ch) {
  return (ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch == '_');
}

bool IsWhiteSpace(char ch) {
  return (ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' || ch == '\f' ||
          ch == '\v');
}

bool IsDigit(char ch) { return ch >= '0' && ch <= '9'; }

bool IsPunctuation(char ch) {
  return ch == '[' || ch == ']' || ch == '(' || ch == ')' || ch == '{' ||
         ch == '}' || ch == '.' || ch == '&' || ch == '*' || ch == '+' ||
         ch == '-' || ch == '~' || ch == '!' || ch == '/' || ch == '%' ||
         ch == '<' || ch == '>' || ch == '^' || ch == '|' || ch == '?' ||
         ch == ':' || ch == ';' || ch == '=' || ch == ',' || ch == '#';
}

int32_t TokenStringToCharConstant(const std::string &tokenString) {
  if (tokenString.empty()) {
    throw std::runtime_error("Character constant can't be empty");
  }
  if (tokenString.size() == 1) {
    return tokenString[0];
  }
  if (tokenString == "\\'") {
    return '\'';
  }
  if (tokenString == "\\\"") {
    return '"';
  }
  if (tokenString == "\\?") {
    return '\?';
  }
  // todo
  if (tokenString == "\\\\") {
    return '\\';
  }
  if (tokenString == "\\a") {
    return '\a';
  }
  if (tokenString == "\\b") {
    return '\b';
  }
  if (tokenString == "\\f") {
    return '\f';
  }
  if (tokenString == "\\n") {
    return '\n';
  }
  if (tokenString == "\\r") {
    return '\r';
  }
  if (tokenString == "\\t") {
    return '\t';
  }
  if (tokenString == "\\v") {
    return '\v';
  }

  if (tokenString.starts_with('\\')) {
    if (tokenString[1] == 'x') {
      if (tokenString.size() <= 2) {
        throw std::runtime_error("At least one hexadecimal digit required");
      }
      std::istringstream ss(tokenString.substr(2, tokenString.size() - 2));
      int32_t number;
      if (!(ss >> std::hex >> number)) {
        throw std::runtime_error("Failed to convert " + ss.str() +
                                 " to hex character");
      }
      if (number > std::numeric_limits<uint8_t>::max()) {
        throw std::runtime_error(
            "Character constant is not allowed to have a value higher than the "
            "maximum value of unsigned char");
      }
      return number;
    } else {
      if (tokenString.size() <= 1) {
        throw std::runtime_error("At least one octal digit required");
      }
      std::istringstream ss(tokenString.substr(1, tokenString.size() - 1));
      int32_t number;
      if (!(ss >> std::oct >> number)) {
        throw std::runtime_error("Failed to convert " + ss.str() +
                                 " to octal character");
      }
      if (number > std::numeric_limits<uint8_t>::max()) {
        throw std::runtime_error(
            "Character constant is not allowed to have a value higher than the "
            "maximum value of unsigned char");
      }
      return number;
    }
  }
  throw std::runtime_error("Incorrect sequence for character literal: " +
                           tokenString);
}

tok::TokenKind ParsePunctuation(uint32_t &pos, char curChar, char nextChar,
                                char nnChar) {
  tok::TokenKind type = tok::unknown;
  switch (curChar) {
  case '[': {
    type = tok::l_square;
    ++pos;
    break;
  }
  case ']': {
    type = tok::r_square;
    ++pos;
    break;
  }
  case '(': {
    type = tok::l_paren;
    ++pos;
    break;
  }
  case ')': {
    type = tok::r_paren;
    ++pos;
    break;
  }
  case '{': {
    type = tok::l_brace;
    ++pos;
    break;
  }
  case '}': {
    type = tok::r_brace;
    ++pos;
    break;
  }
  case '.': {
    if (nextChar == '.' && nnChar == '.') {
      pos += 3;
      type = tok::ellipsis;
    } else {
      ++pos;
      type = tok::period;
    }
    break;
  }
  case '&': {
    if (nextChar == '&') {
      pos += 2;
      type = tok::amp_amp;
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::amp_equal;
    } else {
      ++pos;
      type = tok::amp;
    }
    break;
  }
  case '*': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::star_equal;
    } else {
      ++pos;
      type = tok::star;
    }
    break;
  }
  case '+': {
    if (nextChar == '+') {
      pos += 2;
      type = tok::plus_plus;
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::plus_equal;
    } else {
      ++pos;
      type = tok::plus;
    }
    break;
  }
  case '-': {
    if (nextChar == '>') {
      pos += 2;
      type = tok::arrow;
    } else if (nextChar == '-') {
      pos += 2;
      type = tok::minus_minus;
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::minus_equal;
    } else {
      ++pos;
      type = tok::minus;
    }
    break;
  }
  case '~': {
    ++pos;
    type = tok::tilde;
    break;
  }
  case '!': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::exclaim_equal;
    } else {
      ++pos;
      type = tok::exclaim;
    }
    break;
  }
  case '/': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::slash_equal;
    } else {
      ++pos;
      type = tok::slash;
    }
    break;
  }
  case '%': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::percent_equal;
    } else {
      ++pos;
      type = tok::percent;
    }
    break;
  }
  case '<': {
    if (nextChar == '<') {
      if (nnChar == '=') {
        pos += 3;
        type = tok::less_less_equal;
      } else {
        pos += 2;
        type = tok::less_less;
      }
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::less_equal;
    } else {
      ++pos;
      type = tok::less;
    }
    break;
  }
  case '>': {
    if (nextChar == '>') {
      if (nnChar == '=') {
        pos += 3;
        type = tok::greater_greater_equal;
      } else {
        pos += 2;
        type = tok::greater_greater;
      }
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::greater_equal;
    } else {
      ++pos;
      type = tok::greater;
    }
    break;
  }
  case '^': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::caret_equal;
    } else {
      ++pos;
      type = tok::caret;
    }
    break;
  }
  case '|': {
    if (nextChar == '|') {
      pos += 2;
      type = tok::pipe_pipe;
    } else if (nextChar == '=') {
      pos += 2;
      type = tok::pipe_equal;
    } else {
      type = tok::pipe;
    }
    break;
  }
  case '?': {
    ++pos;
    type = tok::question;
    break;
  }
  case ':': {
    ++pos;
    type = tok::colon;
    break;
  }
  case ';': {
    ++pos;
    type = tok::semi;
    break;
  }
  case ',': {
    ++pos;
    type = tok::comma;
    break;
  }
  case '=': {
    if (nextChar == '=') {
      pos += 2;
      type = tok::equal_equal;
    } else {
      ++pos;
      type = tok::equal;
    }
    break;
  }
  case '#': {
    if (nextChar == '#') {
      pos += 2;
      type = tok::pp_hashhash;
    } else {
      ++pos;
      type = tok::pp_hash;
    }
    break;
  }
  }
  return type;
}

tok::TokenKind GetKeywordTokenType(const std::string &characters) {
  static std::unordered_map<std::string, tok::TokenKind> hashTable = {
      {"auto", tok::kw_auto},
      {"double", tok::kw_double},
      {"int", tok::kw_int},
      {"struct", tok::kw_struct},
      {"break", tok::kw_break},
      {"else", tok::kw_else},
      {"long", tok::kw_long},
      {"switch", tok::kw_switch},
      {"case", tok::kw_case},
      {"enum", tok::kw_enum},
      {"register", tok::kw_register},
      {"typedef", tok::kw_typedef},
      {"char", tok::kw_char},
      {"extern", tok::kw_extern},
      {"return", tok::kw_return},
      {"union", tok::kw_union},
      {"const", tok::kw_const},
      {"float", tok::kw_float},
      {"short", tok::kw_short},
      {"unsigned", tok::kw_unsigned},
      {"continue", tok::kw_continue},
      {"for", tok::kw_for},
      {"signed", tok::kw_signed},
      {"default", tok::kw_default},
      {"goto", tok::kw_goto},
      {"sizeof", tok::kw_sizeof},
      {"volatile", tok::kw_volatile},
      {"do", tok::kw_do},
      {"if", tok::kw_if},
      {"static", tok::kw_static},
      {"while", tok::kw_while},
      {"void", tok::kw_void},
      {"restrict", tok::kw_restrict},
      {"inline", tok::kw_inline}};
  if (hashTable.find(characters) != hashTable.end()) {
    return hashTable[characters];
  }
  return tok::identifier;
}

PPTokens tokenize(std::string &sourceCode, std::string_view sourcePath) {

  /// check BOM header
  constexpr static std::string_view UTF8_BOM = "\xef\xbb\xbf";
  if (sourceCode.size() >= 3 && sourceCode.substr(0, 3) == UTF8_BOM) {
    sourceCode = sourceCode.substr(3);
  }

  /// compatible with windows
  {
    std::string::size_type pos = 0;
    while ((pos = sourceCode.find("\r\n", pos)) != sourceCode.npos) {
      sourceCode.erase(pos, 1);
    }
  }
  sourceCode.shrink_to_fit();

  /// calculate start offset per line
  uint32_t offset = 0;
  std::vector<uint32_t> lineStartOffset = {offset};
  for (auto &ch : sourceCode) {
    offset++;
    if (ch == '\n') {
      lineStartOffset.push_back(offset);
    }
  }
  lineStartOffset.push_back(offset + 1);
  lineStartOffset.shrink_to_fit();

  /// variables used each time
  std::vector<PPToken> results;
  offset = 0;
  uint32_t tokenStartOffset = 0;
  bool leadingWhiteSpace = false;

  char delimiter;
  std::string characters;
  State state = State::Start;

  auto InsertToken = [&](uint32_t start, uint32_t end, tok::TokenKind tokenKind,
                         std::string value = {}) {
    auto &newToken = results.emplace_back(tokenKind, start, end - start, 0, 0,
                                          std::move(value));
    newToken.setLeadingWhitespace(leadingWhiteSpace);
    leadingWhiteSpace = false;
    characters.clear();
  };

  while (offset < sourceCode.size()) {
    char curChar = (offset < sourceCode.size() ? sourceCode[offset] : ' ');
    std::string debugStr = sourceCode.substr(offset);
    char nextChar =
        (offset < sourceCode.size() - 1) ? sourceCode[offset + 1] : '\0';

    switch (state) {
    case State::Start: {
      if (IsLetter(curChar)) {
        state = State::Identifier;
        tokenStartOffset = offset;
        break;
      }
      if (IsDigit(curChar) || (curChar == '.' && IsDigit(nextChar))) {
        state = State::Number;
        tokenStartOffset = offset;
        break;
      }
      if (curChar == '\'') {
        state = State::CharacterLiteral;
        tokenStartOffset = offset++;
        break;
      }
      if (curChar == '"') {
        if (results.size() >= 2 &&
            results[results.size() - 2].getTokenKind() == tok::pp_hash &&
            results[results.size() - 1].getTokenKind() == tok::identifier &&
            results[results.size() - 1].getValue() == "include") {
          state = State::AfterInclude;
          delimiter = '"';
          tokenStartOffset = offset++;
        } else {
          state = State::StringLiteral;
          tokenStartOffset = offset++;
        }
        break;
      }
      if (curChar == '\\') {
        InsertToken(offset, offset + 1, tok::pp_backslash);
        offset++;
        break;
      }
      if (curChar == '\n') {
        InsertToken(offset, offset + 1, tok::pp_newline);
        offset++;
        break;
      }
      if (curChar == '/' && nextChar == '/') {
        state = State::LineComment;
        offset += 2;
        break;
      }
      if (curChar == '/' && nextChar == '*') {
        state = State::BlockComment;
        offset += 2;
        break;
      }
      /// Line comments and block comments need to be processed first
      if (IsPunctuation(curChar)) {
        if (curChar == '<' && results.size() >= 2 &&
            results[results.size() - 2].getTokenKind() == tok::pp_hash &&
            results[results.size() - 1].getTokenKind() == tok::identifier &&
            results[results.size() - 1].getValue() == "include") {
          state = State::AfterInclude;
          delimiter = '>';
          tokenStartOffset = offset++;
        } else {
          state = State::Punctuator;
          tokenStartOffset = offset;
        }
        break;
      }
      /// last process
      if (IsWhiteSpace(curChar)) {
        leadingWhiteSpace = true;
        offset++;
        break;
      }
      LCC_ASSERT(0 && "illegal ch");
      break;
    }
    case State::CharacterLiteral: {
      if (curChar == '\'' &&
          (characters.empty() || !characters.ends_with('\\'))) {
        state = State::Start;
        InsertToken(tokenStartOffset, offset, tok::char_constant, characters);
      } else {
        characters += curChar;
      }
      offset++;
      break;
    }
    case State::StringLiteral: {
      if (curChar == '"' &&
          (characters.empty() || !characters.ends_with('\\'))) {
        state = State::Start;
        InsertToken(tokenStartOffset, offset, tok::string_literal, characters);
      } else {
        characters += curChar;
      }
      offset++;
      break;
    }
    case State::Identifier: {
      if (IsLetter(curChar) || IsDigit(curChar)) {
        characters += curChar;
        offset++;
      } else {
        state = State::Start;
        InsertToken(tokenStartOffset, offset, tok::identifier, characters);
      }
      break;
    }
    case State::Number: {
      constexpr std::uint8_t toLower = 32;
      if (characters.empty()) {
        characters += curChar;
        offset++;
      } else {
        if ((curChar != 'e' && curChar != 'E') &&
            (curChar != 'p' && curChar != 'P') &&
            (curChar != 'f' && curChar != 'F') &&
            (curChar != 'u' && curChar != 'U') &&
            (curChar != 'l' && curChar != 'L') && !IsDigit(curChar) &&
            (curChar != '.') &&
            (((characters.back() | toLower) != 'e' &&
              (characters.back() | toLower) != 'p') ||
             (curChar != '+' && curChar != '-'))) {
          InsertToken(tokenStartOffset, offset, tok::pp_number, characters);
          state = State::Start;
        } else {
          characters += curChar;
          offset++;
        }
        break;
      }
      break;
    }
    case State::Punctuator: {
      char nnChar =
          (offset < sourceCode.size() - 2) ? sourceCode[offset + 2] : '\0';
      tok::TokenKind tk = ParsePunctuation(offset, curChar, nextChar, nnChar);
      LCC_ASSERT(tk != tok::unknown);
      InsertToken(tokenStartOffset, offset, tk);
      state = State::Start;
      break;
    }
    case State::LineComment: {
      if (curChar == '\n') {
        state = State::Start;
      } else {
        offset++;
      }
      break;
    }
    case State::BlockComment: {
      if (curChar == '*' && nextChar == '/') {
        state = State::Start;
        leadingWhiteSpace = true;
        offset += 2;
      } else {
        offset++;
      }
      break;
    }
    case State::AfterInclude: {
      if (curChar != delimiter && curChar != '\n') {
        characters += curChar;
        offset++;
        break;
      }
      /// curChar is delimiter
      if (curChar != '\n') {
        InsertToken(tokenStartOffset, offset++, tok::string_literal,
                    characters);
        state = State::Start;
        break;
      }
      LCC_ASSERT(0);
    }
    }
  }
  return PPTokens(std::move(results),
                  {Source::File{std::string(sourcePath), std::move(sourceCode),
                                std::move(lineStartOffset)}});
}
} // namespace lcc
