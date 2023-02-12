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
#include <algorithm>
#include <charconv> // std::from_chars
#include <set>
#include <limits>

namespace lcc {

Lexer::Lexer(std::string &sourceCode, std::string_view sourcePath, LanguageOption option)
: mLangOption(option) {
  RegularSourceCode(sourceCode);
  /// calculate start offset per line
  uint32_t offset = 0;
  auto &offsets = mSourceFile.lineStartOffsets;
  offsets.push_back(offset);
  for (auto &ch : sourceCode) {
    offset++;
    if (ch == '\n') {
      offsets.push_back(offset);
    }
  }
  offsets.push_back(offset + 1);
  offsets.shrink_to_fit();

  mSourceFile.sourceCode = sourceCode;
  mSourceFile.sourcePath = sourcePath;
}

void Lexer::RegularSourceCode(std::string &sourceCode) {
  /// check BOM header
  std::string_view UTF8_BOM = "\xef\xbb\xbf";
  if (sourceCode.size() >= 3 && sourceCode.substr(0, 3) == UTF8_BOM) {
    sourceCode = sourceCode.substr(3);
  }
  /// compatible with windows
  std::string::size_type pos = 0;
  while ((pos = sourceCode.find("\r\n", pos)) != sourceCode.npos) {
    sourceCode.erase(pos, 1);
  }

  sourceCode.shrink_to_fit();
}

Token::ValueType Lexer::ParseStringLiteral(const Token &ppToken) {
  auto chars = ProcessCharacters(ppToken.getContent());
  if (!chars) {
    LOGE(ppToken.getLine(), ppToken.getColumn(), "parse string literal error");
  }
  return std::string(chars.value().begin(), chars.value().end());
}

/**
整型
10进制：123 123u 123l 123ul 123lu 123ull 123llu
8进制: 0123 0123u 0123l
16进制: 0x123 0xff 0xffull
int a = 123lu;
int b = 0123lu;
int c = 0x123lu;

浮点型
10进制: .123 .123f .123F .123l .123L .123e0 .123e+1 .123e-3
16进制: 0x.123 0x.123f 0x.123p0 0x.123p+1 0x.123p-3
16进制: 0x.ff 0x.ffp-3
float a1 = 0x.ffp-3;
 */
Token::ValueType Lexer::ParseNumber(const Token &ppToken) {
  std::string content = ppToken.getContent();
  std::string_view character = content;
  const char *begin = character.begin(), *end = character.end();
  LCC_ASSERT(std::distance(begin, end) >= 1);
  /// If the number is just "0x", treat the x as a suffix instead of as a hex
  /// prefix
  bool isHex = character.size() > 2 &&
               (character.starts_with("0x") || character.starts_with("0X")) &&
                (IsHexDigit(character[2]) || character[2] == '.');
  std::vector<char> charSet{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
  if (isHex) {
    charSet = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a',
               'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F'};
  }
  charSet.shrink_to_fit();
  bool isFloat = false;
  auto searchFunction = [&charSet, &isFloat](char c) mutable {
    if (c == '.' && !isFloat) {
      isFloat = true;
      return false;
    }
    return std::find(charSet.begin(), charSet.end(), c) == charSet.end();
  };
  const auto *suffixBegin =
      std::find_if(begin + (isHex ? 2 : 0), end, searchFunction);
  // If it's a float it might still have an exponent part. If it's non hex this
  // is e [optional + or -] then again followed by digits. If it's a hex then
  // its p [optional + or -]. We check if it's either an then continue our
  // search
  constexpr unsigned toLower = 32;
  if (suffixBegin != end && (*suffixBegin | toLower) == (isHex ? 'p' : 'e')) {
    isFloat = true;
    suffixBegin++;
    if (suffixBegin != end && (*suffixBegin == '+' || *suffixBegin == '-')) {
      suffixBegin++;
    }
    const auto *prev = suffixBegin;
    /// The exponent of a hex floating point number is actually normal decimal
    /// digits not hex
    if (isHex) {
      charSet.resize(10);
    }
    suffixBegin = std::find_if(suffixBegin, end, searchFunction);
    /// first character must be digit
    if (prev == suffixBegin) {
      LOGE(ppToken.getLine(), ppToken.getColumn(), "expected digits after exponent");
    }
  } else if (isHex && isFloat) {
    LOGE(ppToken.getLine(), ppToken.getColumn(), "binary floating point must contain exponent");
  }

  bool isHexOrOctal = isHex;
  if (!isHex && !isFloat && begin[0] == '0') {
    isHexOrOctal = true;
    size_t len = std::distance(begin, suffixBegin);
    for (int i = 0; i < len; ++i) {
      if (character[i] >= '8') {
        LOGE(ppToken.getLine(), ppToken.getColumn(), "invalid octal character");
      }
    }
  }

  auto suffix = std::string_view(suffixBegin, std::distance(suffixBegin, end));
  bool valid;
  if (!isFloat) {
    static std::set<std::string_view> variants = {"u",   "U",   "ul",  "Ul",  "uL",  "UL",
                                     "uLL", "ULL", "ull", "Ull", "lu",  "lU",
                                     "Lu",  "LU",  "LLu", "LLU", "llu", "llU",
                                     "l",   "L",   "ll",  "LL",  ""};
    valid = variants.find(suffix) != variants.end();
  } else {
    static std::set<std::string_view> variants = {"f", "l", "F", "L", ""};
    valid = variants.find(suffix) != variants.end();
  }
  if (!valid) {
    LOGE(ppToken.getLine(), ppToken.getColumn(), "invalid literal suffix");
  }

  if (!isFloat) {
    bool unsignedConsidered =
        isHexOrOctal || std::any_of(suffix.begin(), suffix.end(), [](char c) {
          return c == 'u' || c == 'U';
        });
    std::string_view sv(begin, suffixBegin - begin);
    std::uint64_t number = std::strtoull(sv.data(), nullptr, 0);
    if (suffix.empty()) {
      if (number > static_cast<uint64_t>(std::numeric_limits<int32_t>::max())) {
        if (isHexOrOctal && number <= std::numeric_limits<uint32_t>::max()) {
          return static_cast<uint32_t>(number);
        }else if (isHexOrOctal && number > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
          return number;
        }else {
          return static_cast<int64_t>(number);
        }
      }else {
        return static_cast<int32_t>(number);
      }
    } else if (suffix == "u" || suffix == "U") {
      if (number > std::numeric_limits<uint32_t>::max()) {
        return number;
      }
      return static_cast<uint32_t>(number);
    } else if (suffix == "L" || suffix == "l") {
      /// think about long case
      if (isHexOrOctal) {
        if (number > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
          return number;
        }
      }
      return static_cast<int64_t>(number);
    } else if (suffix.size() == 2 &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'u' || c == 'U'; }) &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'l' || c == 'L'; })) {
      /// just think about ul
      return static_cast<uint64_t>(number);
    } else if (suffix == "ll" || suffix == "LL") {
      if (isHexOrOctal) {
        if (number > static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
          return number;
        }
      }
      return static_cast<int64_t>(number);
    } else if (suffix.size() == 3 &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'u' || c == 'U'; }) &&
               (suffix.find("LL") != std::string_view::npos ||
                suffix.find("ll") != std::string_view::npos)) {
      /// just think about ull
      return number;
    } else {
      LCC_UNREACHABLE;
    }
  } else {
    auto input = (*begin == '.' ? "0" : "") + std::string(begin, suffixBegin);
    if (suffix.empty()) {
      return std::strtod(input.c_str(), nullptr);
    }
    if (suffix == "f" || suffix == "F") {
      return std::strtof(input.c_str(), nullptr);
    }
    if (suffix == "l" || suffix == "L") {
      return std::strtod(input.c_str(), nullptr);
    }
  }
  LCC_UNREACHABLE;
}

tok::TokenKind Lexer::ParsePunctuation(uint32_t & offset, char curChar, char nextChar,
                                char nnChar) {
  tok::TokenKind type = tok::unknown;
  switch (curChar) {
  case '[': {
    type = tok::l_square;
    ++offset;
    break;
  }
  case ']': {
    type = tok::r_square;
    ++offset;
    break;
  }
  case '(': {
    type = tok::l_paren;
    ++offset;
    break;
  }
  case ')': {
    type = tok::r_paren;
    ++offset;
    break;
  }
  case '{': {
    type = tok::l_brace;
    ++offset;
    break;
  }
  case '}': {
    type = tok::r_brace;
    ++offset;
    break;
  }
  case '.': {
    if (nextChar == '.' && nnChar == '.') {
      offset += 3;
      type = tok::ellipsis;
    } else {
      ++offset;
      type = tok::period;
    }
    break;
  }
  case '&': {
    if (nextChar == '&') {
      offset += 2;
      type = tok::amp_amp;
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::amp_equal;
    } else {
      ++offset;
      type = tok::amp;
    }
    break;
  }
  case '*': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::star_equal;
    } else {
      ++offset;
      type = tok::star;
    }
    break;
  }
  case '+': {
    if (nextChar == '+') {
      offset += 2;
      type = tok::plus_plus;
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::plus_equal;
    } else {
      ++offset;
      type = tok::plus;
    }
    break;
  }
  case '-': {
    if (nextChar == '>') {
      offset += 2;
      type = tok::arrow;
    } else if (nextChar == '-') {
      offset += 2;
      type = tok::minus_minus;
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::minus_equal;
    } else {
      ++offset;
      type = tok::minus;
    }
    break;
  }
  case '~': {
    ++offset;
    type = tok::tilde;
    break;
  }
  case '!': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::exclaim_equal;
    } else {
      ++offset;
      type = tok::exclaim;
    }
    break;
  }
  case '/': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::slash_equal;
    } else {
      ++offset;
      type = tok::slash;
    }
    break;
  }
  case '%': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::percent_equal;
    } else {
      ++offset;
      type = tok::percent;
    }
    break;
  }
  case '<': {
    if (nextChar == '<') {
      if (nnChar == '=') {
        offset += 3;
        type = tok::less_less_equal;
      } else {
        offset += 2;
        type = tok::less_less;
      }
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::less_equal;
    } else {
      ++offset;
      type = tok::less;
    }
    break;
  }
  case '>': {
    if (nextChar == '>') {
      if (nnChar == '=') {
        offset += 3;
        type = tok::greater_greater_equal;
      } else {
        offset += 2;
        type = tok::greater_greater;
      }
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::greater_equal;
    } else {
      ++offset;
      type = tok::greater;
    }
    break;
  }
  case '^': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::caret_equal;
    } else {
      ++offset;
      type = tok::caret;
    }
    break;
  }
  case '|': {
    if (nextChar == '|') {
      offset += 2;
      type = tok::pipe_pipe;
    } else if (nextChar == '=') {
      offset += 2;
      type = tok::pipe_equal;
    } else {
      type = tok::pipe;
      ++offset;
    }
    break;
  }
  case '?': {
    ++offset;
    type = tok::question;
    break;
  }
  case ':': {
    ++offset;
    type = tok::colon;
    break;
  }
  case ';': {
    ++offset;
    type = tok::semi;
    break;
  }
  case ',': {
    ++offset;
    type = tok::comma;
    break;
  }
  case '=': {
    if (nextChar == '=') {
      offset += 2;
      type = tok::equal_equal;
    } else {
      ++offset;
      type = tok::equal;
    }
    break;
  }
  case '#': {
    if (nextChar == '#') {
      offset += 2;
      type = tok::pp_hashhash;
    } else {
      ++offset;
      type = tok::pp_hash;
    }
    break;
  }
  default:
    return type;
  }
  return type;
}
std::vector<Token> Lexer::tokenize() {
  /// variables used each time
  std::vector<Token> results;

  uint32_t offset = 0;

  auto InsertToken = [&](uint32_t startOffset, uint32_t endOffset, tok::TokenKind tokenKind,
                         std::string value = {}) {
    auto &newToken = results.emplace_back(tokenKind, startOffset, endOffset - startOffset, mSourceFile,
                                          std::move(value));
    newToken.setLeadingWhitespace(leadingWhiteSpace);
    leadingWhiteSpace = false;
    characters.clear();
  };

  std::string_view sourceCode = mSourceFile.sourceCode;
  while (offset < sourceCode.size()) {
    char curChar = (offset < sourceCode.size() ? sourceCode[offset] : '\0');
    char nextChar =
        (offset < sourceCode.size() - 1) ? sourceCode[offset + 1] : '\0';

    switch (state) {
    case State::Start: {
      if (IsLetter(curChar)) {
        state = State::Identifier;
        tokenStartOffset = offset;
        break;
      }
      if (IsDigit(curChar) ||
          (curChar == '.' && IsDigit(nextChar))) {
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
            results[results.size() - 1].getStrTokName() == "include") {
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
            results[results.size() - 1].getContent() == "include") {
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
      LOGE(GetLine(tokenStartOffset), GetColumn(tokenStartOffset), "illegal char");
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
      if (IsLetter(curChar) ||  IsDigit(curChar)) {
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
        char lower_char = (curChar | toLower);
        if (!IsJudgeNumber(characters, lower_char) &&
            (lower_char != 'e') &&
            (lower_char != 'p') &&
            (lower_char != 'f') &&
            (lower_char != 'u') &&
            (lower_char != 'l') &&
            (lower_char != '.') &&
            (((characters.back() | toLower) != 'e' &&
              (characters.back() | toLower) != 'p') ||
             (lower_char != '+' && lower_char != '-'))) {
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
      InsertToken(tokenStartOffset, offset, tk, tok::getPunctuatorSpelling(tk));
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
      LOGE(GetLine(tokenStartOffset), GetColumn(tokenStartOffset), "illegal newline in after include");
    }
    }
  }
  results.shrink_to_fit();

  if (mLangOption == LanguageOption::C) {
    return toCTokens(std::move(results));
  }

  return results;
}

std::vector<Token> Lexer::toCTokens(std::vector<Token>&& ppTokens) {
  std::vector<Token> results;
  for (auto &iter : ppTokens) {
    switch (iter.getTokenKind()) {
    case tok::pp_hash:
    case tok::pp_hashhash:
    case tok::pp_backslash:
      LOGE(iter.getLine(), iter.getColumn(), "illegal token kind in c token");
      break;
    case tok::pp_newline:
      break;
    case tok::identifier: {
      iter.setTokenKind(tok::getKeywordTokenType(iter.getContent()));
      results.push_back(iter);
      break;
    }
    case tok::pp_number: {
      auto number = ParseNumber(iter);
      iter.setTokenKind(tok::numeric_constant);
      iter.setValue(number);
      results.push_back(iter);
      break;
    }
    case tok::string_literal: {
      auto str = ParseStringLiteral(iter);
      iter.setTokenKind(tok::string_literal);
      iter.setValue(str);
      results.push_back(iter);
      break;
    }
    case tok::char_constant: {
      auto chars = ProcessCharacters(iter.getContent());
      if (!chars || chars.value().empty() || chars.value().size() > 1) {
        LOGE(iter.getLine(), iter.getLine(), "process char constant error");
        break;
      }
      iter.setTokenKind(tok::char_constant);
      iter.setValue((int32_t)chars.value()[0]);
      results.push_back(iter);
      break;
    }
    default:
      iter.setValue(tok::getPunctuatorSpelling(iter.getTokenKind()));
      results.push_back(iter);
    }
  }
  results.shrink_to_fit();
  return results;
}

bool Lexer::IsLetter(char ch) {
  if (ch == '_') {
    return true;
  }
  if (ch >= 'a' && ch <= 'z') {
    return true;
  }
  if (ch >= 'A' && ch <= 'Z') {
    return true;
  }
  return false;
}

bool Lexer::IsWhiteSpace(char ch) {
  return (ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' || ch == '\f' ||
          ch == '\v');
}

bool Lexer::IsDigit(char ch) { return ch >= '0' && ch <= '9'; }

bool Lexer::IsOctDigit(char ch) {return ch >= '0' && ch <= '7';}

bool Lexer::IsHexDigit(char ch) {
  return IsDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
}

bool Lexer::IsPunctuation(char ch) {
  return ch == '[' || ch == ']' || ch == '(' || ch == ')' || ch == '{' ||
         ch == '}' || ch == '.' || ch == '&' || ch == '*' || ch == '+' ||
         ch == '-' || ch == '~' || ch == '!' || ch == '/' || ch == '%' ||
         ch == '<' || ch == '>' || ch == '^' || ch == '|' || ch == '?' ||
         ch == ':' || ch == ';' || ch == '=' || ch == ',' || ch == '#';
}

uint32_t Lexer::GetLine(uint32_t offset) const {
  auto result = std::lower_bound(mSourceFile.lineStartOffsets.begin(), mSourceFile.lineStartOffsets.end(), offset);
  return std::distance(mSourceFile.lineStartOffsets.begin(), result) + ((*result == offset) ? 1 : 0);
}

uint32_t Lexer::GetColumn(uint32_t offset) const {
  uint32_t line = GetLine(offset);
  return offset - mSourceFile.lineStartOffsets[line - 1] + 1;
}

uint32_t Lexer::OctalToNum(std::string_view value)
{
  std::uint32_t result;
  auto errors = std::from_chars(value.data(), value.data() + value.size(), result, 8);
  LCC_ASSERT(errors.ec == std::errc{});
  return result;
}

std::optional<std::uint32_t> Lexer::EscapeCharToValue(char escape) {
  switch (escape) {
  case '\\':
    return '\\';
  case '\'':
    return '\'';
  case '"':
    return '"';
  case 'a':
    return '\a';
  case 'b':
    return '\b';
  case 'f':
    return '\f';
  case 'n':
    return '\n';
  case 'r':
    return '\r';
  case 't':
    return '\t';
  case 'v':
    return '\v';
  case '?':
    return '\?';
  default:
    return {};
  }
  return 0;
}

std::optional<std::vector<char>> Lexer::ProcessCharacters(std::string_view characters) {
  std::vector<char> result;
  result.reserve(characters.size());
  int offset = 0, resultStart = 0;
  while (offset < characters.size()) {
    char ch = characters[offset];
    if (ch == '\n') {
      return {};
    }
    if (ch != '\\') {
      result.push_back(ch);
      resultStart++;
      offset++;
      continue;
    }
    if (offset + 1 == characters.size()) {
      break;
    }
    if (characters[offset+1] == 'x') {
      offset += 2;
      int lastHex = offset;
      while (lastHex < characters.size()) {
        if (IsHexDigit(characters[lastHex])) {
          lastHex++;
          continue;
        }
        break;
      }
      if (offset == lastHex) {
        /// error at least one hexadecimal digit required
        return {};
      }
      std::string_view sv(characters.data()+offset, lastHex-offset);
      auto value = std::strtol(sv.data(), nullptr, 16);
      result.push_back((char)value);
      resultStart++;
      offset = lastHex;
      break;
    }
    /// '\0' is octal char
    else if ( IsDigit(characters[offset+1])) {
      offset++;
      int start = offset;
      /// first octal char must be oct char
      if (! IsOctDigit(characters[offset])) {
        return {};
      }
      /// first octal char
      if ( IsOctDigit(characters[offset])) {
        offset++;
      }
      /// second octal char
      if ( IsOctDigit(characters[offset])) {
        offset++;
      }
      /// third octal char
      if ( IsOctDigit(characters[offset])) {
        offset++;
      }
      auto value = OctalToNum(characters.substr(start, offset-start));
      result.push_back((char)value);
      resultStart++;
      break;
    }else {
      auto character = EscapeCharToValue(characters[offset + 1]);
      if (!character) {
        return {};
      }
      result.push_back((char)character.value());
      resultStart++;
      offset += 2;
    }
  }
  result.resize(resultStart);
  return result;
}

bool Lexer::IsJudgeNumber(const std::string &preCharacters, char curChar) {
  if (preCharacters.size() == 1 && preCharacters.back() == '0' && (curChar == 'x'|| curChar == 'X')) {
    return true;
  }

  if (preCharacters.starts_with("0x") || preCharacters.starts_with("0X")) {
    return IsHexDigit(curChar);
  }

  if (preCharacters.starts_with("0")) {
    return IsOctDigit(curChar);
  }

  return IsDigit(curChar);
}
} // namespace lcc
