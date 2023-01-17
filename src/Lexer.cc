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
#include <cassert>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <charconv>
#include <optional>

#include <llvm/Support/Error.h>

constexpr auto UTF32_MAX = 0x10FFFF;

namespace lcc {
namespace util {
bool IsLetter(char ch) {
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

bool IsWhiteSpace(char ch) {
  return (ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r' || ch == '\f' ||
          ch == '\v');
}

bool IsDigit(char ch) { return ch >= '0' && ch <= '9'; }

bool IsOctDigit(char ch) {return ch >= '0' && ch <= '7';}

bool IsHexDigit(char ch) {
  return IsDigit(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F');
}

bool IsPunctuation(char ch) {
  return ch == '[' || ch == ']' || ch == '(' || ch == ')' || ch == '{' ||
         ch == '}' || ch == '.' || ch == '&' || ch == '*' || ch == '+' ||
         ch == '-' || ch == '~' || ch == '!' || ch == '/' || ch == '%' ||
         ch == '<' || ch == '>' || ch == '^' || ch == '|' || ch == '?' ||
         ch == ':' || ch == ';' || ch == '=' || ch == ',' || ch == '#';
}

std::uint32_t octalToValue(std::string_view value)
{
  std::uint32_t result;
  auto errors = std::from_chars(value.data(), value.data() + value.size(), result, 8);
  LCC_ASSERT(errors.ec == std::errc{});
  return result;
}
} // namespace util

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

std::optional<std::uint32_t> escapeCharToValue(char escape) {
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
  case ' ':
//    LCC_ASSERT(0 && "expected character after backslash");
    return {};
    break;
  default:
//    LCC_ASSERT(0 && "invalid escape sequence");
    return {};
    break;
  }
  return 0;
}

std::optional<std::vector<char>> processCharacters(std::string_view characters) {
  std::vector<char> result;
  result.resize(characters.size());
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
        if (util::IsHexDigit(characters[lastHex])) {
          lastHex++;
          continue;
        }
        break;
      }
      if (offset == lastHex) {
        /// error at least one hexadecimal digit required
        return {};
      }

      llvm::APInt input;
      llvm::StringRef(characters.data()+offset, lastHex-offset).getAsInteger(16, input);
      auto value = input.getZExtValue();
      result.push_back(value);
      resultStart++;
      offset = lastHex;
      break;
    }
    /// '\0' is octal char
    else if (util::IsDigit(characters[offset+1])) {
      offset++;
      int start = offset;
      /// first octal char must be oct char
      if (!util::IsOctDigit(characters[offset])) {
        return {};
      }
      /// first octal char
      if (util::IsOctDigit(characters[offset])) {
        offset++;
      }
      /// second octal char
      if (util::IsOctDigit(characters[offset])) {
        offset++;
      }
      /// third octal char
      if (util::IsOctDigit(characters[offset])) {
        offset++;
      }
      auto value = util::octalToValue(characters.substr(start, offset-start));
      result.push_back((char)value);
      resultStart++;
      break;
    }else {
      auto character = escapeCharToValue(characters[offset + 1]);
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

CToken ParseStringLiteral(const PPToken &ppToken, const SourceInterface &interface) {
  auto chars = processCharacters(ppToken.getValue());
  if (!chars) {
    logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "parse string literal error");
  }
  return CToken(tok::string_literal, ppToken.getOffset(), ppToken.getLength(),
                ppToken.getFileId(), ppToken.getMacroId(),
                std::string(chars.value().begin(), chars.value().end()));
}

template <class T, class... Args>
std::pair<CToken::ValueType, CToken::NumType>
castInteger(std::uint64_t integer,
            std::array<CToken::NumType, sizeof...(Args) + 1> types) {
  // Clang
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
  // GCC
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"
  // MSVC
#pragma warning(push)
#pragma warning(disable : 4018)
#pragma warning(disable : 4389)
  if constexpr (std::is_signed_v<T>) {
    if (llvm::APInt::getSignedMaxValue(sizeof(T) * 8).uge(integer)) {
      return {{llvm::APSInt(llvm::APInt(sizeof(T) * 8, integer),
                            !std::is_signed_v<T>)},
              types[0]};
    }
  } else {
    if (llvm::APInt::getMaxValue(sizeof(T) * 8).uge(integer)) {
      return {{llvm::APSInt(llvm::APInt(sizeof(T) * 8, integer),
                            !std::is_signed_v<T>)},
              types[0]};
    }
  }
  if constexpr (sizeof...(Args) != 0) {
    std::array<CToken::NumType, sizeof...(Args)> second{};
    std::copy(types.begin() + 1, types.end(), second.begin());
    return castInteger<Args...>(integer, second);
  } else {
    LCC_UNREACHABLE;
  }
#pragma GCC diagnostic pop
#pragma clang diagnostic pop
#pragma warning(pop)
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
std::pair<CToken::ValueType, CToken::NumType>
ParseNumber(const PPToken &ppToken, const SourceInterface &interface) {
  std::string_view character = ppToken.getValue();
  const char *begin = character.begin(), *end = character.end();
  LCC_ASSERT(std::distance(begin, end) >= 1);
  /// If the number is just "0x", treat the x as a suffix instead of as a hex
  /// prefix
  bool isHex = character.size() > 2 &&
               (character.starts_with("0x") || character.starts_with("0X")) &&
               util::IsHexDigit(character[2]);
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
      logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "expected digits after exponent");
    }
  } else if (isHex && isFloat) {
    logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "binary floating point must contain exponent");
  }

  bool isHexOrOctal = isHex;
  if (!isHex && !isFloat && begin[0] == '0') {
    isHexOrOctal = true;
    size_t len = std::distance(begin, suffixBegin);
    for (int i = 0; i < len; ++i) {
      if (character[i] >= '8') {
        logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "invalid octal character");
      }
    }
  }

  auto suffix = std::string_view(suffixBegin, std::distance(suffixBegin, end));
  bool valid;
  if (!isFloat) {
    constexpr std::array variants = {"u",   "U",   "ul",  "Ul",  "uL",  "UL",
                                     "uLL", "ULL", "ull", "Ull", "lu",  "lU",
                                     "Lu",  "LU",  "LLu", "LLU", "llu", "llU",
                                     "l",   "L",   "ll",  "LL",  ""};
    valid =
        std::find(variants.begin(), variants.end(), suffix) != variants.end();
  } else {
    constexpr std::array variants = {"f", "l", "F", "L", ""};
    valid =
        std::find(variants.begin(), variants.end(), suffix) != variants.end();
  }
  if (!valid) {
    logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "invalid literal suffix");
  }

  if (!isFloat) {
    bool unsignedConsidered =
        isHexOrOctal || std::any_of(suffix.begin(), suffix.end(), [](char c) {
          return c == 'u' || c == 'U';
        });
    llvm::APInt test;
    llvm::StringRef(begin, suffixBegin - begin).getAsInteger(0, test);
    if (test.getActiveBits() > (unsignedConsidered ? 64u : 63u)) {
      logErr(ppToken.getLine(interface), ppToken.getColumn(interface), "integer value too big to be representable");
    }
    auto integer = test.getZExtValue();
    if (suffix.empty()) {
      if (isHexOrOctal) {
        /// think about all case
        return castInteger<std::int32_t, std::uint32_t, std::int64_t,
                           std::uint64_t>(
            integer, {CToken::NumType::Int, CToken::NumType::UnsignedInt,
                      CToken::NumType::Long, CToken::NumType::UnsignedLong});
      }
      return castInteger<std::int32_t, std::int64_t>(
          integer, {CToken::NumType::Int, CToken::NumType::Long});
    } else if (suffix == "u" || suffix == "U") {
      /// think about unsigned case
      return castInteger<std::uint32_t, std::uint64_t>(
          integer,
          {CToken::NumType::UnsignedInt, CToken::NumType::UnsignedLong});
    } else if (suffix == "L" || suffix == "l") {
      /// think about long case
      if (isHexOrOctal) {
        return castInteger<std::int64_t, std::uint64_t>(
            integer, {CToken::NumType::Long, CToken::NumType::UnsignedLong});
      }
      return castInteger<std::int64_t>(integer, {CToken::NumType::Long});
    } else if (suffix.size() == 2 &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'u' || c == 'U'; }) &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'l' || c == 'L'; })) {
      /// just think about ul
      return castInteger<std::uint64_t>(integer,
                                        {CToken::NumType::UnsignedLong});
    } else if (suffix == "ll" || suffix == "LL") {
      if (isHexOrOctal) {
        return castInteger<std::int64_t, std::uint64_t>(
            integer,
            {CToken::NumType::LongLong, CToken::NumType::UnsignedLongLong});
      }
      /// just think about ll
      return castInteger<std::int64_t>(integer, {CToken::NumType::LongLong});
    } else if (suffix.size() == 3 &&
               std::any_of(suffix.begin(), suffix.end(),
                           [](char c) { return c == 'u' || c == 'U'; }) &&
               (suffix.find("LL") != std::string_view::npos ||
                suffix.find("ll") != std::string_view::npos)) {
      /// just think about ull
      return castInteger<std::uint64_t>(integer,
                                        {CToken::NumType::UnsignedLongLong});
    } else {
      LCC_UNREACHABLE;
    }
  } else {
    auto input = (*begin == '.' ? "0" : "") + std::string(begin, suffixBegin);
    if (suffix.empty()) {
      llvm::APFloat number(llvm::APFloat::IEEEdouble());
      auto result =
          number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
      if (!result) {
        LCC_UNREACHABLE;
      }
      return {std::move(number), CToken::NumType::Double};
    }
    if (suffix == "f" || suffix == "F") {
      llvm::APFloat number(llvm::APFloat::IEEEsingle());
      auto result =
          number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
      if (!result) {
        LCC_UNREACHABLE;
      }
      return {std::move(number), CToken::NumType::Float};
    }
    if (suffix == "l" || suffix == "L") {
      llvm::APFloat number(llvm::APFloat::IEEEdouble());
      auto result =
          number.convertFromString(input, llvm::APFloat::rmNearestTiesToEven);
      if (!result) {
        LCC_UNREACHABLE;
      }
      return {std::move(number), CToken::NumType::LongDouble};
    }
  }
  LCC_UNREACHABLE;
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
  default:
    return type;
  }
  return type;
}

tok::TokenKind GetKeywordTokenType(std::string_view characters) {
  static std::unordered_map<std::string_view, tok::TokenKind> hashTable = {
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
      if (util::IsLetter(curChar)) {
        state = State::Identifier;
        tokenStartOffset = offset;
        break;
      }
      if (util::IsDigit(curChar) ||
          (curChar == '.' && util::IsDigit(nextChar))) {
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
      if (util::IsPunctuation(curChar)) {
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
      if (util::IsWhiteSpace(curChar)) {
        leadingWhiteSpace = true;
        offset++;
        break;
      }
      std::cerr << "offset: " << tokenStartOffset
                << ", illegal char: " << curChar << std::endl;
      LCC_ASSERT(0);
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
      if (util::IsLetter(curChar) || util::IsDigit(curChar)) {
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
            (curChar != 'l' && curChar != 'L') && !util::IsDigit(curChar) &&
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
      std::cerr << "offset: " << tokenStartOffset
                << ", illegal newline in after include" << std::endl;
      LCC_ASSERT(0);
    }
    }
  }
  return PPTokens(std::move(results),
                  {Source::File{std::string(sourcePath), std::move(sourceCode),
                                std::move(lineStartOffset)}});
}

CTokens toCTokens(PPTokens &&ppTokens) {
  std::vector<CToken> result;
  for (auto &iter : ppTokens.data()) {
    switch (iter.getTokenKind()) {
    case tok::pp_hash:
    case tok::pp_hashhash:
    case tok::pp_backslash:
      logErr(iter.getLine(ppTokens), iter.getColumn(ppTokens), "illegal token kind in c token");
      break;
    case tok::pp_newline:
      break;
    case tok::identifier: {
      result.emplace_back(GetKeywordTokenType(iter.getValue()),
                          iter.getOffset(), iter.getLength(), iter.getFileId(),
                          iter.getMacroId(), lcc::to_string(iter.getValue()));
      break;
    }
    case tok::pp_number: {
      auto number = ParseNumber(iter, ppTokens);
      result.emplace_back(tok::numeric_constant, iter.getOffset(),
                          iter.getLength(), iter.getFileId(), iter.getMacroId(),
                          std::move(number.first), number.second);
      break;
    }
    case tok::string_literal: {
      auto cToken = ParseStringLiteral(iter, ppTokens);
      result.push_back(std::move(cToken));
      break;
    }
    case tok::char_constant: {
      auto chars = processCharacters(iter.getValue());
      if (!chars) {
        logErr(iter.getLine(ppTokens), iter.getLine(ppTokens), "process char constant error");
        break;
      }
      if (chars.value().empty()) {
        logErr(iter.getLine(ppTokens), iter.getColumn(ppTokens), "character literal cannot be empty");
        break;
      }
      if (chars.value().size() > 1) {
        logErr(iter.getLine(ppTokens), iter.getColumn(ppTokens), "character literal size more than 1");
        break;
      }
      result.emplace_back(
          tok::char_constant, iter.getOffset(), iter.getLength(),
          iter.getFileId(), iter.getMacroId(),
          llvm::APSInt(llvm::APInt(4 * 8, chars.value()[0], true), false), CToken::Int);
      break;
    }
    default:
      result.emplace_back(iter.getTokenKind(), iter.getOffset(),
                          iter.getLength(), iter.getFileId(),
                          iter.getMacroId());
    }
  }
  result.shrink_to_fit();
  return CTokens(std::move(result),
                 {ppTokens.getFiles().begin(), ppTokens.getFiles().end()});
}
} // namespace lcc
