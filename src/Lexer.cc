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

using namespace llvm;

Lexer::Lexer(llvm::SourceMgr &mgr, DiagnosticEngine &diag, std::string &&sourceCode, std::string_view sourcePath, LanguageOption option)
    : Mgr(mgr), Diag(diag),mSourceCode(std::move(sourceCode)),mLangOption(option) {

  RegularSourceCode();
  auto memBuf = MemoryBuffer::getMemBuffer(mSourceCode, sourcePath);
  Mgr.AddNewSourceBuffer(std::move(memBuf), SMLoc());
  auto *m = Mgr.getMemoryBuffer(Mgr.getMainFileID());
  P = m->getBufferStart();
  Ep = m->getBufferEnd();
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
  StringRef character = ppToken.getRepresentation();
  const char *begin = character.begin(), *end = character.end();
  LCC_ASSERT(std::distance(begin, end) >= 1);
  /// If the number is just "0x", treat the x as a suffix instead of as a hex
  /// prefix
  bool isHex = character.size() > 2 &&
               (character.startswith("0x") || character.startswith("0X")) &&
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
      DiagReport(Diag, SMLoc::getFromPointer(ppToken.getOffset()+(suffixBegin-begin)),diag::err_lex_expected_digits_after_exponent);
    }
  } else if (isHex && isFloat) {
      DiagReport(Diag, SMLoc::getFromPointer(ppToken.getOffset()+(suffixBegin-begin)),diag::err_lex_binary_floating);
  }

  bool isHexOrOctal = isHex;
  if (!isHex && !isFloat && begin[0] == '0') {
    isHexOrOctal = true;
    size_t len = std::distance(begin, suffixBegin);
    for (int i = 0; i < len; ++i) {
      if (character[i] >= '8') {
        DiagReport(Diag, SMLoc::getFromPointer(ppToken.getOffset() + i),diag::err_lex_invalid_octal_character);
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
    DiagReport(Diag, SMLoc::getFromPointer(ppToken.getOffset()+(suffixBegin-begin)),diag::err_lex_invalid_literal_suffix);
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

tok::TokenKind Lexer::ParsePunctuation(const char * & offset, char curChar, char nextChar,
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

  std::vector<Token> results;

  /// Sp meaning start p
  const char *Sp = P;
  std::string strBuilder;
  char includeDelimiter{' '};

  auto InsertToken = [&](const char * sp, const char *p, tok::TokenKind tokenKind,
                         std::string value = {}) {
    auto &newToken = results.emplace_back(tokenKind, sp, p - sp, Mgr, std::move(value));
    strBuilder.clear();
  };

  while (P < Ep) {
    char curChar = (P < Ep ? P[0] : '\0');
    char nextChar = (P < Ep - 1) ? P[1] : '\0';

    switch (state) {
    case State::Start: {
      if (IsLetter(curChar)) {
        state = State::Identifier;
        Sp = P;
        break;
      }
      if (IsDigit(curChar) ||
          (curChar == '.' && IsDigit(nextChar))) {
        state = State::Number;
        Sp = P;
        break;
      }
      if (curChar == '\'') {
        state = State::CharacterLiteral;
        Sp = P++;
        break;
      }
      if (curChar == '"') {
        if (results.size() >= 2 &&
            results[results.size() - 2].getTokenKind() == tok::pp_hash &&
            results[results.size() - 1].getTokenKind() == tok::identifier &&
            results[results.size() - 1].getRepresentation() == "include") {
          state = State::AfterInclude;
          includeDelimiter = '"';
          Sp = P++;
        } else {
          state = State::StringLiteral;
          Sp = P++;
        }
        break;
      }
      if (curChar == '\\') {
        InsertToken(Sp, ++P, tok::pp_backslash);
        break;
      }
      /// \r\n meaning \n in windows
      if (curChar == '\r' && nextChar == '\n') {
        InsertToken(Sp+1, P+=2, tok::pp_newline);
        break;
      }
      if (curChar == '\n') {
        InsertToken(Sp, ++P, tok::pp_newline);
        break;
      }
      if (curChar == '/' && nextChar == '/') {
        state = State::LineComment;
        P += 2;
        break;
      }
      if (curChar == '/' && nextChar == '*') {
        state = State::BlockComment;
        P += 2;
        break;
      }
      /// Line comments and block comments need to be processed first
      if (IsPunctuation(curChar)) {
        if (curChar == '<' && results.size() >= 2 &&
            results[results.size() - 2].getTokenKind() == tok::pp_hash &&
            results[results.size() - 1].getTokenKind() == tok::identifier &&
            results[results.size() - 1].getRepresentation() == "include") {
          state = State::AfterInclude;
          includeDelimiter = '>';
          Sp = P++;
        } else {
          state = State::Punctuator;
          Sp = P;
        }
        break;
      }
      /// last process
      if (IsWhiteSpace(curChar)) {
        P++;
        break;
      }
      DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_illegal_char);
      P++; /// skip this char
      break;
    }
    case State::CharacterLiteral: {
      if (curChar == '\'' && strBuilder.empty()) {
        state = State::Start;
        InsertToken(Sp, P, tok::char_constant, strBuilder);
        DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_empty_char_literal);
      }else if (curChar == '\'' && !strBuilder.ends_with('\\')) {
        state = State::Start;
        InsertToken(Sp, P, tok::char_constant, strBuilder);
      }else {
        strBuilder += curChar;
      }
      P++;
      break;
    }
    case State::StringLiteral: {
      if (curChar == '"' &&
          (strBuilder.empty() || !strBuilder.ends_with('\\'))) {
        state = State::Start;
        InsertToken(Sp, P, tok::string_literal, strBuilder);
      } else {
        strBuilder += curChar;
      }
      P++;
      break;
    }
    case State::Identifier: {
      if (IsLetter(curChar) ||  IsDigit(curChar)) {
        strBuilder += curChar;
        P++;
      } else {
        state = State::Start;
        InsertToken(Sp, P, tok::identifier, strBuilder);
      }
      break;
    }
    case State::Number: {
      constexpr std::uint8_t toLower = 32;
      if (strBuilder.empty()) {
        strBuilder += curChar;
        P++;
      } else {
        char lower_char = (curChar | toLower);
        if (!IsJudgeNumber(strBuilder, lower_char) &&
            (lower_char != 'e') &&
            (lower_char != 'p') &&
            (lower_char != 'f') &&
            (lower_char != 'u') &&
            (lower_char != 'l') &&
            (lower_char != '.') &&
            (((strBuilder.back() | toLower) != 'e' &&
              (strBuilder.back() | toLower) != 'p') ||
             (lower_char != '+' && lower_char != '-'))) {
          InsertToken(Sp, P, tok::pp_number, strBuilder);
          state = State::Start;
        } else {
          strBuilder += curChar;
          P++;
        }
        break;
      }
      break;
    }
    case State::Punctuator: {
      char nnChar = (P < Ep - 2) ? P[2] : '\0';
      tok::TokenKind tk = ParsePunctuation(P, curChar, nextChar, nnChar);
      LCC_ASSERT(tk != tok::unknown);
      InsertToken(Sp, P, tk, tok::getPunctuatorSpelling(tk));
      state = State::Start;
      break;
    }
    case State::LineComment: {
      if (curChar == '\n') {
        state = State::Start;
      } else {
        P++;
      }
      break;
    }
    case State::BlockComment: {
      if (curChar == '*' && nextChar == '/') {
        state = State::Start;
        P += 2;
      } else {
        P++;
      }
      break;
    }
    case State::AfterInclude: {
      if (curChar != includeDelimiter && curChar != '\n') {
        strBuilder += curChar;
        P++;
        break;
      }
      /// curChar is delimiter
      if (curChar != '\n') {
        InsertToken(Sp, P++, tok::string_literal,
                    strBuilder);
        state = State::Start;
        break;
      }
      DiagReport(Diag, SMLoc::getFromPointer(P), diag::err_lex_illegal_newline_in_after_include);
    }
    }
  }
  results.shrink_to_fit();

  if (state == State::CharacterLiteral) {
    DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_unclosed_char);
  }else if (state == State::StringLiteral) {
    DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_unclosed_string);
  }else if (state == State::BlockComment) {
    DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_unclosed_block_comment);
  }else if (state == State::AfterInclude) {
    DiagReport(Diag, SMLoc::getFromPointer(Sp), diag::err_lex_unclosed_after_include);
  }

  if (mLangOption == LanguageOption::C99) {
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
      DiagReport(Diag, SMLoc::getFromPointer(iter.getOffset()), diag::err_lex_illegal_token_in_c);
      break;
    case tok::pp_newline:
      break;
    case tok::identifier: {
      iter.setTokenKind(tok::getKeywordTokenType(iter.getRepresentation()));
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
      auto chars = ParseCharacters(iter, false);
      std::string str(chars.begin(), chars.end());
      iter.setTokenKind(tok::string_literal);
      iter.setValue(str);
      results.push_back(iter);
      break;
    }
    case tok::char_constant: {
      auto chars = ParseCharacters(iter, true);
      iter.setTokenKind(tok::char_constant);
      iter.setValue((int32_t)chars[0]);
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

void Lexer::RegularSourceCode() {
  /// check BOM header
  std::string_view UTF8_BOM = "\xef\xbb\xbf";
  if (mSourceCode.size() >= 3 && mSourceCode.substr(0, 3) == UTF8_BOM) {
    mSourceCode = mSourceCode.substr(3);
  }
  /// compatible with windows
  std::string::size_type pos = 0;
  while ((pos = mSourceCode.find("\r\n", pos)) != mSourceCode.npos) {
    mSourceCode.erase(pos, 1);
  }

  mSourceCode.shrink_to_fit();
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

uint32_t Lexer::OctalToNum(std::string_view value)
{
  std::uint32_t result;
  auto errors = std::from_chars(value.data(), value.data() + value.size(), result, 8);
  LCC_ASSERT(errors.ec == std::errc{});
  return result;
}

std::uint32_t Lexer::ParseEscapeChar(const char *p, char escape){
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
    DiagReport(Diag, SMLoc::getFromPointer(p), diag::err_lex_invalid_escaped_char);
    return escape;
  }
  return 0;
}

std::vector<char> Lexer::ParseCharacters(const Token &ppToken, bool handleCharMode) {
  const auto *sp = ppToken.getOffset();

  llvm::StringRef characters = ppToken.getRepresentation();
  std::vector<char> result;
  result.reserve(characters.size());
  int offset = 0, resultStart = 0;
  while (offset < characters.size()) {
    char ch = characters[offset];
    if (ch == '\n') {
      if (handleCharMode) {
        DiagReport(Diag, SMLoc::getFromPointer(sp+offset), diag::err_lex_implicit_newline_in_char);
      }
      else{
        DiagReport(Diag, SMLoc::getFromPointer(sp+offset), diag::err_lex_implicit_newline_in_string);
      }
      offset++;
      continue;
    }
    if (ch != '\\') {
      result.push_back(ch);
      resultStart++;
      offset++;
      if (handleCharMode) {
        if (resultStart > 1) {
          DiagReport(Diag, SMLoc::getFromPointer(sp + offset), diag::warn_lex_multi_character);
        }
      }
      continue;
    }
    if (offset + 1 == characters.size()) {
      break;
    }
    /// meaning ch == '\'
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
        DiagReport(Diag, SMLoc::getFromPointer(sp+offset), diag::err_lex_at_least_one_hexadecimal_digit_required);
        result.resize(1);
        result[0] = 'x';
        return result;
      }
      std::string_view sv(characters.data()+offset, lastHex-offset);
      auto value = std::strtol(sv.data(), nullptr, 16);
      result.push_back((char)value);
      resultStart++;
      offset = lastHex;
      break;
    }
    /// '\0' is octal char
    else if (IsDigit(characters[offset+1])) {
      offset++;
      int start = offset;
      /// first octal char
      if (offset < characters.size() && IsOctDigit(characters[offset])) {
        offset++;
      }else {
        goto end;
      }
      /// second octal char
      if (offset < characters.size() && IsOctDigit(characters[offset])) {
        offset++;
      }else {
        goto end;
      }
      /// third octal char
      if (offset < characters.size() && IsOctDigit(characters[offset])) {
        offset++;
      }else {
        goto end;
      }
      end:
      if (offset == start) {
        DiagReport(Diag, SMLoc::getFromPointer(sp+offset), diag::err_lex_at_least_one_oct_digit_required);
        result.resize(1);
        result[0] = '0';
        return result;
      }
      auto value = OctalToNum(characters.substr(start, offset-start));
      result.push_back((char)value);
      resultStart++;
      break;
    }else {
      auto character = ParseEscapeChar(sp+offset,characters[offset + 1]);
      result.push_back((char)character);
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
