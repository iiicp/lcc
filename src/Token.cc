/***********************************
 * File:     Token.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#include "Token.h"
namespace lcc {

std::string Token::GetTokenSpelling() const {
  const char *punctuator = getPunctuatorSpelling(Kind);
  if (punctuator) {
    return punctuator;
  }
  const char *keyword = getKeywordSpelling(Kind);
  if (keyword) {
    return keyword;
  }
  switch (Kind) {
  case tok::identifier:
  case tok::char_constant:
  case tok::string_literal:
  case tok::numeric_constant:
    return llvm::StringRef(Ptr, Length).str();
  case tok::eof:
    return "eof";
  default:
    return "unknown";
  }
}
} // namespace lcc::lexer
