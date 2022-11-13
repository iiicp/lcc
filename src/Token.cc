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
#include <sstream>
namespace lcc {

std::string Token::GetTokenSpelling() const {
  const char *punctuator = getPunctuatorSpelling(mType);
  if (punctuator) {
    return punctuator;
  }
  const char *keyword = getKeywordSpelling(mType);
  if (keyword) {
    return keyword;
  }
  switch (mType) {
  case tok::identifier:
  case tok::char_constant:
  case tok::string_literal:
  case tok::numeric_constant:
    return std::visit(
        [](auto &&Value) -> std::string {
          using T = std::decay_t<decltype(Value)>;
          if constexpr (std::is_same<T, std::monostate>::value) {
            return "";
          } else if constexpr (std::is_same<T, std::monostate>::value) {
            return Value;
          } else {
            std::ostringstream os;
            os << Value;
            return os.str();
          }
        },
        mValue);
  case tok::eof:
    return "eof";
  default:
    return "unknown";
  }
}
} // namespace lcc::lexer
