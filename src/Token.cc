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
namespace lcc::lexer {

std::string Token::GetTokenSpelling() const {
  switch (mType) {
#define PUNCTUATOR(X, Y)                                                       \
  case X:                                                                      \
    return Y;
#define KEYWORD(X, Y)                                                          \
  case kw_##X:                                                                 \
    return Y;
#include "TokenKinds.def"
  case identifier:
  case char_constant:
  case string_literal:
  case numeric_constant:
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
  case eof:
    return "eof";
  default:
    return "unknown";
  }
}
} // namespace lcc::lexer
