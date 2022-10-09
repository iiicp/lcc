/***********************************
 * File:     Token.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/2
 *
 * Sign:     enjoy life
 ***********************************/

#include "Token.h"
#include <sstream>

namespace lcc {
namespace lexer {
std::string Token::GetTokenSimpleSpelling(TokenType tokenType) const {
  switch (tokenType) {
#define PUNCTUATOR(X, Y)                                                       \
  case X:                                                                      \
    return Y;
#define KEYWORD(X, Y)                                                          \
  case kw_##X:                                                                 \
    return Y;
#include "TokenKinds.def"
  case eof:
    return "eof";
  case identifier:
    return std::get<std::string>(GetValue());
  case numeric_constant:
  case char_constant:
  case string_literal:
    return std::visit(
        [](auto &&value) -> std::string {
          using T = std::decay_t<decltype(value)>;
          if constexpr (std::is_same_v<std::string, T>) {
            return value;
          } else if constexpr (!std::is_same_v<std::monostate, T>) {
            std::ostringstream ss;
            ss << value;
            return ss.str();
          } else {
            return "";
          }
        },
        GetValue());
  default:
    break;
  }
  return "unKnow";
}
} // namespace Lexer
} // namespace lcc