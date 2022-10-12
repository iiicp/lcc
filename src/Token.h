/***********************************
 * File:     Token.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#ifndef LCC_TOKEN_H
#define LCC_TOKEN_H
#include <cstdint>
#include <string>
#include <variant>
namespace lcc::lexer {
enum TokenType {
#define TOK(X) X,
#include "TokenKinds.def"
};
class Token {
  using TokenValue =
      std::variant<std::monostate, int32_t, uint32_t, int64_t, uint64_t,
                   float, double, std::string>;

private:
  uint64_t mLine;
  uint64_t mColumn;
  TokenType mType;
  TokenValue mValue;

public:
  explicit Token(uint64_t Line, uint64_t Column, TokenType Type)
      : mLine(Line), mColumn(Column), mType(Type) {}
  template <typename T>
  explicit Token(uint64_t Line, uint64_t Column, TokenType Type, T &&Value)
      : mLine(Line), mColumn(Column), mType(Type),
        mValue(std::forward<T>(Value)) {}
  [[nodiscard]] uint64_t GetLine() const { return mLine; }
  [[nodiscard]] uint64_t GetColumn() const { return mColumn; }
  [[nodiscard]] TokenType GetTokenType() const { return mType; }
  [[nodiscard]] const TokenValue &GetTokenValue() const { return mValue; }

  std::string GetTokenSpelling() const;
};
} // namespace lcc::lexer

#endif // LCC_TOKEN_H
