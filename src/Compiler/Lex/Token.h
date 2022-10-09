/***********************************
 * File:     Token.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/2
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_TOKEN_H
#define LCC_TOKEN_H
#include <string>
#include <variant>
namespace lcc {
namespace lexer {
enum TokenType {
#define TOK(X) X,
#include "TokenKinds.def"
#undef TOK
};

class Token {
  using variant = std::variant<std::monostate, char, int32_t, uint32_t, int64_t,
                               uint64_t, float, double, std::string>;

public:
  explicit Token(uint64_t Line, uint64_t Column,
                 TokenType Type) noexcept
      : mLine(Line), mColumn(Column), mType(Type) {}

  template <class T>
  Token(uint64_t Line, uint64_t Column,TokenType Type,
        T &&Value)
      : mLine(Line), mColumn(Column), mType(Type),
        mValue(std::forward<T>(Value)) {}

  TokenType GetTokenType() const { return mType; }

  const variant &GetValue() const { return mValue; }

  uint64_t GetLine() const { return mLine; }

  uint64_t GetColumn() const { return mColumn; }

  std::string GetTokenSimpleSpelling(TokenType tokenType) const;

private:
  uint64_t mLine;
  uint64_t mColumn;
  TokenType mType;
  variant mValue;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_TOKEN_H
