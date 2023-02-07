/***********************************
 * File:     CToken.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#ifndef LCC_TOKEN_H
#define LCC_TOKEN_H
#include "TokenKinds.h"
#include <cstdint>
#include <string>
#include <vector>
#include <variant>
#include <algorithm>
namespace lcc{
class Token {
private:
  using TokenValue =
      std::variant<std::monostate, int32_t, uint32_t, int64_t, uint64_t, float, double, std::string>;
  TokenValue mValue;
  tok::TokenKind mTokenKind;
  uint32_t mOffset; // byte offset
  uint32_t mLength;
  bool mLeadingWhiteSpace;
  const SourceFile &mSourceFile;
public:
  using ValueType = TokenValue;
  Token(tok::TokenKind tokenKind, uint32_t offset, uint32_t length, const SourceFile &sourceFile,
            ValueType value = std::monostate{})
      : mLeadingWhiteSpace(false), mTokenKind(tokenKind), mOffset(offset),
        mLength(length), mSourceFile(sourceFile), mValue(std::move(value)){}

  [[nodiscard]] std::string_view getRepresentation() const {
    return mSourceFile.sourceCode.substr(mOffset, mLength);
  }

  std::string getContent() const {
    return std::visit([](auto &&value) -> std::string {
      using T = std::decay_t<decltype(value)>;
      if constexpr (std::is_same_v<T, std::monostate>) {
        return "";
      }else if constexpr (std::is_same_v<T, std::string>) {
        return value;
      }else {
        return std::to_string(value);
      }
    }, mValue);
  }

  std::string_view getStrTokName() const {
    assert(std::holds_alternative<std::string>(mValue));
    return std::get<std::string>(mValue);
  }

  [[nodiscard]] uint32_t getLine() const {
    auto result = std::lower_bound(mSourceFile.lineStartOffsets.begin(), mSourceFile.lineStartOffsets.end(), mOffset);
    return std::distance(mSourceFile.lineStartOffsets.begin(), result) + (*result == mOffset ? 1 : 0);
  }

  [[nodiscard]] uint32_t getColumn() const {
    uint32_t line = getLine();
    return mOffset - mSourceFile.lineStartOffsets[line-1] + 1;
  }

  [[nodiscard]] tok::TokenKind getTokenKind() const {
    return mTokenKind;
  }

  void setTokenKind(tok::TokenKind tokenKind) {
    mTokenKind = tokenKind;
  }

  [[nodiscard]] ValueType getValue() const {
    return mValue;
  }

  void setValue(ValueType value) {
    mValue = std::move(value);
  }

  [[nodiscard]] uint32_t getOffset() const {
    return mOffset;
  }
  [[nodiscard]] uint32_t getLength() const {
    return mLength;
  }

  [[nodiscard]] bool hasLeadingWhitespace() const {
    return mLeadingWhiteSpace;
  }
  void setLeadingWhitespace(bool leadingWhitespace) {
    mLeadingWhiteSpace = leadingWhitespace;
  }
};

} // namespace lcc::lexer

#endif // LCC_CTOKEN_H
