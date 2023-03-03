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
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string>
#include <variant>
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
namespace lcc{
class Token {
private:
  using TokenValue =
      std::variant<std::monostate, int32_t, uint32_t, int64_t, uint64_t, float, double, std::string>;
  TokenValue mValue;
  tok::TokenKind mTokenKind;
  const char *mOffsetPtr{nullptr};
  uint32_t mLength;
  llvm::SourceMgr &mSrcMgr;
public:
  using ValueType = TokenValue;
  Token(tok::TokenKind tokenKind, const char *offsetPtr, uint32_t length,
        llvm::SourceMgr &mgr, ValueType value = std::monostate{})
      : mTokenKind(tokenKind), mOffsetPtr(offsetPtr), mLength(length),
        mSrcMgr(mgr), mValue(std::move(value)){}

  [[nodiscard]] llvm::StringRef getRepresentation() const {
      if (std::holds_alternative<std::string>(mValue)) {
        return std::get<std::string>(mValue);
      }else {
        auto *mem = mSrcMgr.getMemoryBuffer(mSrcMgr.getMainFileID());
        uint32_t offset = mOffsetPtr - mem->getBufferStart();
        return mem->getBuffer().substr(offset, mLength);
      }
  }

  [[nodiscard]] std::pair<unsigned, unsigned> getLineAndColumn() const {
    assert(mOffsetPtr);
    return mSrcMgr.getLineAndColumn(llvm::SMLoc::getFromPointer(mOffsetPtr));
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

  [[nodiscard]] const char *getOffset() const {
    return mOffsetPtr;
  }

  [[nodiscard]] llvm::SMLoc getSMLoc() const {
    return llvm::SMLoc::getFromPointer(getOffset());
  }
};

} // namespace lcc::lexer

#endif // LCC_CTOKEN_H
