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
#include "lcc/Basic/TokenKinds.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <string>
#include <variant>
namespace lcc {
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
            : mValue(std::move(value)), mTokenKind(tokenKind), mOffsetPtr(offsetPtr), mLength(length),
              mSrcMgr(mgr) {}

        [[nodiscard]] llvm::StringRef getRepresentation() const {
            if (std::holds_alternative<std::string>(mValue)) {
                return std::get<std::string>(mValue);
            } else {
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
    using TokIter = std::vector<Token>::const_iterator;

    class TokenBase {
    protected:
        bool leadingWhitespace_;
        tok::TokenKind tokenKind_;
        uint32_t macroId_;
        llvm::SMLoc loc_;
        uint32_t length_;

        TokenBase() = default;

        TokenBase(tok::TokenKind tokenKind, llvm::SMLoc loc, uint32_t length, uint32_t macroId)
            : leadingWhitespace_(false), tokenKind_(tokenKind), macroId_(macroId), loc_(loc), length_(length) {}

    public:
        [[nodiscard]] tok::TokenKind GetTokenKind() const {
            return tokenKind_;
        }

        [[nodiscard]] llvm::SMLoc GetLoc() const {
            return loc_;
        }

        [[nodiscard]] uint32_t GetLength() const {
            return length_;
        }

        [[nodiscard]] uint32_t GetMacroId() const {
            return macroId_;
        }

        void SetMacroId(uint32_t macroId) {
            macroId_ = macroId;
        }

        [[nodiscard]] bool IsMacroInserted() const {
            return static_cast<bool>(macroId_);
        }

        [[nodiscard]] bool HasLeadingWhiteSpace() const {
            return leadingWhitespace_;
        }

        void SetLeadingWhiteSpace(bool leadingWhiteSpace) {
            leadingWhitespace_ = leadingWhiteSpace;
        }
    };

    class PPToken final : public TokenBase {
    private:
        std::string value_;

    public:
        PPToken(tok::TokenKind tokenKind, llvm::SMLoc loc, uint64_t length, uint32_t macroId = 0, std::string value = {})
            : TokenBase(tokenKind, loc, length, macroId), value_(std::move(value)) {}

        [[nodiscard]] std::string_view GetValue() const {
            return value_;
        }
    };

    class CToken final : public TokenBase {
    private:
        using TokenValue =
                std::variant<std::monostate, int32_t, uint32_t, int64_t, uint64_t, float, double, std::string>;
        TokenValue value_;

    public:
        CToken(tok::TokenKind tokenKind, llvm::SMLoc loc, uint64_t length, uint32_t macroId = 0, TokenValue value = std::monostate{})
            : TokenBase(tokenKind, loc, length, macroId), value_(std::move(value)) {}

        [[nodiscard]] const TokenValue &GetValue() const {
            return value_;
        }

        void SetValue(const TokenValue &value) {
            value_ = value;
        }
    };
}// namespace lcc

#endif// LCC_CTOKEN_H
