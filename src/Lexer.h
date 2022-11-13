/***********************************
 * File:     Lexer.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#ifndef LCC_LEXER_H
#define LCC_LEXER_H

#include "Token.h"
#include <unordered_map>
#include <vector>
namespace lcc {
enum class IntegerType {
  Default,
  U,
  UL,
  ULL,
  L,
  LL
};
class Lexer {
private:
  uint8_t *mCursor;
  uint8_t *mLineHead;
  uint8_t *mSrcEnd;
  uint32_t mLine;
  uint32_t mColumn;
  std::unordered_map<std::string, tok::TokenKind> mKeywordTypeMap;

public:
  Lexer(uint8_t *SrcStart, uint32_t SrcSize)
      : mCursor(SrcStart), mLineHead(SrcStart), mSrcEnd(SrcStart + SrcSize),
        mLine(1), mColumn(0) {
    InitKeyWordTypeMap();
  };
  std::vector<Token> Tokenize();

private:
  Token GetNextToken();
  bool IsEOF(uint8_t *CharPtr) const;
  [[nodiscard]] bool IsLetter() const;
  [[nodiscard]] bool IsDigit() const;
  [[nodiscard]] bool IsHexDigit() const;
  [[nodiscard]] bool IsOctDigit() const;
  [[nodiscard]] bool IsXDigit(int base) const;
  bool IsDigit(uint8_t *CharPtr) const;
  [[nodiscard]] bool IsLetterOrDigit() const;
  [[nodiscard]] bool IsPunctuatorStart() const;
  [[nodiscard]] bool IsNumericStart() const;
  [[nodiscard]] bool IsCharStart() const;
  [[nodiscard]] bool IsStringStart() const;

  void ScanWhiteSpace();
  Token ScanIdentifier();
  Token ScanPunctuator();
  Token ScanCharacter();
  Token ScanStringLiteral();
  Token ScanNumeric();
  Token ScanIntegerNumeric(int base);
  Token ScanFloatNumeric();
  int32_t ScanEscapeChar();
  IntegerType ScanIntegerSuffix();

  void InitKeyWordTypeMap();
};
} // namespace lcc::lexer

#endif // LCC_LEXER_H
