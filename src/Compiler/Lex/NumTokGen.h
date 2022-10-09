/***********************************
 * File:     NumTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_NUMTOKGEN_H
#define LCC_NUMTOKGEN_H

#include "TokenGen.h"

namespace lcc {
namespace lexer {
class NumTokGen final : public TokenGen {
public:
  bool CanParseToken() override;
  Token ParseToken() override;

private:
  Token ScanFloatLiteral(uint8_t *Start, uint64_t line, uint64_t column);
  Token ScanIntLiteral(uint8_t *Start, int len, int base, uint64_t line,
                       uint64_t column);
};
} // namespace lexer
} // namespace lcc

#endif // LCC_NUMTOKGEN_H
