/***********************************
 * File:     ChaLitTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_CHALITTOKGEN_H
#define LCC_CHALITTOKGEN_H

#include "LitTokGen.h"

namespace lcc {
namespace lexer {
class ChaLitTokGen final : public LitTokGen {
public:
  bool CanParseToken() override;
  Token ParseToken() override;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_CHALITTOKGEN_H
