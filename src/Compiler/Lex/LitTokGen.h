/***********************************
 * File:     LitTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_LITTOKGEN_H
#define LCC_LITTOKGEN_H

#include "TokenGen.h"

namespace lcc {
namespace lexer {
class LitTokGen : public TokenGen {
public:
  int32_t ScanEscapeChar();
};
} // namespace lexer
} // namespace lcc

#endif // LCC_LITTOKGEN_H
