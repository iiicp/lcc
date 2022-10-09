/***********************************
 * File:     EofTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_EOFTOKGEN_H
#define LCC_EOFTOKGEN_H
#include "TokenGen.h"
namespace lcc {
namespace lexer {
class EofTokGen final : public TokenGen {
public:
  bool CanParseToken() override;
  Token ParseToken() override;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_EOFTOKGEN_H
