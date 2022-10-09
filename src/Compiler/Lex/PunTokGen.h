/***********************************
 * File:     PunTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_PUNTOKGEN_H
#define LCC_PUNTOKGEN_H

#include "TokenGen.h"
#include <unordered_set>
namespace lcc {
namespace lexer {
class PunTokGen final : public TokenGen {
public:
  PunTokGen();
  bool CanParseToken() override;
  Token ParseToken() override;

private:
  std::unordered_set<char> FirstCharSet;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_PUNTOKGEN_H
