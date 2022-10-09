/***********************************
 * File:     IdeTokGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_IDETOKGEN_H
#define LCC_IDETOKGEN_H

#include "TokenGen.h"
#include <unordered_map>
namespace lcc {
namespace lexer {
class IdeTokGen final : public TokenGen {
public:
  IdeTokGen();
  Token ParseToken() override;
  bool CanParseToken() override;

private:
  bool IsKeyWord(const std::string &Word);
  std::unordered_map<std::string, TokenType> KeyWordTypeMap;
};
} // namespace lexer
} // namespace lcc

#endif // LCC_IDETOKGEN_H
