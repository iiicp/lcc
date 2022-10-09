/***********************************
 * File:     lexer.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/9/30
 *
 * Sign:     enjoy life
 ***********************************/

#ifndef LCC_LEXER_H
#define LCC_LEXER_H
#include <vector>
#include "Token.h"
namespace lcc {
namespace lexer {
  std::vector<Token> Tokenize(std::string &source);
}
}

#endif // LCC_LEXER_H
