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
#include <string>
#include <vector>
#include "SourceObject.h"

namespace lcc {

using PPTokenObject = SourceObject<PPToken>;
using CTokenObject = SourceObject<CToken>;

class Lexer {
public:
  static PPTokenObject tokenize(std::string &sourceCode, std::string_view sourcePath = "<stdin>");
  static CTokenObject toCTokens(PPTokenObject && ppTokens);
};
}

#endif // LCC_LEXER_H
