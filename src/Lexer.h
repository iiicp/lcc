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

using PPTokens = SourceObject<PPToken>;
using CTokens = SourceObject<CToken>;

PPTokens tokenize(std::string &sourceCode, std::string_view sourcePath = "<stdin>");
CTokens toCTokens(PPTokens && ppTokens);
}

#endif // LCC_LEXER_H
