/***********************************
 * File:     TokenKinds.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/13
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_TOKENKINDS_H
#define LCC_TOKENKINDS_H
#include <string_view>
#include <vector>
namespace lcc {

namespace tok {
enum TokenKind : unsigned short {
#define TOK(X) X,
#include "TokenKinds.def"
  NUM_TOKENS
};

const char *getTokenName(TokenKind Kind);
const char *getPunctuatorSpelling(TokenKind Kind);
const char *getKeywordSpelling(TokenKind Kind);
TokenKind getKeywordTokenType(std::string_view keyword);
} // tok
}

#endif // LCC_TOKENKINDS_H
