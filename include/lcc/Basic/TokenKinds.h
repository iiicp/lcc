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
#ifndef LCC_TOKEN_KIND_H
#define LCC_TOKEN_KIND_H

namespace lcc {
namespace tok {
enum TokenKind : unsigned short {
#define TOK(X) X,
#include "lcc/Basic/TokenKinds.def"
  NUM_TOKENS
};

const char *getTokenName(TokenKind Kind);
const char *getPunctuatorSpelling(TokenKind Kind);
const char *getKeywordSpelling(TokenKind Kind);
} // tok
}

#endif // LCC_TOKEN_KIND_H
