/***********************************
 * File:     TokenKinds.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/13
 *
 * Sign:     enjoy life
 ***********************************/

#include "lcc/Basic/TokenKinds.h"

using namespace lcc;

static const char *const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID, FLAG) #ID,
#include "lcc/Basic/TokenKinds.def"
    nullptr};

const char *tok::getTokenName(TokenKind Kind) {
  if (Kind < tok::NUM_TOKENS)
    return TokNames[Kind];
  return nullptr;
}

const char *tok::getPunctuatorSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(ID, SP)                                                     \
  case ID:                                                                     \
    return SP;
#include "lcc/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}

const char *tok::getKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(ID, FLAG)                                                      \
  case kw_##ID:                                                                \
    return #ID;
#include "lcc/Basic/TokenKinds.def"
  default:
    break;
  }
  return nullptr;
}
