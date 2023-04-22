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

#include "TokenKinds.h"
#include <unordered_map>

using namespace lcc;

static const char * const TokNames[] = {
#define TOK(ID) #ID,
#define KEYWORD(ID) #ID,
#include "TokenKinds.def"
    nullptr
};

const char *tok::getTokenName(TokenKind Kind) {
  if (Kind < tok::NUM_TOKENS)
    return TokNames[Kind];
  return nullptr;
}

const char *tok::getPunctuatorSpelling(TokenKind Kind) {
  switch (Kind) {
#define PUNCTUATOR(ID, SP) case ID: return SP;
#include "TokenKinds.def"
  default: break;
  }
  return nullptr;
}

const char *tok::getKeywordSpelling(TokenKind Kind) {
  switch (Kind) {
#define KEYWORD(ID) case kw_ ## ID: return #ID;
#include "TokenKinds.def"
  default: break;
  }
  return nullptr;
}

tok::TokenKind tok::getKeywordTokenType(std::string_view keyword) {
  static std::unordered_map<std::string_view, tok::TokenKind> hashTable = {
      {"_Bool", tok::kw__Bool},
      {"auto", tok::kw_auto},
      {"double", tok::kw_double},
      {"int", tok::kw_int},
      {"struct", tok::kw_struct},
      {"break", tok::kw_break},
      {"else", tok::kw_else},
      {"long", tok::kw_long},
      {"switch", tok::kw_switch},
      {"case", tok::kw_case},
      {"enum", tok::kw_enum},
      {"register", tok::kw_register},
      {"typedef", tok::kw_typedef},
      {"char", tok::kw_char},
      {"extern", tok::kw_extern},
      {"return", tok::kw_return},
      {"union", tok::kw_union},
      {"const", tok::kw_const},
      {"float", tok::kw_float},
      {"short", tok::kw_short},
      {"unsigned", tok::kw_unsigned},
      {"continue", tok::kw_continue},
      {"for", tok::kw_for},
      {"signed", tok::kw_signed},
      {"default", tok::kw_default},
      {"goto", tok::kw_goto},
      {"sizeof", tok::kw_sizeof},
      {"volatile", tok::kw_volatile},
      {"do", tok::kw_do},
      {"if", tok::kw_if},
      {"static", tok::kw_static},
      {"while", tok::kw_while},
      {"void", tok::kw_void},
      {"restrict", tok::kw_restrict},
      {"inline", tok::kw_inline}};
  if (hashTable.find(keyword) != hashTable.end()) {
    return hashTable[keyword];
  }
  return tok::identifier;
}
