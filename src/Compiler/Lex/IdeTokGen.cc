/***********************************
 * File:     IdeTokGen.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/3
 *
 * Sign:     enjoy life
 ***********************************/
#include "IdeTokGen.h"

namespace lcc {
namespace lexer {
Token IdeTokGen::ParseToken() {
  uint64_t line = mLine;
  uint64_t column = GetColumn();

  uint8_t *begin = mCursor;
  mCursor++;
  while (IsLetterOrDigit(mCursor)) {
    mCursor++;
  }
  /// hello ll
  /// ^    ^
  std::string word(begin, mCursor);
  TokenType type = identifier;
  if (IsKeyWord(word)) {
    type = KeyWordTypeMap[word];
  }
  return Token{line, column, type, std::move(word)};
}

bool IdeTokGen::IsKeyWord(const std::string &Word) {
  return Word == "auto" || Word == "break" || Word == "case" ||
         Word == "char" || Word == "const" || Word == "continue" ||
         Word == "default" || Word == "do" || Word == "double" ||
         Word == "else" || Word == "enum" || Word == "extern" ||
         Word == "float" || Word == "for" || Word == "goto" || Word == "if" ||
         Word == "int" || Word == "long" || Word == "register" ||
         Word == "return" || Word == "short" || Word == "signed" ||
         Word == "sizeof" || Word == "static" || Word == "struct" ||
         Word == "switch" || Word == "typedef" || Word == "union" ||
         Word == "unsigned" || Word == "void" || Word == "volatile" ||
         Word == "while";
}

IdeTokGen::IdeTokGen() {
  KeyWordTypeMap = {{"auto", kw_auto},         {"break", kw_break},
                    {"case", kw_case},         {"char", kw_char},
                    {"const", kw_const},       {"continue", kw_continue},
                    {"default", kw_default},   {"do", kw_do},
                    {"double", kw_double},     {"else", kw_else},
                    {"enum", kw_enum},         {"extern", kw_extern},
                    {"float", kw_float},       {"for", kw_for},
                    {"goto", kw_goto},         {"if", kw_if},
                    {"int", kw_int},           {"long", kw_long},
                    {"register", kw_register}, {"return", kw_return},
                    {"short", kw_short},       {"signed", kw_signed},
                    {"sizeof", kw_sizeof},     {"static", kw_static},
                    {"struct", kw_struct},     {"switch", kw_switch},
                    {"typedef", kw_typedef},   {"union", kw_union},
                    {"unsigned", kw_unsigned}, {"void", kw_void},
                    {"volatile", kw_volatile}, {"while", kw_while}};
}

bool IdeTokGen::CanParseToken() { return IsLetter(mCursor); }

} // namespace Lexer
} // namespace lcc
