/***********************************
 * File:     EofTokGen.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "EofTokGen.h"

namespace lcc {
namespace lexer {
bool EofTokGen::CanParseToken() { return *mCursor == '\0'; }

Token EofTokGen::ParseToken() { return Token{mLine, GetColumn(), eof}; }
} // namespace Lexer
} // namespace lcc
