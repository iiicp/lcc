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

#include "lcc/Basic/Diagnostic.h"
#include "lcc/Lexer/Token.h"
#include <optional>
#include <string>
#include <vector>

#include "TokenStream.h"

namespace lcc::lexer {
    TokenStream<PPToken> tokenize(std::string_view filePath, llvm::raw_ostream *reporter = &llvm::errs(), bool *errors = nullptr);
    TokenStream<PPToken> tokenize(const std::string &source, llvm::raw_ostream *reporter = &llvm::errs(), bool *errors = nullptr, std::string_view path = "<stdin>");
    TokenStream<CToken> toCTokens(TokenStream<PPToken> &&ppTokenStream, llvm::raw_ostream *reporter = &llvm::errs(), bool *errors = nullptr);
    TokenStream<CToken> toCTokens(const TokenStream<PPToken> &ppTokenStream, llvm::raw_ostream *reporter = &llvm::errs(), bool *errors = nullptr);
}// namespace lcc::lexer


#endif// LCC_LEXER_H
