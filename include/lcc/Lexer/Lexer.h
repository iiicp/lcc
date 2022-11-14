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
#include "Token.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SourceMgr.h"
#include <vector>
namespace lcc {
class Lexer {
private:
  llvm::SourceMgr &SrcMgr;
  DiagnosticsEngine &Diags;
  const char *CurPtr;
  uint8_t *mLineHead;
  const char *BufferStart;
  const char *BufferEnd;
  llvm::StringRef CurBuf;

  llvm::StringMap<tok::TokenKind> HashTable;
public:
  Lexer(llvm::SourceMgr &SrcMgr, DiagnosticsEngine &Diags)
      : SrcMgr(SrcMgr), Diags(Diags) {
    CurBuf = SrcMgr.getMemoryBuffer(SrcMgr.getMainFileID())->getBuffer();
    CurPtr = CurBuf.begin();
    BufferStart = CurBuf.begin();
    BufferEnd = CurBuf.end();

    addKeywords();
  };
  std::vector<Token> Tokenize();

private:
  void next(Token &Result);
  llvm::SMLoc getLoc() { return llvm::SMLoc::getFromPointer(CurPtr); }

  void SkipWhiteSpace();
  void SkipComment();
  void LexIdentifier(Token &Result);
  void LexNumeric(Token &Result);
  void LexPunctuator(Token &Result);
  void LexCharacter(Token &Result);
  void LexStringLiteral(Token &Result);
  void ScanEscapeChar(const char *&Ptr);

  void addKeywords();
  void addKeyword(llvm::StringRef Keyword, tok::TokenKind TokenCode);
  tok::TokenKind getKeyword(llvm::StringRef Name,
                            tok::TokenKind DefaultTokenCode = tok::unknown);

  void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind, Token::Variant Value={});
};
} // namespace lcc::lexer

#endif // LCC_LEXER_H
