/***********************************
 * File:     Token.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#ifndef LCC_TOKEN_H
#define LCC_TOKEN_H
#include <cstdint>
#include <string>
#include <variant>
#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "TokenKinds.h"
namespace lcc{
class Token {
  friend class Lexer;
  using Variant = std::variant<std::monostate,int32_t,uint32_t,int64_t,uint64_t,float,double,llvm::StringRef>;
private:
  tok::TokenKind Kind;
  llvm::SMLoc Loc;
  const char *Ptr;
  size_t Length;
  Variant Value;
public:
  tok::TokenKind getKind() const {return Kind;}
  void setKind(tok::TokenKind K) {Kind = K;}

  llvm::SMLoc getLocation() const {
    return Loc;
  }
  size_t getLength() const {
    return Length;
  }

  Variant getTokenValue() const {
    return Value;
  }

  /// is/isNot - Predicates to check if this token is a
  /// specific kind, as in "if (Tok.is(tok::l_brace))
  /// {...}".
  bool is(tok::TokenKind K) const { return Kind == K; }
  bool isNot(tok::TokenKind K) const { return Kind != K; }
  bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const {
    return is(K1) || is(K2);
  }
  template <typename... Ts>
  bool isOneOf(tok::TokenKind K1, Ts... Ks) const {
    return is(K1) || isOneOf(Ks...);
  }

  std::string GetTokenSpelling() const;
};
} // namespace lcc::lexer

#endif // LCC_TOKEN_H
