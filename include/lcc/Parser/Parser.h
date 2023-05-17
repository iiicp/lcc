/***********************************
 * File:     Parser.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/17
 ***********************************/

#ifndef LCC_PARSER_H
#define LCC_PARSER_H
#include "lcc/AST/AST.h"
#include "lcc/Basic/Diagnostic.h"
#include "lcc/Lexer/Token.h"
#include <bitset>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <vector>
namespace lcc {
using TokenBitSet = std::bitset<tok::TokenKind::NUM_TOKENS>;
class Parser {
private:
  const std::vector<Token>& mTokens;
  TokIter mTokCursor;
  TokIter mTokEnd;
  bool mIsCheckTypedefType{true};
  DiagnosticEngine &Diag;
private:
  class Scope {
  private:
    struct Symbol {
      std::string_view identifier;
      bool isTypedef{false};
    };
    std::vector<std::unordered_map<std::string_view, Symbol>> mCurrentScope;
  public:
    Scope() {
      mCurrentScope.emplace_back();
    }
    void addTypedef(std::string_view name);
    bool isTypedefInScope(std::string_view name) const;
    bool checkIsTypedefInCurrentScope(std::string_view name) const;
    void addToScope(std::string_view name);
    void pushScope();
    void popScope();
  };
  Scope mScope;
  TokenBitSet FirstDeclaration, FirstExpression, FirstStatement;
  TokenBitSet FirstStructDeclaration, FirstExternalDeclaration;
public:
  explicit Parser(const std::vector<Token> & tokens, DiagnosticEngine &diag);
  Syntax::TranslationUnit ParseTranslationUnit();
  
private:
  std::optional<Syntax::ExternalDeclaration> ParseExternalDeclaration();
  std::optional<Syntax::Declaration> ParseDeclarationSuffix(
      Syntax::DeclSpec &&declSpec,
      std::optional<Syntax::Declarator> &&alreadyParsedDeclarator = {});
  std::optional<Syntax::Declaration> ParseDeclaration();
  Syntax::DeclSpec ParseDeclarationSpecifiers();
  std::optional<Syntax::Declarator> ParseDeclarator();
  std::optional<Syntax::DirectDeclarator> ParseDirectDeclarator();
  void ParseDirectDeclaratorSuffix(TokIter beginTokLoc, Syntax::DirectDeclarator &directDeclarator);
  std::optional<Syntax::AbstractDeclarator> ParseAbstractDeclarator();
  std::optional<Syntax::DirectAbstractDeclarator> ParseDirectAbstractDec();
  std::optional<Syntax::DirectAbstractDeclarator>
  ParseDirectAbstractDeclaratorSuffix();
  std::optional<Syntax::ParamTypeList> ParseParameterTypeList();
  std::optional<Syntax::ParamList> ParseParameterList();
  std::optional<Syntax::ParameterDeclaration> ParseParameterDeclaration();
  std::optional<Syntax::ParameterDeclaration>
  ParseParameterDeclarationSuffix(Syntax::DeclSpec &declarationSpecifiers);
  Syntax::Pointer ParsePointer();
  std::optional<Syntax::StructOrUnionSpec> ParseStructOrUnionSpecifier();
  std::optional<Syntax::StructOrUnionSpec::StructDeclaration>
  ParseStructDeclaration();
  std::optional<Syntax::StructOrUnionSpec::StructDeclarator>
  ParseStructDeclarator();
  std::optional<Syntax::EnumSpecifier> ParseEnumSpecifier();
  std::optional<Syntax::EnumSpecifier::Enumerator> ParseEnumerator();
  std::optional<Syntax::Initializer> ParseInitializer();
  std::optional<Syntax::InitializerList> ParseInitializerList();

  std::optional<Syntax::BlockStmt> ParseBlockStmt();
  std::optional<Syntax::BlockItem> ParseBlockItem();
  std::optional<Syntax::Stmt> ParseStmt();
  std::optional<Syntax::Stmt> ParseIfStmt();
  std::optional<Syntax::Stmt> ParseWhileStmt();
  std::optional<Syntax::Stmt> ParseDoWhileStmt();
  std::optional<Syntax::Stmt> ParseForStmt();
  std::optional<Syntax::Stmt> ParseBreakStmt();
  std::optional<Syntax::Stmt> ParseContinueStmt();
  std::optional<Syntax::Stmt> ParseReturnStmt();
  std::optional<Syntax::Stmt> ParseSwitchStmt();
  std::optional<Syntax::Stmt> ParseCaseStmt();
  std::optional<Syntax::Stmt> ParseDefaultStmt();
  std::optional<Syntax::Stmt> ParseGotoStmt();
  std::optional<Syntax::Stmt> ParseExprStmt();

  std::optional<Syntax::Expr> ParseExpr();
  std::optional<Syntax::AssignExpr> ParseAssignExpr();
  std::optional<Syntax::CondExpr> ParseConditionalExpr();
  std::optional<Syntax::LogOrExpr> ParseLogOrExpr();
  std::optional<Syntax::LogAndExpr> ParseLogAndExpr();
  std::optional<Syntax::BitOrExpr> ParseBitOrExpr();
  std::optional<Syntax::BitXorExpr> ParseBitXorExpr();
  std::optional<Syntax::BitAndExpr> ParseBitAndExpr();
  std::optional<Syntax::EqualExpr> ParseEqualExpr();
  std::optional<Syntax::RelationalExpr> ParseRelationalExpr();
  std::optional<Syntax::ShiftExpr> ParseShiftExpr();
  std::optional<Syntax::AdditiveExpr> ParseAdditiveExpr();
  std::optional<Syntax::MultiExpr> ParseMultiExpr();
  std::optional<Syntax::CastExpr> ParseCastExpr();
  std::optional<Syntax::UnaryExpr> ParseUnaryExpr();
  std::optional<Syntax::PostFixExpr> ParsePostFixExpr();
  void ParsePostFixExprSuffix(TokIter beginTokLoc,
                              Syntax::PostFixExpr &postFixExpr);

  std::optional<Syntax::TypeName> ParseTypeName();
  bool IsAssignOp(tok::TokenKind type);
  bool Expect(tok::TokenKind tokenType);
  bool ConsumeAny();
  bool Peek(tok::TokenKind tokenType);
  bool PeekN(int n, tok::TokenKind tokenType);
  bool IsUnaryOp(tok::TokenKind tokenType);
  bool IsPostFixExpr(tok::TokenKind tokenType);
  bool IsCurrentIn(TokenBitSet tokenSet);

  bool IsFirstInExternalDeclaration() const;
  bool IsFirstInFunctionDefinition() const;
  bool IsFirstInDeclaration() const;
  bool IsFirstInDeclarationSpecifier() const;
  bool IsFirstInSpecifierQualifier() const;
  bool IsFirstInDeclarator() const;
  bool IsFirstInDirectDeclarator() const;
  bool IsFirstInParameterTypeList() const;
  bool IsFirstInAbstractDeclarator() const;
  bool IsFirstInDirectAbstractDeclarator() const;
  bool IsFirstInParameterList() const;
  bool IsFirstInPointer() const;
  bool IsFirstInBlockItem() const;
  bool IsFirstInInitializer() const;
  bool IsFirstInInitializerList() const;
  bool IsFirstInStatement() const;
  bool IsFirstInExpr() const;
  bool IsFirstInAssignmentExpr() const;
  bool IsFirstInConditionalExpr() const;
  bool IsFirstInLogicalOrExpr() const;
  bool IsFirstInLogicalAndExpr() const;
  bool IsFirstInBitOrExpr() const;
  bool IsFirstInBitXorExpr() const;
  bool IsFirstInBitAndExpr() const;
  bool IsFirstInEqualExpr() const;
  bool IsFirstRelationalExpr() const;
  bool IsFirstInShiftExpr() const;
  bool IsFirstInAdditiveExpr() const;
  bool IsFirstInMultiExpr() const;
  bool IsFirstInTypeName() const;
  bool IsFirstInCastExpr() const;
  bool IsFirstInUnaryExpr() const;
  bool IsFirstInPostFixExpr() const;
  bool IsFirstInPrimaryExpr() const;

  void SetCheckTypedefType(bool state) {
    mIsCheckTypedefType = state;
  }

  [[nodiscard]] bool IsCheckTypedefType() const {
    return mIsCheckTypedefType;
  }

  template <class... Args>
  constexpr static TokenBitSet FormTokenKinds(Args&&... tokenKinds) {
    static_assert((std::is_same_v<std::decay_t<Args>, tok::TokenKind> && ...));
    return (TokenBitSet() | ... | TokenBitSet().set(tokenKinds, true));
  }

  void SkipTo(TokenBitSet recoveryToken, unsigned DiagID);

  std::string_view GetDeclaratorName(const Syntax::Declarator &declarator);
  const Syntax::DirectDeclaratorParamTypeList *
  GetFuncDeclarator(const Syntax::Declarator &declarator);
};
}
#endif // LCC_PARSER_H
