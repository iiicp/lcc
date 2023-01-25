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
#include "Token.h"
#include "Syntax.h"
#include <vector>
#include <set>
#include <string>
#include <optional>
#include <map>
#include <unordered_map>
#include "Semantics.h"
namespace lcc {
class Parser {
private:
  const SourceInterface& mSourceInterface;
  using TokIter = std::vector<CToken>::const_iterator;
  TokIter mTokCursor;
  TokIter mTokEnd;
  std::set<tok::TokenKind> mFirstPostFixSet;
  std::set<tok::TokenKind> mAssignmentSet;
  std::set<tok::TokenKind> mFirstSpecifierQualifierSet;
  std::set<tok::TokenKind> mFirstDeclarationSpecifierSet;
  std::set<tok::TokenKind> mFirstPointerSet;
  std::set<tok::TokenKind> mFirstParameterListSet;
  std::set<tok::TokenKind> mFirstDirectAbstractDeclaratorSet;
  std::set<tok::TokenKind> mFirstAbstractDeclaratorSet;
  std::set<tok::TokenKind> mFirstParameterTypeListSet;
  std::set<tok::TokenKind> mFirstDirectDeclaratorSet;
  std::set<tok::TokenKind> mFirstDeclaratorSet;
  std::set<tok::TokenKind> mFirstDeclarationSet;
  std::set<tok::TokenKind> mFirstExpressionSet;
  std::set<tok::TokenKind> mFirstInitializerSet;
  std::set<tok::TokenKind> mFirstInitializerListSet;
  std::set<tok::TokenKind> mFirstStatementSet;
  std::set<tok::TokenKind> mFirstBlockItem;
  std::set<tok::TokenKind> mFirstFunctionDefinitionSet;
  std::set<tok::TokenKind> mFirstExternalDeclarationSet;
public:
  class Scope {
  private:
    struct Symbol {
      std::string_view identifier;
      bool isTypedef{};
    };
    std::vector<std::unordered_map<std::string_view, Symbol>> mCurrentScope;
  public:
    Scope() {
      mCurrentScope.emplace_back();
    }
    void addTypedef(std::string_view name);
    bool isTypedef(std::string_view name) const;
    bool isTypedefInScope(std::string_view name) const;
    void addToScope(std::string_view name);
    void pushScope();
    void popScope();
  };
private:
  Scope mScope;
public:
  explicit Parser(const CTokenObject & sourceObject);
  Syntax::TranslationUnit ParseTranslationUnit();

private:
  std::optional<Syntax::ExternalDeclaration> ParseExternalDeclaration();
  std::optional<Syntax::FunctionDefinition> ParseFunctionDefinition();
  std::optional<Syntax::Declaration> ParseDeclaration();
  std::optional<Syntax::DeclarationSpecifier> ParseDeclarationSpecifier();
  std::optional<Syntax::SpecifierQualifier> ParseSpecifierQualifier();
  std::optional<Syntax::Declarator> ParseDeclarator();
  std::optional<Syntax::DirectDeclaratorSquare> ParseDirectDeclaratorSquare(
      Syntax::DirectDeclarator& declarator);
  std::optional<Syntax::DirectDeclarator> ParseDirectDeclarator();
  std::optional<Syntax::AbstractDeclarator> ParseAbstractDeclarator();
  std::optional<Syntax::DirectAbstractDeclarator> ParseDirectAbstractDeclarator();
  std::optional<Syntax::ParameterTypeList> ParseParameterTypeList();
  std::optional<Syntax::ParameterList> ParseParameterList();
  std::optional<Syntax::ParameterDeclaration> ParseParameterDeclaration();
  std::optional<Syntax::Pointer> ParsePointer();
  std::optional<Syntax::StructOrUnionSpecifier> ParseStructOrUnionSpecifier();
  std::optional<Syntax::EnumSpecifier> ParseEnumSpecifier();
  std::optional<Syntax::EnumDeclaration> ParseEnumDeclaration(std::string enumName);
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
  std::optional<Syntax::ConditionalExpr> ParseConditionalExpr();
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
  std::optional<Syntax::PrimaryExpr> ParsePrimaryExpr();

  std::optional<Syntax::TypeName> ParseTypeName();
  bool IsDeclarationSpecifier();
  bool IsSpecifierQualifier();
  bool IsAssignment(tok::TokenKind type);
  bool Match(tok::TokenKind tokenType);
  bool Expect(tok::TokenKind tokenType);
  bool Consume(tok::TokenKind tokenType);
  bool ConsumeAny();
  bool Peek(tok::TokenKind tokenType);
  bool IsUnaryOp(tok::TokenKind tokenType);
  bool IsPostFixExpr(tok::TokenKind tokenType);

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
};
}
#endif // LCC_PARSER_H
