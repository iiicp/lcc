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
namespace lcc {
class Parser {
private:
  const SourceInterface& mSourceInterface;
  using TokIter = std::vector<CToken>::const_iterator;
  TokIter mTokCursor;
  TokIter mTokEnd;
public:
  class Scope {
  private:
    std::vector<std::set<std::string>> mCurrentScope;
    std::vector<std::set<std::string>> mTypedefs;
  public:
    Scope() {
      mCurrentScope.emplace_back();
      mTypedefs.emplace_back();
    }
    bool isTypedef(const std::string& name) const;
    void addToScope(std::string name);
    bool isInScope(const std::string& name) const;
    void pushScope();
    void popScope();
  };
private:
  Scope mScope;
public:
  explicit Parser(const CTokenObject & sourceObject): mSourceInterface(sourceObject), mTokCursor(sourceObject.data().cbegin()), mTokEnd(sourceObject.data().cend()) {}
  Syntax::TranslationUnit ParseTranslationUnit();

private:
  std::optional<Syntax::ExternalDeclaration> ParseExternalDeclaration();
  std::optional<Syntax::FunctionDefinition> ParseFunctionDefinition();
  std::optional<Syntax::Declaration> ParseDeclaration();
  std::optional<Syntax::DeclarationSpecifier> ParseDeclarationSpecifier();
  std::optional<Syntax::SpecifierQualifier> ParseSpecifierQualifier();
  std::optional<Syntax::Declarator> ParseDeclarator();
  std::optional<Syntax::DirectDeclarator> ParseDirectDeclarator();
  std::optional<Syntax::AbstractDeclarator> ParseAbstractDeclarator();
  std::optional<Syntax::DirectAbstractDeclarator> ParseDirectAbstractDeclarator();
  std::optional<Syntax::ParameterTypeList> ParseParameterTypeList();
  std::optional<Syntax::ParameterList> ParseParameterList();
  std::optional<Syntax::Pointer> ParsePointer();
  std::optional<Syntax::StructOrUnionSpecifier> ParseStructOrUnionSpecifier();
  std::optional<Syntax::EnumSpecifier> ParseEnumSpecifier();
  std::optional<Syntax::EnumDeclaration> ParseEnumDeclaration();
  std::optional<Syntax::Initializer> ParseInitializer();
  std::optional<Syntax::InitializerList> ParseInitializerList();

  std::optional<Syntax::BlockItem> ParseBlockItem();
  std::optional<Syntax::BlockStmt> ParseBlockStmt();
  std::optional<Syntax::Stmt> ParseStmt();
//  std::unique_ptr<BlockStmt> ParseBlockStmt();
//  std::unique_ptr<IfStmt> ParseIfStmt();
//  std::unique_ptr<WhileStmt> ParseWhileStmt();
//  std::unique_ptr<DoWhileStmt> ParseDoWhileStmt();
//  std::unique_ptr<ForStmt> ParseForStmt();
//  std::unique_ptr<ForDeclarationStmt> ParseForDeclStmt();
//  std::unique_ptr<BreakStmt> ParseBreakStmt();
//  std::unique_ptr<ContinueStmt> ParseContinueStmt();
//  std::unique_ptr<ReturnStmt> ParseReturnStmt();
//  std::unique_ptr<ExprStmt> ParseExprStmt();

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
  bool IsFunction();
  bool IsTypeName();
  bool IsDeclarationSpecifier();
  bool IsSpecifierQualifier();
  bool isAssignment(tok::TokenKind type);
  bool Match(tok::TokenKind tokenType);
  bool Expect(tok::TokenKind tokenType);
  bool Consume(tok::TokenKind tokenType);
  bool ConsumeAny();
  bool Peek(tok::TokenKind tokenType);
  bool IsUnaryOp(tok::TokenKind tokenType);
  bool IsPostFixExpr(tok::TokenKind tokenType);
};
}
#endif // LCC_PARSER_H
