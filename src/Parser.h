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
namespace lcc {
class Parser {
private:
  const SourceInterface& mSourceInterface;
  using TokIter = std::vector<CToken>::const_iterator;
  TokIter mTokCursor;
  TokIter mTokEnd;

public:
  explicit Parser(const CTokenObject & sourceObject): mSourceInterface(sourceObject), mTokCursor(sourceObject.data().cbegin()), mTokEnd(sourceObject.data().cend()) {}
  std::unique_ptr<Program> ParseProgram();

private:
  std::unique_ptr<Function> ParseFunction();
  std::unique_ptr<GlobalDecl> ParseGlobalDecl();
  std::unique_ptr<ConstantExpr> ParseConstantExpr();
  std::unique_ptr<Stmt> ParseStmt();
  std::unique_ptr<BlockStmt> ParseBlockStmt();
  std::unique_ptr<IfStmt> ParseIfStmt();
  std::unique_ptr<WhileStmt> ParseWhileStmt();
  std::unique_ptr<DoWhileStmt> ParseDoWhileStmt();
  std::unique_ptr<ForStmt> ParseForStmt();
  std::unique_ptr<ForDeclarationStmt> ParseForDeclStmt();
  std::unique_ptr<Declaration> ParseDeclStmt();
  std::unique_ptr<BreakStmt> ParseBreakStmt();
  std::unique_ptr<ContinueStmt> ParseContinueStmt();
  std::unique_ptr<ReturnStmt> ParseReturnStmt();
  std::unique_ptr<ExprStmt> ParseExprStmt();
  std::unique_ptr<Expr> ParseExpr();
  std::unique_ptr<AssignExpr> ParseAssignExpr();
  std::unique_ptr<ConditionalExpr> ParseConditionalExpr();
  std::unique_ptr<LogOrExpr> ParseLogOrExpr();
  std::unique_ptr<LogAndExpr> ParseLogAndExpr();
  std::unique_ptr<BitOrExpr> ParseBitOrExpr();
  std::unique_ptr<BitXorExpr> ParseBitXorExpr();
  std::unique_ptr<BitAndExpr> ParseBitAndExpr();
  std::unique_ptr<EqualExpr> ParseEqualExpr();
  std::unique_ptr<RelationalExpr> ParseRelationalExpr();
  std::unique_ptr<ShiftExpr> ParseShiftExpr();
  std::unique_ptr<AdditiveExpr> ParseAdditiveExpr();
  std::unique_ptr<MultiExpr> ParseMultiExpr();
  std::unique_ptr<CastExpr> ParseCastExpr();
  std::unique_ptr<UnaryExpr> ParseUnaryExpr();
  std::unique_ptr<PostFixExpr> ParsePostFixExpr();
  std::unique_ptr<PrimaryExpr> ParsePrimaryExpr();
  std::unique_ptr<Type> ParseType();
  std::unique_ptr<Type> ParseType(std::unique_ptr<Type> &&baseType);
  bool IsFunction();
  bool IsTypeName();
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
