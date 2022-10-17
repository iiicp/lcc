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
#include <vector>
#include "Token.h"
#include "Syntax.h"
namespace lcc::parser {
class Parser {
private:
  std::vector<lexer::Token> mTokens;
  using TokIter = std::vector<lexer::Token>::const_iterator;
  TokIter mTokCursor;
  TokIter mTokEnd;

public:
  explicit Parser(std::vector<lexer::Token> && tokens): mTokens(std::move(tokens)), mTokCursor(tokens.cbegin()), mTokEnd(tokens.cend()) {}
  std::unique_ptr<Program> Parse();

private:
  std::unique_ptr<Program> ParseProgram();
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
  std::unique_ptr<Type> ParseType();
  std::unique_ptr<Type> ParseType(std::unique_ptr<Type> &&baseType);
  bool IsFunction();
  bool IsTypeName();
  bool Match(lexer::TokenType tokenType);
  bool Expect(lexer::TokenType tokenType);
  bool Consume(lexer::TokenType tokenType);
  bool Peek(lexer::TokenType tokenType);
};
}
#endif // LCC_PARSER_H
