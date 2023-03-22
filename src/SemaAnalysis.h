/***********************************
 * File:     SemaAnalysis.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/9
 ***********************************/

#ifndef LCC_SEMAANALYSIS_H
#define LCC_SEMAANALYSIS_H
#include "Syntax.h"
#include "SemaNode.h"
#include "Diagnostic.h"
#include <vector>
#include <map>
namespace lcc {
class SemaAnalysis {
private:
  const std::vector<Token>& mTokens;
  DiagnosticEngine &Diag;
public:
  explicit SemaAnalysis(const std::vector<Token> &tokens, DiagnosticEngine &diag);
  Sema::TranslationUnit Analyse(const Syntax::TranslationUnit &node);
private:

  Sema::TranslationUnit visit(const Syntax::TranslationUnit &node);
  std::vector<Sema::TranslationUnit::Variant> visit(const Syntax::FunctionDefinition &node);
  std::vector<Sema::TranslationUnit::Variant> visit(const Syntax::Declaration &node);

  void visit(const Syntax::BlockItem &node);
  void visit(const Syntax::Stmt &node);
  void visit(const Syntax::IfStmt &node);
  void visit(const Syntax::SwitchStmt &node);
  void visit(const Syntax::CaseStmt &node);
  void visit(const Syntax::DefaultStmt &node);
  void visit(const Syntax::WhileStmt &node);
  void visit(const Syntax::DoWhileStmt &node);
  void visit(const Syntax::ForStmt &node);
  void visit(const Syntax::BreakStmt &node);
  void visit(const Syntax::ContinueStmt &node);
  void visit(const Syntax::ReturnStmt &node);
  void visit(const Syntax::LabelStmt &node);
  void visit(const Syntax::GotoStmt &node);
  void visit(const Syntax::ExprStmt &node);

  void visit(const Syntax::Expr &node);
  void visit(const Syntax::ConditionalExpr &node);
  void visit(const Syntax::LogOrExpr &node);
  void visit(const Syntax::LogAndExpr &node);
  void visit(const Syntax::BitOrExpr &node);
  void visit(const Syntax::BitAndExpr &node);
  void visit(const Syntax::EqualExpr &node);
  void visit(const Syntax::RelationalExpr &node);
  void visit(const Syntax::ShiftExpr &node);
  void visit(const Syntax::AdditiveExpr &node);
  void visit(const Syntax::MultiExpr &node);
  void visit(const Syntax::CastExpr &node);
  void visit(const Syntax::UnaryExpr &node);
  void visit(const Syntax::UnaryExprPostFixExpr &node);
  void visit(const Syntax::UnaryExprUnaryOperator &node);
  void visit(const Syntax::UnaryExprSizeOf &node);
  void visit(const Syntax::PostFixExpr &node);
  void visit(const Syntax::PostFixExprArrow &node);
  void visit(const Syntax::PostFixExprDot &node);
  void visit(const Syntax::PostFixExprIncrement &node);
  void visit(const Syntax::PostFixExprDecrement &node);
  void visit(const Syntax::PostFixExprFuncCall &node);
  void visit(const Syntax::PostFixExprPrimaryExpr &node);
  void visit(const Syntax::PostFixExprSubscript &node);
  void visit(const Syntax::PostFixExprTypeInitializer &node);
  void visit(const Syntax::PrimaryExpr &node);
  void visit(const Syntax::PrimaryExprConstant &node);
  void visit(const Syntax::PrimaryExprIdent &node);
  void visit(const Syntax::PrimaryExprParentheses &node);
};
}

#endif // LCC_SEMAANALYSIS_H
