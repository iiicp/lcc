/***********************************
 * File:     DumpTool.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/5
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_DUMPTOOL_H
#define LCC_DUMPTOOL_H
#include "lcc/AST/AST.h"
namespace lcc::dump {

void dumpTokens(const std::vector<lcc::Token> &tokens);
void dumpAst(const Syntax::TranslationUnit &unit);

void visit(const Syntax::TranslationUnit &unit);
void visit(const Syntax::Declaration &declaration);
void visit(const Syntax::FunctionDefinition &functionDefinition);
void visit(const Syntax::DeclSpec &declarationSpecifiers);
void visit(const Syntax::Declarator &declarator);
void visit(const Syntax::AbstractDeclarator &abstractDeclarator);
void visit(const Syntax::InitializerList &initializerList);
void visit(const Syntax::Initializer &initializer);
void visit(const Syntax::StorageClsSpec &storageClassSpecifier);
void visit(const Syntax::TypeQualifier &typeQualifier);
void visit(const Syntax::TypeSpec &typeSpecifier);
void visit(const Syntax::FunctionSpecifier &functionSpecifier);
// void visit(const Syntax::SpecifierQualifiers &specifierQualifiers);
void visit(const Syntax::Pointer &pointer);
void visit(const Syntax::DirectDeclarator &directDeclarator);
void visit(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator);
void visit(const Syntax::ParamTypeList &paramTypeList);
void visit(const Syntax::ParamList &paramList);

void visit(const Syntax::Stmt &stmt);
void visit(const Syntax::BlockStmt &blockStmt);
void visit(const Syntax::BlockItem &blockItem);
void visit(const Syntax::IfStmt &ifStmt);
void visit(const Syntax::ForStmt &forStmt);
void visit(const Syntax::WhileStmt &whileStmt);
void visit(const Syntax::DoWhileStmt &doWhileStmt);
void visit(const Syntax::BreakStmt &breakStmt);
void visit(const Syntax::ContinueStmt &continueStmt);
void visit(const Syntax::SwitchStmt &switchStmt);
void visit(const Syntax::CaseStmt &caseStmt);
void visit(const Syntax::DefaultStmt &defaultStmt);
void visit(const Syntax::GotoStmt &gotoStmt);
void visit(const Syntax::LabelStmt &labelStmt);
void visit(const Syntax::ExprStmt &exprStmt);
void visit(const Syntax::ReturnStmt &returnStmt);

void visit(const Syntax::Expr &expr);
void visit(const Syntax::ConstantExpr &constantExpr);
void visit(const Syntax::AssignExpr &assignExpr);
void visit(const Syntax::CondExpr &conditionalExpr);
void visit(const Syntax::LogOrExpr &logOrExpr);
void visit(const Syntax::LogAndExpr &logAndExpr);
void visit(const Syntax::BitOrExpr &bitOrExpr);
void visit(const Syntax::BitXorExpr &bitXorExpr);
void visit(const Syntax::BitAndExpr &bitAndExpr);
void visit(const Syntax::EqualExpr &equalExpr);
void visit(const Syntax::RelationalExpr &relationalExpr);
void visit(const Syntax::ShiftExpr &shiftExpr);
void visit(const Syntax::AdditiveExpr &additiveExpr);
void visit(const Syntax::MultiExpr &multiExpr);
void visit(const Syntax::CastExpr &castExpr);
void visit(const Syntax::UnaryExpr &unaryExpr);
void visit(const Syntax::TypeName &typeName);
void visit(const Syntax::PostFixExpr &postFixExpr);
void visit(const Syntax::PrimaryExpr &primaryExpr);
}

#endif // LCC_DUMPTOOL_H
