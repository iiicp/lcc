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
#include "Syntax.h"
namespace lcc::dump {

void dumpTokens(const std::vector<lcc::Token> &tokens);
void dumpAst(const Syntax::TranslationUnit &unit);

void visitor(const Syntax::TranslationUnit &unit);
void visitor(const Syntax::Declaration &declaration);
void visitor(const Syntax::FunctionDefinition &functionDefinition);
void visitor(const Syntax::DeclarationSpecifiers &declarationSpecifiers);
void visitor(const Syntax::Declarator &declarator);
void visitor(const Syntax::AbstractDeclarator &abstractDeclarator);
void visitor(const Syntax::InitializerList &initializerList);
void visitor(const Syntax::Initializer &initializer);
void visitor(const Syntax::StorageClassSpecifier &storageClassSpecifier);
void visitor(const Syntax::TypeQualifier &typeQualifier);
void visitor(const Syntax::TypeSpecifier &typeSpecifier);
void visitor(const Syntax::FunctionSpecifier &functionSpecifier);
//void visitor(const Syntax::SpecifierQualifiers &specifierQualifiers);
void visitor(const Syntax::Pointer &pointer);
void visitor(const Syntax::DirectDeclarator &directDeclarator);
void visitor(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator);
void visitor(const Syntax::ParamTypeList &paramTypeList);
void visitor(const Syntax::ParamList &paramList);

void visitor(const Syntax::Stmt &stmt);
void visitor(const Syntax::BlockStmt &blockStmt);
void visitor(const Syntax::BlockItem &blockItem);
void visitor(const Syntax::IfStmt &ifStmt);
void visitor(const Syntax::ForStmt &forStmt);
void visitor(const Syntax::WhileStmt &whileStmt);
void visitor(const Syntax::DoWhileStmt &doWhileStmt);
void visitor(const Syntax::BreakStmt &breakStmt);
void visitor(const Syntax::ContinueStmt &continueStmt);
void visitor(const Syntax::SwitchStmt &switchStmt);
void visitor(const Syntax::CaseStmt &caseStmt);
void visitor(const Syntax::DefaultStmt &defaultStmt);
void visitor(const Syntax::GotoStmt &gotoStmt);
void visitor(const Syntax::LabelStmt &labelStmt);
void visitor(const Syntax::ExprStmt &exprStmt);
void visitor(const Syntax::ReturnStmt &returnStmt);

void visitor(const Syntax::Expr &expr);
void visitor(const Syntax::ConstantExpr &constantExpr);
void visitor(const Syntax::AssignExpr &assignExpr);
void visitor(const Syntax::ConditionalExpr &conditionalExpr);
void visitor(const Syntax::LogOrExpr &logOrExpr);
void visitor(const Syntax::LogAndExpr &logAndExpr);
void visitor(const Syntax::BitOrExpr &bitOrExpr);
void visitor(const Syntax::BitXorExpr &bitXorExpr);
void visitor(const Syntax::BitAndExpr &bitAndExpr);
void visitor(const Syntax::EqualExpr &equalExpr);
void visitor(const Syntax::RelationalExpr &relationalExpr);
void visitor(const Syntax::ShiftExpr &shiftExpr);
void visitor(const Syntax::AdditiveExpr &additiveExpr);
void visitor(const Syntax::MultiExpr &multiExpr);
void visitor(const Syntax::CastExpr &castExpr);
void visitor(const Syntax::UnaryExpr &unaryExpr);
void visitor(const Syntax::TypeName &typeName);
void visitor(const Syntax::PostFixExpr &postFixExpr);
void visitor(const Syntax::PrimaryExpr &primaryExpr);
}

#endif // LCC_DUMPTOOL_H
