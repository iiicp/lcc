/***********************************
 * File:     Syntax.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/13
 *
 * Sign:     enjoy life
 ***********************************/
#include "Syntax.h"
#include <algorithm>
#include <cassert>
#include <utility>
namespace lcc {

/// expr
Expr::Expr(const SourceInterface &interface, const CToken &curToken,
           std::vector<AssignExpr> assignExpressions)
    : Node(interface, curToken),
      mAssignExpressions(std::move(assignExpressions)) {
  assert(!mAssignExpressions.empty());
}

const std::vector<AssignExpr> &Expr::getAssignExpressions() const {
  return mAssignExpressions;
}

PrimaryExprIdentifier::PrimaryExprIdentifier(const SourceInterface &interface,
                                             const CToken &curToken,
                                             std::string identifier)
    : Node(interface, curToken), mIdentifier(std::move(identifier)) {}

const std::string &PrimaryExprIdentifier::getIdentifier() const {
  return mIdentifier;
}

PrimaryExprConstant::PrimaryExprConstant(const SourceInterface &interface,
                                         const CToken &curToken,
                                         Variant variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const Variant &PrimaryExprConstant::getValue() const { return mVariant; }

PrimaryExprParent::PrimaryExprParent(const SourceInterface &interface,
                                     const CToken &curToken, Expr &&expr)
    : Node(interface, curToken), mExpr(std::move(expr)) {}

const Expr &PrimaryExprParent::getExpr() const { return mExpr; }

PrimaryExpr::PrimaryExpr(const SourceInterface &interface,
                         const CToken &curToken, Variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const Variant &PrimaryExpr::getVariant() const { return mVariant; }

PostFixExprPrimary::PostFixExprPrimary(const SourceInterface &interface,
                                       const CToken &curToken,
                                       PrimaryExpr &&primaryExpr)
    : Node(interface, curToken), mPrimaryExpr(std::move(primaryExpr)) {}

const PostFixExprPrimary &PostFixExprPrimary::getPrimaryExpr() const {
  return mPrimaryExpr;
}

PostFixExprSubscript::PostFixExprSubscript(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)),
      mExpr(std::move(expr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprSubscript::getPostFixExpr() const {
  return *mPostFixExpr;
}

const Expr &PostFixExprSubscript::getExpr() const { return mExpr; }

PostFixExprIncrement::PostFixExprIncrement(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprIncrement::getPostFixExpr() const {
  return *mPostFixExpr;
}

PostFixExprDecrement::PostFixExprDecrement(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprDecrement::getPostFixExpr() const {
  return *mPostFixExpr;
}

PostFixExprDot::PostFixExprDot(const SourceInterface &interface,
                               const CToken &curToken,
                               std::unique_ptr<PostFixExpr> &&postFixExpr,
                               std::string identifier)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprDot::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::string &PostFixExprDot::getIdentifier() const { return mIdentifier; }

PostFixExprArrow::PostFixExprArrow(const SourceInterface &interface,
                                   const CToken &curToken,
                                   std::unique_ptr<PostFixExpr> &&postFixExpr,
                                   std::string identifier)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}

const PostFixExpr &PostFixExprArrow::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::string &PostFixExprArrow::getIdentifier() const {
  return mIdentifier;
}

PostFixExprFuncCall::PostFixExprFuncCall(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<PostFixExpr> &&postFixExpr,
    std::vector<std::unique_ptr<AssignExpr>> &&optParams)
    : Node(interface, curToken), mPostFixExpr(std::move(postFixExpr)),
      mOptParams(std::move(optParams)) {}

const PostFixExpr &PostFixExprFuncCall::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::vector<std::unique_ptr<AssignExpr>> &
PostFixExprFuncCall::getOptionalAssignExpressions() const {
  return mOptParams;
}

PostFixExpr::PostFixExpr(const SourceInterface &interface,
                         const CToken &curToken, Variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const Variant &PostFixExpr::getVariant() const { return mVariant; }

UnaryExprPostFixExpr::UnaryExprPostFixExpr(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<PostFixExpr> &&postExpr)
    : Node(interface, curToken), mPostExpr(std::move(postExpr)) {}

const PostFixExpr &UnaryExprPostFixExpr::getPostExpr() const {
  return mPostExpr;
}

UnaryExprUnaryOperator::UnaryExprUnaryOperator(
    const SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<UnaryExpr> &&unaryExpr)
    : Node(interface, curToken), mTok(curToken.getTokenKind()),
      mUnaryExpr(std::move(unaryExpr)) {}

tok::TokenKind UnaryExprUnaryOperator::getOperator() const { return mTok; }

const UnaryExpr &UnaryExprUnaryOperator::getUnaryExpr() const {
  return *mUnaryExpr;
}

UnaryExprSizeOf::UnaryExprSizeOf(const SourceInterface &interface,
                                 const CToken &curToken, Variant &&variant)
    : Node(interface, curToken), mValue(std::move(variant)) {}

const Variant &UnaryExprSizeOf::getVariant() const { return mValue; }

UnaryExpr::UnaryExpr(const SourceInterface &interface, const CToken &curToken,
                     Variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const Variant &UnaryExpr::getVariant() const { return mVariant; }

AssignExprAssign::AssignExprAssign(const SourceInterface &interface,
                                   const CToken &curToken,
                                   UnaryExpr &&unaryExpr,
                                   std::unique_ptr<AssignExpr> &&assignExpr)
    : Node(interface, curToken), mUnaryExpr(std::move(unaryExpr)),
      mAssignExpr(std::move(assignExpr)) {
  assert(mAssignExpr);
}

const UnaryExpr &AssignExprAssign::getUnaryExpr() const { return mUnaryExpr; }

const AssignExpr &AssignExprAssign::getAssignExpr() const {
  return *mAssignExpr;
}

TypeName::TypeName(const SourceInterface &interface, const CToken &curToken,
                   std::vector<SpecifierQualifier> &&specifierQualifiers,
                   std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : Node(interface, curToken),
      mSpecifierQualifiers(std::move(specifierQualifiers)),
      mAbstractDeclarator(std::move(abstractDeclarator)) {}

const std::vector<SpecifierQualifier> &
TypeName::getSpecifierQualifiers() const {
  return mSpecifierQualifiers;
}

const AbstractDeclarator *TypeName::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

CastExpr::CastExpr(const SourceInterface &interface, const CToken &curToken,
                   Variant &&unaryOrCast)
    : Node(interface, curToken), mVariant(std::move(unaryOrCast)) {}

const Variant &CastExpr::getVariant() const { return mVariant; }

MultiExpr::MultiExpr(
    SourceInterface &interface, const CToken &curToken, CastExpr &&castExpr,
    std::vector<std::pair<tok::TokenKind, CastExpr>> &&optCastExps)
    : Node(interface, curToken), mCastExpr(std::move(castExpr)),
      mOptCastExps(std::move(optCastExps)) {}

const CastExpr &MultiExpr::getCastExpr() const { return mCastExpr; }

const std::vector<std::pair<tok::TokenKind, CastExpr>> &
MultiExpr::getOptionalCastExpr() const {
  return mOptCastExps;
}

AdditiveExpr::AdditiveExpr(
    SourceInterface &interface, const CToken &curToken, MultiExpr &&multiExpr,
    std::vector<std::pair<tok::TokenKind, MultiExpr>> &&optionalMultiExps)
    : Node(interface, curToken), mMultiExpr(std::move(multiExpr)),
      mOptionalMultiExpr(std::move(optionalMultiExps)) {}

const MultiExpr &AdditiveExpr::getMultiExpr() const { return mMultiExpr; }

const std::vector<std::pair<tok::TokenKind, MultiExpr>> &
AdditiveExpr::getOptionalMultiExpr() const {
  return mOptionalMultiExpr;
}

ShiftExpr::ShiftExpr(
    SourceInterface &interface, const CToken &curToken,
    AdditiveExpr &&additiveExpr,
    std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &&optAdditiveExps)
    : Node(interface, curToken), mAdditiveExpr(std::move(additiveExpr)),
      mOptAdditiveExps(std::move(optAdditiveExps)) {}

const AdditiveExpr &ShiftExpr::getAdditiveExpr() const { return mAdditiveExpr; }

const std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &
ShiftExpr::getOptAdditiveExps() const {
  return mOptAdditiveExps;
}

RelationalExpr::RelationalExpr(
    SourceInterface &interface, const CToken &curToken, ShiftExpr &&shiftExpr,
    std::vector<std::pair<tok::TokenKind, ShiftExpr>> &&optShiftExps)
    : Node(interface, curToken), mShiftExpr(std::move(shiftExpr)),
      mOptShiftExps(std::move(optShiftExps)) {}

const ShiftExpr &RelationalExpr::getShiftExpr() const { return mShiftExpr; }

const std::vector<std::pair<tok::TokenKind, ShiftExpr>> &
RelationalExpr::getOptionalShiftExpressions() const {
  return mOptShiftExps;
}

EqualExpr::EqualExpr(
    SourceInterface &interface, const CToken &curToken,
    RelationalExpr &&relationalExpr,
    std::vector<std::pair<tok::TokenKind, RelationalExpr>> &&optRelationalExps)
    : Node(interface, curToken), mRelationalExpr(std::move(relationalExpr)),
      mOptRelationExps(std::move(optRelationalExps)) {}

const RelationalExpr &EqualExpr::getRelationalExpr() const {
  return mRelationalExpr;
}

const std::vector<std::pair<tok::TokenKind, RelationalExpr>> &
EqualExpr::getOptionalRelationalExpr() const {
  return mOptRelationExps;
}

LogAndExpr::LogAndExpr(SourceInterface &interface, const CToken &curToken,
                       BitOrExpr &&bitOrExpr,
                       std::vector<BitOrExpr> &&optBitOrExps)
    : Node(interface, curToken), mBitOrExpr(std::move(bitOrExpr)),
      mOptBitOrExps(std::move(optBitOrExps)) {}

const BitOrExpr &LogAndExpr::getBitOrExpression() const { return mBitOrExpr; }

const std::vector<BitOrExpr> &LogAndExpr::getOptionalBitOrExpressions() const {
  return mOptBitOrExps;
}

BitAndExpr::BitAndExpr(SourceInterface &interface, const CToken &curToken,
                       EqualExpr &&equalExpr,
                       std::vector<EqualExpr> &&optEqualExps)
    : Node(interface, curToken), mEqualExpr(std::move(equalExpr)),
      mOptEqualExps(std::move(optEqualExps)) {}

const EqualExpr &BitAndExpr::getEqualExpr() const { return mEqualExpr; }

const std::vector<EqualExpr> &BitAndExpr::getOptionalEqualExpr() const {
  return mOptEqualExps;
}

BitXorExpr::BitXorExpr(SourceInterface &interface, const CToken &curToken,
                       BitAndExpr &&bitAndExpr,
                       std::vector<BitAndExpr> &&optBitAndExps)
    : Node(interface, curToken), mBitAndExpr(std::move(bitAndExpr)),
      mOptBitAndExps(std::move(optBitAndExps)) {}

const BitAndExpr &BitXorExpr::getBitAndExpr() const { return mBitAndExpr; }

const std::vector<BitAndExpr> &
BitXorExpr::getOptionalBitAndExpressions() const {
  return mOptBitAndExps;
}

BitOrExpr::BitOrExpr(SourceInterface &interface, const CToken &curToken,
                     BitXorExpr &&bitXorExpr,
                     std::vector<BitXorExpr> &&optBitXorExps)
    : Node(interface, curToken), mBitXorExpr(std::move(bitXorExpr)),
      mOptBitXorExps(std::move(optBitXorExps)) {}

const BitXorExpr &BitOrExpr::getBitXorExpression() const { return mBitXorExpr; }

const std::vector<BitXorExpr> &BitOrExpr::getOptionalBitXorExpressions() const {
  return mOptBitXorExps;
}

LogOrExpr::LogOrExpr(SourceInterface &interface, const CToken &curToken,
                     LogAndExpr &&logAndExpr,
                     std::vector<LogAndExpr> &&optLogAndExps)
    : Node(interface, curToken), mLogAndExpr(std::move(logAndExpr)),
      mOptLogAndExps(std::move(optLogAndExps)) {}

const LogOrExpr &LogOrExpr::getAndExpression() const { return mLogAndExpr; }

const std::vector<LogOrExpr> &LogOrExpr::getOptionalAndExpressions() const {
  return mOptLogAndExps;
}

ConditionalExpr::ConditionalExpr(SourceInterface &interface,
                                 const CToken &curToken, LogOrExpr &&logOrExpr,
                                 std::unique_ptr<Expr> &&optExpr,
                                 std::unique_ptr<ConditionalExpr> &&optCondExpr)
    : Node(interface, curToken), mLogOrExpr(std::move(logOrExpr)),
      mOptExpr(std::move(optExpr)), mOptCondExpr(std::move(optCondExpr)) {}

const LogOrExpr &ConditionalExpr::getLogicalOrExpression() const {
  return mLogOrExpr;
}

const Expr *ConditionalExpr::getOptionalExpression() const {
  return mOptExpr.get();
}

const ConditionalExpr *
ConditionalExpr::getOptionalConditionalExpression() const {
  return mOptCondExpr.get();
}

AssignExpr::AssignExpr(
    SourceInterface &interface, const CToken &curToken,
    std::variant<AssignExprAssign, ConditionalExpr> &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const std::variant<AssignExprAssign, ConditionalExpr> &
AssignExpr::getVariant() const {
  return mVariant;
}

/// stmt
ReturnStmt::ReturnStmt(SourceInterface &interface, const CToken &curToken,
                       std::unique_ptr<Expr> &&optExpr)
    : Node(interface, curToken), mOptExpr(std::move(optExpr)) {}

const Expr *ReturnStmt::getExpression() const { return mOptExpr.get(); }

IfStmt::IfStmt(SourceInterface &interface, const CToken &curToken, Expr &&expr,
               std::unique_ptr<Stmt> &&thenStmt,
               std::unique_ptr<Stmt> &&optElseStmt)
    : Node(interface, curToken), mExpr(std::move(expr)),
      mThenStmt(std::move(thenStmt)), mOptElseStmt(std::move(optElseStmt)) {
  assert(mThenStmt);
}

const Expr &IfStmt::getExpression() const { return mExpr; }

const Stmt &IfStmt::getThenStmt() const { return *mThenStmt; }

const Stmt *IfStmt::getElseStmt() const { return mOptElseStmt.get(); }

SwitchStmt::SwitchStmt(SourceInterface &interface, const CToken &curToken,
                       Expr &&expression, std::unique_ptr<Stmt> &&statement)
    : Node(interface, curToken), mExpr(std::move(expression)),
      mStmt(std::move(statement)) {
  assert(mStmt);
}

const Expr &SwitchStmt::getExpression() const { return mExpr; }

const Stmt &SwitchStmt::getStatement() const { return mStmt; }

DefaultStmt::DefaultStmt(SourceInterface &interface, const CToken &curToken,
                         std::unique_ptr<Stmt> &&statement)
    : Node(interface, curToken), mStmt(std::move(statement)) {
  assert(mStmt);
}

const Stmt &DefaultStmt::getStatement() const { return *mStmt; }

CaseStmt::CaseStmt(SourceInterface &interface, const CToken &curToken,
                   const constantVariant &constant,
                   std::unique_ptr<Stmt> &&statement)
    : Node(interface, curToken), mConstant(constant),
      mStatement(std::move(statement)) {}

const Stmt *CaseStmt::getStatement() const { return mStatement.get(); }

const constantVariant &CaseStmt::getConstant() const { return mConstant; }

GotoStmt::GotoStmt(SourceInterface &interface, const CToken &curToken,
                   std::string identifier)
    : Node(interface, curToken), mIdentifier(std::move(identifier)) {}

const std::string &GotoStmt::getIdentifier() const { return mIdentifier; }

LabelStmt::LabelStmt(SourceInterface &interface, const CToken &curToken,
                     std::string identifier)
    : Node(interface, curToken), mIdentifier(std::move(identifier)) {}

const std::string &LabelStmt::getIdentifier() const { return mIdentifier; }

WhileStmt::WhileStmt(SourceInterface &interface, const CToken &curToken,
                     Expr &&expr, std::unique_ptr<Stmt> &&stmt)
    : Node(interface, curToken), mStmt(std::move(stmt)) {
  assert(mStmt);
}

const Expr &WhileStmt::getExpression() const { return mExpr; }

const Stmt &WhileStmt::getStatement() const { return *mStmt; }

DoWhileStmt::DoWhileStmt(SourceInterface &interface, const CToken &curToken,
                         std::unique_ptr<Stmt> &&stmt, Expr &&expr)
    : Node(interface, curToken), mStmt(std::move(stmt)) {
  assert(mStmt);
}
const Expr &DoWhileStmt::getExpression() const { return mExpr; }

const Stmt &DoWhileStmt::getStatement() const { return *mStmt; }

ForStmt::ForStmt(SourceInterface &interface, const CToken &curToken,
                 std::unique_ptr<Stmt> &&stmt, std::unique_ptr<Expr> &&initExpr,
                 std::unique_ptr<Expr> &&controlExpr,
                 std::unique_ptr<Expr> &&postExpr)
    : Node(interface, curToken), mStmt(std::move(stmt)),
      mInitExpr(std::move(initExpr)), mControlExpr(std::move(controlExpr)),
      mPostExpr(std::move(postExpr)) {
  assert(mStmt);
}

const Stmt &ForStmt::getStatement() const { return *mStmt; }

const Expr *ForStmt::getInitial() const { return mInitExpr.get(); }

const Expr *ForStmt::getControlling() const { return mControlExpr.get(); }

const Expr *ForStmt::getPost() const { return mPostExpr.get(); }

ForDeclarationStmt::ForDeclarationStmt(SourceInterface &interface,
                                       const CToken &curToken,
                                       std::unique_ptr<Stmt> &&stmt,
                                       Declaration &&initDecl,
                                       std::unique_ptr<Expr> &&controlExpr,
                                       std::unique_ptr<Expr> &&postExpr)
    : Node(interface, curToken), mStmt(std::move(stmt)),
      mInitDecl(std::move(initDecl)), mControlExpr(std::move(controlExpr)),
      mPostExpr(std::move(postExpr)) {
  assert(mStmt);
}

const Declaration &ForDeclarationStmt::getInitial() const { return mInitDecl; }

const Expr *ForDeclarationStmt::getControlling() const {
  return mControlExpr.get();
}

const Expr *ForDeclarationStmt::getPost() const { return mPostExpr.get(); }

const Stmt &ForDeclarationStmt::getStatement() const { return *mStmt; }

ExprStmt::ExprStmt(SourceInterface &interface, const CToken &curToken,
                   std::unique_ptr<Expr> &&optExpr)
    : Node(interface, curToken), mOptExpr(std::move(optExpr)) {}

const Expr *ExprStmt::getOptionalExpression() const { return mOptExpr.get(); }

BreakStmt::BreakStmt(SourceInterface &interface, const CToken &curToken)
    : Node(interface, curToken) {}

ContinueStmt::ContinueStmt(SourceInterface &interface, const CToken &curToken)
    : Node(interface, curToken) {}

BlockItem::BlockItem(SourceInterface &interface, const CToken &curToken,
                     variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &BlockItem::getVariant() const { return mVariant; }

variant &BlockItem::getVariant() { return mVariant; }

BlockStmt::BlockStmt(SourceInterface &interface, const CToken &curToken,
                     std::vector<BlockItem> &&blockItems)
    : Node(interface, curToken), mBlockItems(std::move(blockItems)) {}

const std::vector<BlockItem> &BlockStmt::getBlockItems() const {
  return mBlockItems;
}

Stmt::Stmt(SourceInterface &interface, const CToken &curToken,
           variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &Stmt::getVariant() const { return mVariant; }
variant &Stmt::getVariant() { return mVariant; }

/// decl
Declaration::Declaration(
    SourceInterface &interface, const CToken &curToken,
    std::vector<DeclarationSpecifier> &&declarationSpecifiers,
    std::vector<std::pair<std::unique_ptr<Declarator>,
                          std::unique_ptr<Initializer>>> &&initDeclarators)
    : Node(interface, curToken),
      mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mInitDeclarators(std::move(initDeclarators)) {}

const std::vector<DeclarationSpecifier> &
Declaration::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}

const std::vector<
    std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> &
Declaration::getInitDeclarators() const {
  return mInitDeclarators;
}

DirectAbstractDeclaratorAssignmentExpression::
    DirectAbstractDeclaratorAssignmentExpression(
        SourceInterface &interface, const CToken &curToken,
        std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
        std::unique_ptr<AssignExpr> &&assignmentExpression)
    : Node(interface, curToken),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mAssignmentExpression(std::move(assignmentExpression)) {}

const DirectAbstractDeclarator *
DirectAbstractDeclaratorAssignmentExpression::getDirectAbstractDeclarator()
    const {
  return mDirectAbstractDeclarator.get();
}

const AssignExpr *
DirectAbstractDeclaratorAssignmentExpression::getAssignmentExpression() const {
  return mAssignmentExpression.get();
}

DirectAbstractDeclaratorParameterTypeList::
    DirectAbstractDeclaratorParameterTypeList(
        SourceInterface &interface, const CToken &curToken,
        std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
        std::unique_ptr<ParameterTypeList> &&parameterTypeList)
    : Node(interface, curToken),
      mDirectAbstractDeclarator(
          std::move(directAbstractDeclarator),
          mParameterTypeList(std::move(parameterTypeList))) {}

const DirectAbstractDeclarator *
DirectAbstractDeclaratorParameterTypeList::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

const ParameterTypeList *
DirectAbstractDeclaratorParameterTypeList::getParameterTypeList() const {
  return mParameterTypeList.get();
}

DirectAbstractDeclarator::DirectAbstractDeclarator(SourceInterface &interface,
                                                   const CToken &curToken,
                                                   variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &DirectAbstractDeclarator::getVariant() const { return mVariant; }

AbstractDeclarator::AbstractDeclarator(
    SourceInterface &interface, const CToken &curToken,
    std::vector<Pointer> &&pointers,
    DirectAbstractDeclarator &&directAbstractDeclarator)
    : Node(interface, curToken), mPointers(std::move(pointers)),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

const std::vector<Pointer> &AbstractDeclarator::getPointers() const {
  return mPointers;
}

const DirectAbstractDeclarator &
AbstractDeclarator::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator;
}

ParameterList::ParameterList(SourceInterface &interface, const CToken &curToken,
                             std::vector<ParameterDeclaration> &&parameterList)
    : Node(interface, curToken), mParameterList(std::move(parameterList)) {}

const std::vector<ParameterDeclaration> &
ParameterList::getParameterDeclarations() const {
  return mParameterList;
}

ParameterTypeList::ParameterTypeList(SourceInterface &interface,
                                     const CToken &curToken,
                                     ParameterList &&parameterList,
                                     bool hasEllipse)
    : Node(interface, curToken), mParameterList(std::move(parameterList)),
      mHasEllipse(hasEllipse) {}

const ParameterList &ParameterTypeList::getParameterList() const {
  return mParameterList;
}

bool ParameterTypeList::hasEllipse() const { return mHasEllipse; }

DirectDeclaratorParentParameters::DirectDeclaratorParentParameters(
    SourceInterface &interface, const CToken &curToken,
    DirectDeclarator &&directDeclarator, ParameterTypeList &&parameterTypeList)
    : Node(interface, curToken), mDirectDeclarator(std::move(directDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

const DirectDeclarator &
DirectDeclaratorParentParameters::getDirectDeclarator() const {
  return mDirectDeclarator;
}

const ParameterTypeList &
DirectDeclaratorParentParameters::getParameterTypeList() const {
  return mParameterTypeList;
}

DirectDeclaratorParentIdentifiers::DirectDeclaratorParentIdentifiers(
    SourceInterface &interface, const CToken &curToken,
    DirectDeclarator &&directDeclarator, std::vector<std::string> &&identifiers)
    : Node(interface, curToken), mDirectDeclarator(std::move(directDeclarator)),
      mIdentifiers(std::move(identifiers)) {}

const DirectDeclarator &
DirectDeclaratorParentIdentifiers::getDirectDeclarator() const {
  return mDirectDeclarator;
}

const std::vector<std::string> &
DirectDeclaratorParentIdentifiers::getIdentifiers() const {
  return mIdentifiers;
}

DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    SourceInterface &interface, const CToken &curToken,
    DirectDeclarator &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers)
    : Node(interface, curToken), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)) {}

const DirectDeclarator &DirectDeclaratorAsterisk::getDirectDeclarator() const {
  return mDirectDeclarator;
}

const std::vector<TypeQualifier> &
DirectDeclaratorAsterisk::getTypeQualifiers() const {
  return mTypeQualifiers;
}

DirectDeclaratorNoStaticOrAsterisk::DirectDeclaratorNoStaticOrAsterisk(
    SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers,
    std::unique_ptr<AssignExpr> &&assignmentExpression)
    : Node(interface, curToken), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)),
      mAssignmentExpression(std::move(assignmentExpression)) {}

const DirectDeclarator &
DirectDeclaratorNoStaticOrAsterisk::getDirectDeclarator() const {
  return *mDirectDeclarator;
}

const std::vector<TypeQualifier> &
DirectDeclaratorNoStaticOrAsterisk::getTypeQualifiers() const {
  return mTypeQualifiers;
}

const std::unique_ptr<AssignExpr> &
DirectDeclaratorNoStaticOrAsterisk::getAssignmentExpression() const {
  return mAssignmentExpression;
}

DirectDeclaratorStatic::DirectDeclaratorStatic(
    SourceInterface &interface, const CToken &curToken,
    std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers,
    AssignExpr &&assignmentExpression)
    : Node(interface, curToken), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)),
      mAssignmentExpression(std::move(assignmentExpression)) {}

const DirectDeclarator &DirectDeclaratorStatic::getDirectDeclarator() const {
  return *mDirectDeclarator;
}

const std::vector<TypeQualifier> &
DirectDeclaratorStatic::getTypeQualifiers() const {
  return mTypeQualifiers;
}

const AssignExpr &DirectDeclaratorStatic::getAssignmentExpression() const {
  return AssignExpr;
}

DirectDeclarator::DirectDeclarator(SourceInterface &interface,
                                   const CToken &curToken, variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &DirectDeclarator::getVariant() const { return mVariant; }

Declarator::Declarator(SourceInterface &interface, const CToken &curToken,
                       std::vector<Pointer> &&pointers,
                       DirectDeclarator &&directDeclarator)
    : Node(interface, curToken), mPointers(std::move(pointers)),
      mDirectDeclarator(std::move(directDeclarator)) {}

const std::vector<Pointer> &Declarator::getPointers() const {
  return mPointers;
}

const DirectDeclarator &Declarator::getDirectDeclarator() const {
  return mDirectDeclarator;
}

StructOrUnionSpecifier::StructOrUnionSpecifier(
    SourceInterface &interface, const CToken &curToken, bool isUnion,
    std::string identifier, std::vector<StructDeclaration> &&structDeclarations)
    : Node(interface, curToken), mIsUnion(isUnion), mIdentifier(identifier),
      mStructDeclarations(std::move(structDeclarations)) {}

bool StructOrUnionSpecifier::isUnion() const { return mIsUnion; }

const std::string &StructOrUnionSpecifier::getIdentifier() const {
  return mIdentifier;
}

const std::vector<StructDeclaration> &
StructOrUnionSpecifier::getStructDeclarations() const {
  return mStructDeclarations;
}

EnumDeclaration::EnumDeclaration(
    SourceInterface &interface, const CToken &curToken, std::string name,
    std::vector<std::pair<std::string, std::int32_t>> values)
    : Node(interface, curToken), mName(name), mValues(std::move(values)) {}

const std::string &EnumDeclaration::getName() const { return mName; }

const std::vector<std::pair<std::string, std::int32_t>> &
EnumDeclaration::getValues() const {
  return mValues;
}

EnumSpecifier::EnumSpecifier(SourceInterface &interface, const CToken &curToken,
                             variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &EnumSpecifier::getVariant() const { return mVariant; }

TypeSpecifier::TypeSpecifier(SourceInterface &interface, const CToken &curToken,
                             variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &TypeSpecifier::getVariant() const { return mVariant; }

Pointer::Pointer(SourceInterface &interface, const CToken &curToken,
                 std::vector<TypeQualifier> &&typeQualifiers)
    : Node(interface, curToken), mTypeQualifiers(std::move(typeQualifiers)) {}

const std::vector<TypeQualifier> &Pointer::getTypeQualifiers() const {
  return mTypeQualifiers;
}

InitializerList::InitializerList(SourceInterface &interface,
                                 const CToken &curToken,
                                 vector &&nonCommaExpressionsAndBlocks)
    : Node(interface, curToken),
      mNonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks)) {}

const vector &InitializerList::getNonCommaExpressionsAndBlocks() const {
  return mNonCommaExpressionsAndBlocks;
}

Initializer::Initializer(SourceInterface &interface, const CToken &curToken,
                         variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &Initializer::getVariant() const { return mVariant; }

FunctionDefinition::FunctionDefinition(
    SourceInterface &interface, const CToken &curToken,
    std::vector<DeclarationSpecifier> &&declarationSpecifiers,
    Declarator &&declarator, std::vector<Declaration> &&declarations,
    BlockStmt &&compoundStatement)
    : Node(interface, curToken),
      mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mDeclarator(std::move(declarator)),
      mDeclarations(std::move(declarations)),
      mCompoundStatement(std::move(compoundStatement)) {}

const std::vector<DeclarationSpecifier> &
FunctionDefinition::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}

const Declarator &FunctionDefinition::getDeclarator() const {
  return mDeclarator;
}

const std::vector<Declaration> &FunctionDefinition::getDeclarations() const {
  return mDeclarations;
}

const BlockStmt &FunctionDefinition::getCompoundStatement() const {
  return mCompoundStatement;
}

ExternalDeclaration::ExternalDeclaration(SourceInterface &interface,
                                         const CToken &curToken,
                                         variant &&variant)
    : Node(interface, curToken), mVariant(std::move(variant)) {}

const variant &ExternalDeclaration::getVariant() const { return mVariant; }

explicit TranslationUnit::TranslationUnit(
    std::vector<ExternalDeclaration> &&globals) noexcept
    : mGlobals(std::move(globals)) {}

const std::vector<ExternalDeclaration> &TranslationUnit::getGlobals() const {
  return mGlobals;
}

} // namespace lcc