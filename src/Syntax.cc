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
namespace lcc::Syntax {

/// expr
Expr::Expr(std::vector<AssignExpr> assignExpressions)
    : Node(), mAssignExpressions(std::move(assignExpressions)) {
  assert(!mAssignExpressions.empty());
}

const std::vector<AssignExpr> &Expr::getAssignExpressions() const {
  return mAssignExpressions;
}

PrimaryExprIdentifier::PrimaryExprIdentifier(std::string identifier)
    : Node(), mIdentifier(std::move(identifier)) {}

const std::string &PrimaryExprIdentifier::getIdentifier() const {
  return mIdentifier;
}

PrimaryExprConstant::PrimaryExprConstant(Variant variant)
    : Node(), mVariant(std::move(variant)) {}

const PrimaryExprConstant::Variant &PrimaryExprConstant::getValue() const {
  return mVariant;
}

PrimaryExprParent::PrimaryExprParent(Expr &&expr)
    : Node(), mExpr(std::move(expr)) {}

const Expr &PrimaryExprParent::getExpr() const { return mExpr; }

PrimaryExpr::PrimaryExpr(Variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const PrimaryExpr::Variant &PrimaryExpr::getVariant() const { return mVariant; }

PostFixExprPrimary::PostFixExprPrimary(PrimaryExpr &&primaryExpr)
    : Node(), mPrimaryExpr(std::move(primaryExpr)) {}

const PrimaryExpr &PostFixExprPrimary::getPrimaryExpr() const {
  return mPrimaryExpr;
}

PostFixExprSubscript::PostFixExprSubscript(
    std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr)
    : Node(), mPostFixExpr(std::move(postFixExpr)), mExpr(std::move(expr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprSubscript::getPostFixExpr() const {
  return *mPostFixExpr;
}

const Expr &PostFixExprSubscript::getExpr() const { return mExpr; }

PostFixExprIncrement::PostFixExprIncrement(
    std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(), mPostFixExpr(std::move(postFixExpr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprIncrement::getPostFixExpr() const {
  return *mPostFixExpr;
}

PostFixExprDecrement::PostFixExprDecrement(
    std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(), mPostFixExpr(std::move(postFixExpr)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprDecrement::getPostFixExpr() const {
  return *mPostFixExpr;
}

PostFixExprDot::PostFixExprDot(std::unique_ptr<PostFixExpr> &&postFixExpr,
                               std::string identifier)
    : Node(), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {
  assert(mPostFixExpr);
}

const PostFixExpr &PostFixExprDot::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::string &PostFixExprDot::getIdentifier() const { return mIdentifier; }

PostFixExprArrow::PostFixExprArrow(std::unique_ptr<PostFixExpr> &&postFixExpr,
                                   std::string identifier)
    : Node(), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}

const PostFixExpr &PostFixExprArrow::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::string &PostFixExprArrow::getIdentifier() const {
  return mIdentifier;
}

PostFixExprFuncCall::PostFixExprFuncCall(
    std::unique_ptr<PostFixExpr> &&postFixExpr,
    std::vector<AssignExpr> &&optParams)
    : Node(), mPostFixExpr(std::move(postFixExpr)),
      mOptParams(std::move(optParams)) {}

const PostFixExpr &PostFixExprFuncCall::getPostFixExpr() const {
  return *mPostFixExpr;
}

const std::vector<AssignExpr> &
PostFixExprFuncCall::getOptionalAssignExpressions() const {
  return mOptParams;
}

PostFixExpr::PostFixExpr(Variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const PostFixExpr::Variant &PostFixExpr::getVariant() const { return mVariant; }

UnaryExprPostFixExpr::UnaryExprPostFixExpr(PostFixExpr &&postExpr)
    : Node(), mPostExpr(std::move(postExpr)) {}

const PostFixExpr &UnaryExprPostFixExpr::getPostExpr() const {
  return mPostExpr;
}

UnaryExprUnaryOperator::UnaryExprUnaryOperator(
    tok::TokenKind tokenKind, std::unique_ptr<UnaryExpr> &&unaryExpr)
    : Node(), mTok(tokenKind), mUnaryExpr(std::move(unaryExpr)) {}

tok::TokenKind UnaryExprUnaryOperator::getOperator() const { return mTok; }

const UnaryExpr &UnaryExprUnaryOperator::getUnaryExpr() const {
  return *mUnaryExpr;
}

UnaryExprSizeOf::UnaryExprSizeOf(Variant &&variant)
    : Node(), mValue(std::move(variant)) {}

const UnaryExprSizeOf::Variant &UnaryExprSizeOf::getVariant() const {
  return mValue;
}

UnaryExpr::UnaryExpr(Variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const UnaryExpr::Variant &UnaryExpr::getVariant() const { return mVariant; }

AssignExprAssign::AssignExprAssign(UnaryExpr &&unaryExpr,
                                   tok::TokenKind tokenKind,
                                   std::unique_ptr<AssignExpr> &&assignExpr)
    : Node(), mOperator(tokenKind), mUnaryExpr(std::move(unaryExpr)),
      mAssignExpr(std::move(assignExpr)) {
  assert(mAssignExpr);
}

const UnaryExpr &AssignExprAssign::getUnaryExpr() const { return mUnaryExpr; }

const AssignExpr &AssignExprAssign::getAssignExpr() const {
  return *mAssignExpr;
}

const tok::TokenKind AssignExprAssign::getOperator() const {
  return mOperator;
}
TypeName::TypeName(std::vector<SpecifierQualifier> &&specifierQualifiers,
                   std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : Node(), mSpecifierQualifiers(std::move(specifierQualifiers)),
      mAbstractDeclarator(std::move(abstractDeclarator)) {}

const std::vector<SpecifierQualifier> &
TypeName::getSpecifierQualifiers() const {
  return mSpecifierQualifiers;
}

const AbstractDeclarator *TypeName::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

CastExpr::CastExpr(Variant &&unaryOrCast)
    : Node(), mVariant(std::move(unaryOrCast)) {}

const CastExpr::Variant &CastExpr::getVariant() const { return mVariant; }

MultiExpr::MultiExpr(
    CastExpr &&castExpr,
    std::vector<std::pair<tok::TokenKind, CastExpr>> &&optCastExps)
    : Node(), mCastExpr(std::move(castExpr)),
      mOptCastExps(std::move(optCastExps)) {}

const CastExpr &MultiExpr::getCastExpr() const { return mCastExpr; }

const std::vector<std::pair<tok::TokenKind, CastExpr>> &
MultiExpr::getOptionalCastExpr() const {
  return mOptCastExps;
}

AdditiveExpr::AdditiveExpr(
    MultiExpr &&multiExpr,
    std::vector<std::pair<tok::TokenKind, MultiExpr>> &&optionalMultiExps)
    : Node(), mMultiExpr(std::move(multiExpr)),
      mOptionalMultiExpr(std::move(optionalMultiExps)) {}

const MultiExpr &AdditiveExpr::getMultiExpr() const { return mMultiExpr; }

const std::vector<std::pair<tok::TokenKind, MultiExpr>> &
AdditiveExpr::getOptionalMultiExpr() const {
  return mOptionalMultiExpr;
}

ShiftExpr::ShiftExpr(
    AdditiveExpr &&additiveExpr,
    std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &&optAdditiveExps)
    : Node(), mAdditiveExpr(std::move(additiveExpr)),
      mOptAdditiveExps(std::move(optAdditiveExps)) {}

const AdditiveExpr &ShiftExpr::getAdditiveExpr() const { return mAdditiveExpr; }

const std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &
ShiftExpr::getOptAdditiveExps() const {
  return mOptAdditiveExps;
}

RelationalExpr::RelationalExpr(
    ShiftExpr &&shiftExpr,
    std::vector<std::pair<tok::TokenKind, ShiftExpr>> &&optShiftExps)
    : Node(), mShiftExpr(std::move(shiftExpr)),
      mOptShiftExps(std::move(optShiftExps)) {}

const ShiftExpr &RelationalExpr::getShiftExpr() const { return mShiftExpr; }

const std::vector<std::pair<tok::TokenKind, ShiftExpr>> &
RelationalExpr::getOptionalShiftExpressions() const {
  return mOptShiftExps;
}

EqualExpr::EqualExpr(
    RelationalExpr &&relationalExpr,
    std::vector<std::pair<tok::TokenKind, RelationalExpr>> &&optRelationalExps)
    : Node(), mRelationalExpr(std::move(relationalExpr)),
      mOptRelationExps(std::move(optRelationalExps)) {}

const RelationalExpr &EqualExpr::getRelationalExpr() const {
  return mRelationalExpr;
}

const std::vector<std::pair<tok::TokenKind, RelationalExpr>> &
EqualExpr::getOptionalRelationalExpr() const {
  return mOptRelationExps;
}

LogAndExpr::LogAndExpr(BitOrExpr &&bitOrExpr,
                       std::vector<BitOrExpr> &&optBitOrExps)
    : Node(), mBitOrExpr(std::move(bitOrExpr)),
      mOptBitOrExps(std::move(optBitOrExps)) {}

const BitOrExpr &LogAndExpr::getBitOrExpression() const { return mBitOrExpr; }

const std::vector<BitOrExpr> &LogAndExpr::getOptionalBitOrExpressions() const {
  return mOptBitOrExps;
}

BitAndExpr::BitAndExpr(EqualExpr &&equalExpr,
                       std::vector<EqualExpr> &&optEqualExps)
    : Node(), mEqualExpr(std::move(equalExpr)),
      mOptEqualExps(std::move(optEqualExps)) {}

const EqualExpr &BitAndExpr::getEqualExpr() const { return mEqualExpr; }

const std::vector<EqualExpr> &BitAndExpr::getOptionalEqualExpr() const {
  return mOptEqualExps;
}

BitXorExpr::BitXorExpr(BitAndExpr &&bitAndExpr,
                       std::vector<BitAndExpr> &&optBitAndExps)
    : Node(), mBitAndExpr(std::move(bitAndExpr)),
      mOptBitAndExps(std::move(optBitAndExps)) {}

const BitAndExpr &BitXorExpr::getBitAndExpr() const { return mBitAndExpr; }

const std::vector<BitAndExpr> &
BitXorExpr::getOptionalBitAndExpressions() const {
  return mOptBitAndExps;
}

BitOrExpr::BitOrExpr(BitXorExpr &&bitXorExpr,
                     std::vector<BitXorExpr> &&optBitXorExps)
    : Node(), mBitXorExpr(std::move(bitXorExpr)),
      mOptBitXorExps(std::move(optBitXorExps)) {}

const BitXorExpr &BitOrExpr::getBitXorExpression() const { return mBitXorExpr; }

const std::vector<BitXorExpr> &BitOrExpr::getOptionalBitXorExpressions() const {
  return mOptBitXorExps;
}

LogOrExpr::LogOrExpr(LogAndExpr &&logAndExpr,
                     std::vector<LogAndExpr> &&optLogAndExps)
    : Node(), mLogAndExpr(std::move(logAndExpr)),
      mOptLogAndExps(std::move(optLogAndExps)) {}

const LogAndExpr &LogOrExpr::getAndExpression() const { return mLogAndExpr; }

const std::vector<LogAndExpr> &LogOrExpr::getOptionalAndExpressions() const {
  return mOptLogAndExps;
}

ConditionalExpr::ConditionalExpr(LogOrExpr &&logOrExpr,
                                 std::unique_ptr<Expr> &&optExpr,
                                 std::unique_ptr<ConditionalExpr> &&optCondExpr)
    : Node(), mLogOrExpr(std::move(logOrExpr)), mOptExpr(std::move(optExpr)),
      mOptCondExpr(std::move(optCondExpr)) {}

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

    std::variant<AssignExprAssign, ConditionalExpr> &&variant)
    : Node(), mVariant(std::move(variant)) {}

const std::variant<AssignExprAssign, ConditionalExpr> &
AssignExpr::getVariant() const {
  return mVariant;
}

/// stmt
ReturnStmt::ReturnStmt(std::unique_ptr<Expr> &&optExpr)
    : Node(), mOptExpr(std::move(optExpr)) {}

const Expr *ReturnStmt::getExpression() const { return mOptExpr.get(); }

IfStmt::IfStmt(Expr &&expr, std::unique_ptr<Stmt> &&thenStmt,
               std::unique_ptr<Stmt> &&optElseStmt)
    : Node(), mExpr(std::move(expr)), mThenStmt(std::move(thenStmt)),
      mOptElseStmt(std::move(optElseStmt)) {
  assert(mThenStmt);
}

const Expr &IfStmt::getExpression() const { return mExpr; }

const Stmt &IfStmt::getThenStmt() const { return *mThenStmt; }

const Stmt *IfStmt::getElseStmt() const { return mOptElseStmt.get(); }

SwitchStmt::SwitchStmt(Expr &&expression, std::unique_ptr<Stmt> &&statement)
    : Node(), mExpr(std::move(expression)), mStmt(std::move(statement)) {
  assert(mStmt);
}

const Expr &SwitchStmt::getExpression() const { return mExpr; }

const Stmt &SwitchStmt::getStatement() const { return *mStmt; }

DefaultStmt::DefaultStmt(std::unique_ptr<Stmt> &&statement)
    : Node(), mStmt(std::move(statement)) {
  assert(mStmt);
}

const Stmt &DefaultStmt::getStatement() const { return *mStmt; }

CaseStmt::CaseStmt(const constantVariant &constant,
                   std::unique_ptr<Stmt> &&statement)
    : Node(), mConstant(constant), mStatement(std::move(statement)) {}

const Stmt *CaseStmt::getStatement() const { return mStatement.get(); }

const CaseStmt::constantVariant &CaseStmt::getConstant() const {
  return mConstant;
}

GotoStmt::GotoStmt(std::string identifier)
    : Node(), mIdentifier(std::move(identifier)) {}

const std::string &GotoStmt::getIdentifier() const { return mIdentifier; }

LabelStmt::LabelStmt(std::string identifier)
    : Node(), mIdentifier(std::move(identifier)) {}

const std::string &LabelStmt::getIdentifier() const { return mIdentifier; }

WhileStmt::WhileStmt(Expr &&expr, std::unique_ptr<Stmt> &&stmt)
    : Node(), mExpr(std::move(expr)), mStmt(std::move(stmt)) {
  assert(mStmt);
}

const Expr &WhileStmt::getExpression() const { return mExpr; }

const Stmt &WhileStmt::getStatement() const { return *mStmt; }

DoWhileStmt::DoWhileStmt(std::unique_ptr<Stmt> &&stmt, Expr &&expr)
    : Node(), mStmt(std::move(stmt)), mExpr(std::move(expr)) {
  assert(mStmt);
}
const Expr &DoWhileStmt::getExpression() const { return mExpr; }

const Stmt &DoWhileStmt::getStatement() const { return *mStmt; }

ForStmt::ForStmt(std::unique_ptr<Stmt> &&stmt, std::unique_ptr<Expr> &&initExpr,
                 std::unique_ptr<Expr> &&controlExpr,
                 std::unique_ptr<Expr> &&postExpr)
    : Node(), mStmt(std::move(stmt)), mInitExpr(std::move(initExpr)),
      mControlExpr(std::move(controlExpr)), mPostExpr(std::move(postExpr)) {
  assert(mStmt);
}

const Stmt &ForStmt::getStatement() const { return *mStmt; }

const Expr *ForStmt::getInitial() const { return mInitExpr.get(); }

const Expr *ForStmt::getControlling() const { return mControlExpr.get(); }

const Expr *ForStmt::getPost() const { return mPostExpr.get(); }

ForDeclarationStmt::ForDeclarationStmt(std::unique_ptr<Stmt> &&stmt,
                                       Declaration &&initDecl,
                                       std::unique_ptr<Expr> &&controlExpr,
                                       std::unique_ptr<Expr> &&postExpr)
    : Node(), mStmt(std::move(stmt)), mInitDecl(std::move(initDecl)),
      mControlExpr(std::move(controlExpr)), mPostExpr(std::move(postExpr)) {
  assert(mStmt);
}

const Declaration &ForDeclarationStmt::getInitial() const { return mInitDecl; }

const Expr *ForDeclarationStmt::getControlling() const {
  return mControlExpr.get();
}

const Expr *ForDeclarationStmt::getPost() const { return mPostExpr.get(); }

const Stmt &ForDeclarationStmt::getStatement() const { return *mStmt; }

ExprStmt::ExprStmt(std::unique_ptr<Expr> &&optExpr)
    : Node(), mOptExpr(std::move(optExpr)) {}

const Expr *ExprStmt::getOptionalExpression() const { return mOptExpr.get(); }

std::unique_ptr<Expr> ExprStmt::moveOptionalExpr() {
  return std::move(mOptExpr);
}


BreakStmt::BreakStmt() : Node() {}

ContinueStmt::ContinueStmt() : Node() {}

BlockItem::BlockItem(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const BlockItem::variant &BlockItem::getVariant() const { return mVariant; }

BlockItem::variant &BlockItem::getVariant() { return mVariant; }

BlockStmt::BlockStmt(std::vector<BlockItem> &&blockItems)
    : Node(), mBlockItems(std::move(blockItems)) {}

const std::vector<BlockItem> &BlockStmt::getBlockItems() const {
  return mBlockItems;
}

Stmt::Stmt(variant &&variant) : Node(), mVariant(std::move(variant)) {}

const Stmt::variant &Stmt::getVariant() const { return mVariant; }
Stmt::variant &Stmt::getVariant() { return mVariant; }

/// decl
Declaration::Declaration(

    std::vector<DeclarationSpecifier> &&declarationSpecifiers,
    std::vector<std::pair<std::unique_ptr<Declarator>,
                          std::unique_ptr<Initializer>>> &&initDeclarators)
    : Node(), mDeclarationSpecifiers(std::move(declarationSpecifiers)),
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

        std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
        std::unique_ptr<AssignExpr> &&assignmentExpression)
    : Node(), mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
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

        std::unique_ptr<Syntax::DirectAbstractDeclarator>
            &&directAbstractDeclarator,
        std::unique_ptr<Syntax::ParameterTypeList> &&parameterTypeList)
    : Node(), mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

const DirectAbstractDeclarator *
DirectAbstractDeclaratorParameterTypeList::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

const ParameterTypeList *
DirectAbstractDeclaratorParameterTypeList::getParameterTypeList() const {
  return mParameterTypeList.get();
}

DirectAbstractDeclarator::DirectAbstractDeclarator(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const DirectAbstractDeclarator::variant &
DirectAbstractDeclarator::getVariant() const {
  return mVariant;
}

AbstractDeclarator::AbstractDeclarator(
    std::vector<Pointer> &&pointers,
    DirectAbstractDeclarator &&directAbstractDeclarator)
    : Node(), mPointers(std::move(pointers)),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

const std::vector<Pointer> &AbstractDeclarator::getPointers() const {
  return mPointers;
}

const DirectAbstractDeclarator &
AbstractDeclarator::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator;
}

ParameterList::ParameterList(std::vector<ParameterDeclaration> &&parameterList)
    : Node(), mParameterList(std::move(parameterList)) {}

const std::vector<ParameterDeclaration> &
ParameterList::getParameterDeclarations() const {
  return mParameterList;
}

ParameterTypeList::ParameterTypeList(ParameterList &&parameterList,
                                     bool hasEllipse)
    : Node(), mParameterList(std::move(parameterList)),
      mHasEllipse(hasEllipse) {}

const ParameterList &ParameterTypeList::getParameterList() const {
  return mParameterList;
}

bool ParameterTypeList::hasEllipse() const { return mHasEllipse; }

DirectDeclaratorParentParameters::DirectDeclaratorParentParameters(
    DirectDeclarator &&directDeclarator, ParameterTypeList &&parameterTypeList)
    : Node(), mDirectDeclarator(std::make_unique<DirectDeclarator>(
                  std::move(directDeclarator))),
      mParameterTypeList(std::move(parameterTypeList)) {}

const DirectDeclarator &
DirectDeclaratorParentParameters::getDirectDeclarator() const {
  return *mDirectDeclarator;
}

const ParameterTypeList &
DirectDeclaratorParentParameters::getParameterTypeList() const {
  return mParameterTypeList;
}

DirectDeclaratorSquare::DirectDeclaratorSquare(

    std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers,
    std::unique_ptr<AssignExpr> &&assignmentExpression)
    : Node(), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)),
      mAssignmentExpression(std::move(assignmentExpression)) {}

const DirectDeclarator &DirectDeclaratorSquare::getDirectDeclarator() const {
  return *mDirectDeclarator;
}

const std::vector<TypeQualifier> &
DirectDeclaratorSquare::getTypeQualifiers() const {
  return mTypeQualifiers;
}

const std::unique_ptr<AssignExpr> &
DirectDeclaratorSquare::getAssignmentExpression() const {
  return mAssignmentExpression;
}

DirectDeclarator::DirectDeclarator(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const DirectDeclarator::variant &DirectDeclarator::getVariant() const {
  return mVariant;
}

Declarator::Declarator(std::vector<Pointer> &&pointers,
                       DirectDeclarator &&directDeclarator)
    : Node(), mPointers(std::move(pointers)),
      mDirectDeclarator(std::move(directDeclarator)) {}

const std::vector<Pointer> &Declarator::getPointers() const {
  return mPointers;
}

const DirectDeclarator &Declarator::getDirectDeclarator() const {
  return mDirectDeclarator;
}

StructOrUnionSpecifier::StructOrUnionSpecifier(
    bool isUnion, std::string identifier,
    std::vector<StructDeclaration> &&structDeclarations)
    : Node(), mIsUnion(isUnion), mIdentifier(identifier),
      mStructDeclarations(std::move(structDeclarations)) {}

bool StructOrUnionSpecifier::isUnion() const { return mIsUnion; }

const std::string &StructOrUnionSpecifier::getIdentifier() const {
  return mIdentifier;
}

const std::vector<StructOrUnionSpecifier::StructDeclaration> &
StructOrUnionSpecifier::getStructDeclarations() const {
  return mStructDeclarations;
}

EnumDeclaration::EnumDeclaration(
    std::string name, std::vector<std::pair<std::string, std::int32_t>> values)
    : Node(), mName(name), mValues(std::move(values)) {}

const std::string &EnumDeclaration::getName() const { return mName; }

const std::vector<std::pair<std::string, std::int32_t>> &
EnumDeclaration::getValues() const {
  return mValues;
}

EnumSpecifier::EnumSpecifier(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const EnumSpecifier::variant &EnumSpecifier::getVariant() const {
  return mVariant;
}

TypeSpecifier::TypeSpecifier(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const TypeSpecifier::variant &TypeSpecifier::getVariant() const {
  return mVariant;
}

Pointer::Pointer(std::vector<TypeQualifier> &&typeQualifiers)
    : Node(), mTypeQualifiers(std::move(typeQualifiers)) {}

const std::vector<TypeQualifier> &Pointer::getTypeQualifiers() const {
  return mTypeQualifiers;
}

InitializerList::InitializerList(vector &&nonCommaExpressionsAndBlocks)
    : Node(),
      mNonCommaExpressionsAndBlocks(std::move(nonCommaExpressionsAndBlocks)) {}

const InitializerList::vector &
InitializerList::getNonCommaExpressionsAndBlocks() const {
  return mNonCommaExpressionsAndBlocks;
}

Initializer::Initializer(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const Initializer::variant &Initializer::getVariant() const { return mVariant; }

FunctionDefinition::FunctionDefinition(

    std::vector<DeclarationSpecifier> &&declarationSpecifiers,
    Declarator &&declarator, BlockStmt &&compoundStatement)
    : Node(), mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mDeclarator(std::move(declarator)),
      mCompoundStatement(std::move(compoundStatement)) {}

const std::vector<DeclarationSpecifier> &
FunctionDefinition::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}

const Declarator &FunctionDefinition::getDeclarator() const {
  return mDeclarator;
}

const BlockStmt &FunctionDefinition::getCompoundStatement() const {
  return mCompoundStatement;
}

ExternalDeclaration::ExternalDeclaration(variant &&variant)
    : Node(), mVariant(std::move(variant)) {}

const ExternalDeclaration::variant &ExternalDeclaration::getVariant() const {
  return mVariant;
}

TranslationUnit::TranslationUnit(
    std::vector<ExternalDeclaration> &&globals) noexcept
    : mGlobals(std::move(globals)) {}

const std::vector<ExternalDeclaration> &TranslationUnit::getGlobals() const {
  return mGlobals;
}

} // namespace lcc::Syntax