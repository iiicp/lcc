#include "Syntax.h"
namespace lcc::Syntax {
Node::Node(TokIter begin): mBeginTokLoc(begin) {
}
TokIter Node::getBeginLoc() const { return mBeginTokLoc; }

PrimaryExprIdent::PrimaryExprIdent(TokIter begin,
                                   std::string_view identifier)
    : Node(begin), mIdent(identifier) {}
[[nodiscard]] const std::string_view &PrimaryExprIdent::getIdentifier() const {
  return mIdent;
}

PrimaryExprConstant::PrimaryExprConstant(TokIter begin, Variant variant)
    : Node(begin), mValue(std::move(variant)){};
[[nodiscard]] const PrimaryExprConstant::Variant &
PrimaryExprConstant::getValue() const {
  return mValue;
}

PrimaryExprParentheses::PrimaryExprParentheses(TokIter begin, Expr &&expr)
    : Node(begin), mExpr(std::make_unique<Expr>(std::move(expr))) {}

[[nodiscard]] const Expr &PrimaryExprParentheses::getExpr() const {
  return *mExpr;
}

PostFixExprPrimaryExpr::PostFixExprPrimaryExpr(TokIter begin,
                                               PrimaryExpr &&primaryExpr)
    : Node(begin), mPrimaryExpr(std::move(primaryExpr)){};

[[nodiscard]] const PrimaryExpr &
PostFixExprPrimaryExpr::getPrimaryExpr() const {
  return mPrimaryExpr;
}

PostFixExprSubscript::PostFixExprSubscript(
    TokIter begin, std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)),
      mExpr(std::make_unique<Expr>(std::move(expr))) {}

[[nodiscard]] const PostFixExpr *PostFixExprSubscript::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const Expr &PostFixExprSubscript::getExpr() const {
  return *mExpr;
}

PostFixExprFuncCall::PostFixExprFuncCall(
    TokIter begin, std::unique_ptr<PostFixExpr> &&postFixExpr,
    std::vector<std::unique_ptr<AssignExpr>> &&optParams)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)),
      mOptParams(std::move(optParams)) {}

[[nodiscard]] const PostFixExpr *PostFixExprFuncCall::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::vector<std::unique_ptr<AssignExpr>> &
PostFixExprFuncCall::getOptionalAssignExpressions() const {
  return mOptParams;
}

PostFixExprDot::PostFixExprDot(TokIter begin,
                               std::unique_ptr<PostFixExpr> &&postFixExpr,
                               std::string_view identifier)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}

[[nodiscard]] const PostFixExpr *PostFixExprDot::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::string_view &PostFixExprDot::getIdentifier() const {
  return mIdentifier;
}

PostFixExprArrow::PostFixExprArrow(TokIter begin,
                                   std::unique_ptr<PostFixExpr> &&postFixExpr,
                                   std::string_view identifier)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}
[[nodiscard]] const PostFixExpr *PostFixExprArrow::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::string_view &PostFixExprArrow::getIdentifier() const {
  return mIdentifier;
}

PostFixExprIncrement::PostFixExprIncrement(
    TokIter begin, std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)) {}
[[nodiscard]] const PostFixExpr *PostFixExprIncrement::getPostFixExpr() const {
  return mPostFixExpr.get();
}

PostFixExprDecrement::PostFixExprDecrement(
    TokIter begin, std::unique_ptr<PostFixExpr> &&postFixExpr)
    : Node(begin), mPostFixExpr(std::move(postFixExpr)) {}
[[nodiscard]] const PostFixExpr *PostFixExprDecrement::getPostFixExpr() const {
  return mPostFixExpr.get();
}

PostFixExprTypeInitializer::PostFixExprTypeInitializer(
    TokIter begin, TypeName &&typeName, InitializerList &&initializerList)
    : Node(begin), mTypeName(std::make_unique<TypeName>(std::move(typeName))),
      mInitializerList(
          std::make_unique<InitializerList>(std::move(initializerList))) {}

[[nodiscard]] const InitializerList *
PostFixExprTypeInitializer::getInitializerList() const {
  return mInitializerList.get();
}
[[nodiscard]] const TypeName *PostFixExprTypeInitializer::getTypeName() const {
  return mTypeName.get();
}

UnaryExprPostFixExpr::UnaryExprPostFixExpr(TokIter begin,
                                           PostFixExpr &&postExpr)
    : Node(begin), mPostExpr(std::move(postExpr)) {}

[[nodiscard]] const PostFixExpr &UnaryExprPostFixExpr::getPostExpr() const {
  return mPostExpr;
}

UnaryExprUnaryOperator::UnaryExprUnaryOperator(
    TokIter begin, UnaryOperator anOperator,
    std::unique_ptr<CastExpr> &&castExpr)
    : Node(begin), mOperator(anOperator), mCastExpr(std::move(castExpr)) {}

[[nodiscard]] UnaryExprUnaryOperator::UnaryOperator
UnaryExprUnaryOperator::getOperator() const {
  return mOperator;
}
[[nodiscard]] const CastExpr *UnaryExprUnaryOperator::getCastExpr() const {
  return mCastExpr.get();
}

UnaryExprSizeOf::UnaryExprSizeOf(TokIter begin, Variant &&variant)
    : Node(begin), mValue(std::move(variant)){};

[[nodiscard]] const UnaryExprSizeOf::Variant &
UnaryExprSizeOf::getVariant() const {
  return mValue;
}

TypeSpecifier::TypeSpecifier(TokIter begin, variant &&variant)
    : Node(begin), mVariant(std::move(variant)) {}

[[nodiscard]] const TypeSpecifier::variant &TypeSpecifier::getVariant() const {
  return mVariant;
}

TypeQualifier::TypeQualifier(TokIter begin, Qualifier qualifier)
    : Node(begin), mQualifier(qualifier){};
[[nodiscard]] TypeQualifier::Qualifier TypeQualifier::getQualifier() const {
  return mQualifier;
};

CastExpr::CastExpr(TokIter begin, Variant &&unaryOrCast)
    : Node(begin), mVariant(std::move(unaryOrCast)) {}

[[nodiscard]] const CastExpr::Variant &CastExpr::getVariant() const {
  return mVariant;
}

MultiExpr::MultiExpr(
    TokIter begin, CastExpr &&castExpr,
    std::vector<std::pair<BinaryOperator, CastExpr>> &&optCastExps)
    : Node(begin), mCastExpr(std::move(castExpr)),
      mOptCastExps(std::move(optCastExps)) {}
[[nodiscard]] const CastExpr &MultiExpr::getCastExpr() const {
  return mCastExpr;
}
[[nodiscard]] const std::vector<std::pair<MultiExpr::BinaryOperator, CastExpr>>
    &MultiExpr::getOptionalCastExpr() const {
  return mOptCastExps;
}

AdditiveExpr::AdditiveExpr(
    TokIter begin, MultiExpr &&multiExpr,
    std::vector<std::pair<BinaryOperator, MultiExpr>> &&optionalMultiExps)
    : Node(begin), mMultiExpr(std::move(multiExpr)),
      mOptionalMultiExpr(std::move(optionalMultiExps)) {}
[[nodiscard]] const MultiExpr &AdditiveExpr::getMultiExpr() const {
  return mMultiExpr;
}
[[nodiscard]] const std::vector<
    std::pair<AdditiveExpr::BinaryOperator, MultiExpr>> &
AdditiveExpr::getOptionalMultiExpr() const {
  return mOptionalMultiExpr;
}

ShiftExpr::ShiftExpr(
    TokIter begin, AdditiveExpr &&additiveExpr,
    std::vector<std::pair<BinaryOperator, AdditiveExpr>> &&optAdditiveExps)
    : Node(begin), mAdditiveExpr(std::move(additiveExpr)),
      mOptAdditiveExps(std::move(optAdditiveExps)) {}
[[nodiscard]] const AdditiveExpr &ShiftExpr::getAdditiveExpr() const {
  return mAdditiveExpr;
}
[[nodiscard]] const std::vector<
    std::pair<ShiftExpr::BinaryOperator, AdditiveExpr>> &
ShiftExpr::getOptAdditiveExps() const {
  return mOptAdditiveExps;
}

RelationalExpr::RelationalExpr(
    TokIter begin, ShiftExpr &&shiftExpr,
    std::vector<std::pair<RelationalExpr::BinaryOperator, ShiftExpr>>
        &&optShiftExps)
    : Node(begin), mShiftExpr(std::move(shiftExpr)),
      mOptShiftExps(std::move(optShiftExps)) {}
[[nodiscard]] const ShiftExpr &RelationalExpr::getShiftExpr() const {
  return mShiftExpr;
}
[[nodiscard]] const std::vector<
    std::pair<RelationalExpr::BinaryOperator, ShiftExpr>> &
RelationalExpr::getOptionalShiftExpressions() const {
  return mOptShiftExps;
}

EqualExpr::EqualExpr(
    TokIter begin, RelationalExpr &&relationalExpr,
    std::vector<std::pair<BinaryOperator, RelationalExpr>> &&optRelationalExps)
    : Node(begin), mRelationalExpr(std::move(relationalExpr)),
      mOptRelationExps(std::move(optRelationalExps)) {}
[[nodiscard]] const RelationalExpr &EqualExpr::getRelationalExpr() const {
  return mRelationalExpr;
}

[[nodiscard]] const std::vector<
    std::pair<EqualExpr::BinaryOperator, RelationalExpr>> &
EqualExpr::getOptionalRelationalExpr() const {
  return mOptRelationExps;
}

BitAndExpr::BitAndExpr(TokIter begin, EqualExpr &&equalExpr,
                       std::vector<EqualExpr> &&optEqualExps)
    : Node(begin), mEqualExpr(std::move(equalExpr)),
      mOptEqualExps(std::move(optEqualExps)) {}
[[nodiscard]] const EqualExpr &BitAndExpr::getEqualExpr() const {
  return mEqualExpr;
}

[[nodiscard]] const std::vector<EqualExpr> &
BitAndExpr::getOptionalEqualExpr() const {
  return mOptEqualExps;
}

BitXorExpr::BitXorExpr(TokIter begin, BitAndExpr &&bitAndExpr,
                       std::vector<BitAndExpr> &&optBitAndExps)
    : Node(begin), mBitAndExpr(std::move(bitAndExpr)),
      mOptBitAndExps(std::move(optBitAndExps)) {}
[[nodiscard]] const BitAndExpr &BitXorExpr::getBitAndExpr() const {
  return mBitAndExpr;
}
[[nodiscard]] const std::vector<BitAndExpr> &
BitXorExpr::getOptionalBitAndExpressions() const {
  return mOptBitAndExps;
}

BitOrExpr::BitOrExpr(TokIter begin, BitXorExpr &&bitXorExpr,
                     std::vector<BitXorExpr> &&optBitXorExps)
    : Node(begin), mBitXorExpr(std::move(bitXorExpr)),
      mOptBitXorExps(std::move(optBitXorExps)) {}
[[nodiscard]] const BitXorExpr &BitOrExpr::getBitXorExpression() const {
  return mBitXorExpr;
}

[[nodiscard]] const std::vector<BitXorExpr> &
BitOrExpr::getOptionalBitXorExpressions() const {
  return mOptBitXorExps;
}

LogAndExpr::LogAndExpr(TokIter begin, BitOrExpr &&bitOrExpr,
                       std::vector<BitOrExpr> &&optBitOrExps)
    : Node(begin), mBitOrExpr(std::move(bitOrExpr)),
      mOptBitOrExps(std::move(optBitOrExps)) {}
[[nodiscard]] const BitOrExpr &LogAndExpr::getBitOrExpression() const {
  return mBitOrExpr;
}
[[nodiscard]] const std::vector<BitOrExpr> &
LogAndExpr::getOptionalBitOrExpressions() const {
  return mOptBitOrExps;
}

LogOrExpr::LogOrExpr(TokIter begin, LogAndExpr &&logAndExpr,
                     std::vector<LogAndExpr> &&optLogAndExps)
    : Node(begin), mLogAndExpr(std::move(logAndExpr)),
      mOptLogAndExps(std::move(optLogAndExps)) {}
[[nodiscard]] const LogAndExpr &LogOrExpr::getAndExpression() const {
  return mLogAndExpr;
}
[[nodiscard]] const std::vector<LogAndExpr> &
LogOrExpr::getOptionalAndExpressions() const {
  return mOptLogAndExps;
}

ConditionalExpr::ConditionalExpr(TokIter begin, LogOrExpr &&logOrExpr,
                                 std::unique_ptr<Expr> &&optExpr,
                                 std::unique_ptr<ConditionalExpr> &&optCondExpr)
    : Node(begin), mLogOrExpr(std::move(logOrExpr)), mOptExpr(std::move(optExpr)),
      mOptCondExpr(std::move(optCondExpr)) {}
[[nodiscard]] const LogOrExpr &ConditionalExpr::getLogicalOrExpression() const {
  return mLogOrExpr;
}
[[nodiscard]] const Expr *ConditionalExpr::getOptionalExpression() const {
  return mOptExpr.get();
}
[[nodiscard]] const ConditionalExpr *
ConditionalExpr::getOptionalConditionalExpression() const {
  return mOptCondExpr.get();
}

AssignExpr::AssignExpr(
    TokIter begin, ConditionalExpr &&conditionalExpression,
    std::vector<std::pair<AssignmentOperator, ConditionalExpr>>
        &&optConditionExpr)
    : Node(begin), mCondExpr(std::move(conditionalExpression)),
      mOptConditionExpr(std::move(optConditionExpr)) {}

[[nodiscard]] const ConditionalExpr &AssignExpr::getConditionalExpr() const {
  return mCondExpr;
}
[[nodiscard]] const std::vector<
    std::pair<AssignExpr::AssignmentOperator, ConditionalExpr>> &
AssignExpr::getOptionalConditionalExpr() const {
  return mOptConditionExpr;
}

Expr::Expr(TokIter begin, std::vector<AssignExpr> assignExpressions)
    : Node(begin), mAssignExpressions(std::move(assignExpressions)) {}

const std::vector<AssignExpr> &Expr::getAssignExpressions() const {
  return mAssignExpressions;
}

ExprStmt::ExprStmt(TokIter begin, std::unique_ptr<Expr> &&optExpr)
    : Node(begin), mOptExpr(std::move(optExpr)) {}
[[nodiscard]] const Expr *ExprStmt::getOptionalExpression() const {
  return mOptExpr.get();
}
std::unique_ptr<Expr> ExprStmt::moveOptionalExpr() {
  return std::move(mOptExpr);
}

IfStmt::IfStmt(TokIter begin, Expr &&expr, std::unique_ptr<Stmt> &&thenStmt,
               std::unique_ptr<Stmt> &&optElseStmt)
    : Node(begin), mExpr(std::move(expr)), mThenStmt(std::move(thenStmt)),
      mOptElseStmt(std::move(optElseStmt)) {}

[[nodiscard]] const Expr &IfStmt::getExpression() const { return mExpr; }

[[nodiscard]] const Stmt *IfStmt::getThenStmt() const {
  return mThenStmt.get();
}

[[nodiscard]] const Stmt *IfStmt::getElseStmt() const {
  return mOptElseStmt.get();
}

SwitchStmt::SwitchStmt(TokIter begin, Expr &&expression,
                       std::unique_ptr<Stmt> &&statement)
    : Node(begin), mExpr(std::move(expression)), mStmt(std::move(statement)) {}

[[nodiscard]] const Expr &SwitchStmt::getExpression() const { return mExpr; }

[[nodiscard]] const Stmt *SwitchStmt::getStatement() const {
  return mStmt.get();
}

DefaultStmt::DefaultStmt(TokIter begin, std::unique_ptr<Stmt> &&statement)
    : Node(begin), mStmt(std::move(statement)) {}
[[nodiscard]] const Stmt *DefaultStmt::getStatement() const {
  return mStmt.get();
}

CaseStmt::CaseStmt(TokIter begin, ConstantExpr &&constantExpr,
                   std::unique_ptr<Stmt> &&statement)
    : Node(begin), mConstantExpr(std::move(constantExpr)),
      mStatement(std::move(statement)) {}

[[nodiscard]] const ConstantExpr &CaseStmt::getConstantExpr() const {
  return mConstantExpr;
}
[[nodiscard]] const Stmt *CaseStmt::getStatement() const {
  return mStatement.get();
}

LabelStmt::LabelStmt(TokIter begin, std::string_view identifier)
    : Node(begin), mIdentifier(identifier) {}
[[nodiscard]] const std::string_view &LabelStmt::getIdentifier() const {
  return mIdentifier;
}

GotoStmt::GotoStmt(TokIter begin, std::string_view identifier)
    : Node(begin), mIdentifier(identifier) {}
[[nodiscard]] const std::string_view &GotoStmt::getIdentifier() const {
  return mIdentifier;
}

DoWhileStmt::DoWhileStmt(TokIter begin, std::unique_ptr<Stmt> &&stmt,
                         Expr &&expr)
    : Node(begin), mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
[[nodiscard]] const Stmt *DoWhileStmt::getStatement() const {
  return mStmt.get();
}
[[nodiscard]] const Expr &DoWhileStmt::getExpression() const { return mExpr; }

WhileStmt::WhileStmt(TokIter begin, Expr &&expr,
                     std::unique_ptr<Stmt> &&stmt)
    : Node(begin), mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
[[nodiscard]] const Expr &WhileStmt::getExpression() const { return mExpr; }
[[nodiscard]] const Stmt *WhileStmt::getStatement() const {
  return mStmt.get();
}

ForStmt::ForStmt(
    TokIter begin, std::unique_ptr<Stmt> &&stmt,
    std::variant<std::unique_ptr<Declaration>, std::unique_ptr<Expr>> &&initial,
    std::unique_ptr<Expr> &&controlExpr, std::unique_ptr<Expr> &&postExpr)
    : Node(begin), mStmt(std::move(stmt)), mInitial(std::move(initial)),
      mControlExpr(std::move(controlExpr)), mPostExpr(std::move(postExpr)) {}
[[nodiscard]] const Stmt *ForStmt::getStatement() const { return mStmt.get(); }

[[nodiscard]] const std::variant<std::unique_ptr<Declaration>,
                                 std::unique_ptr<Expr>> &
ForStmt::getInitial() const {
  return mInitial;
}
[[nodiscard]] const Expr *ForStmt::getControlling() const {
  return mControlExpr.get();
}
[[nodiscard]] const Expr *ForStmt::getPost() const { return mPostExpr.get(); }

StorageClassSpecifier::StorageClassSpecifier(
    TokIter begin, StorageClassSpecifier::Specifiers specifier)
    : Node(begin), mSpecifier(specifier) {}
[[nodiscard]] StorageClassSpecifier::Specifiers
StorageClassSpecifier::getSpecifier() const {
  return mSpecifier;
}

FunctionSpecifier::FunctionSpecifier(TokIter begin) : Node(begin) {}

DeclarationSpecifiers::DeclarationSpecifiers(TokIter begin) : Node(begin) {}
void DeclarationSpecifiers::addStorageClassSpecifier(
    StorageClassSpecifier &&specifier) {
  mStorageClassSpecifiers.push_back(std::move(specifier));
}
void DeclarationSpecifiers::addTypeSpecifier(TypeSpecifier &&specifier) {
  mTypeSpecifiers.push_back(std::move(specifier));
}
void DeclarationSpecifiers::addTypeQualifier(TypeQualifier &&qualifier) {
  mTypeQualifiers.push_back(std::move(qualifier));
}
void DeclarationSpecifiers::addFunctionSpecifier(
    FunctionSpecifier &&specifier) {
  mFunctionSpecifiers.push_back(std::move(specifier));
}

[[nodiscard]] const std::vector<StorageClassSpecifier> &
DeclarationSpecifiers::getStorageClassSpecifiers() const {
  return mStorageClassSpecifiers;
}
[[nodiscard]] const std::vector<TypeSpecifier> &
DeclarationSpecifiers::getTypeSpecifiers() const {
  return mTypeSpecifiers;
}
[[nodiscard]] const std::vector<TypeQualifier> &
DeclarationSpecifiers::getTypeQualifiers() const {
  return mTypeQualifiers;
}
[[nodiscard]] const std::vector<FunctionSpecifier> &
DeclarationSpecifiers::getFunctionSpecifiers() const {
  return mFunctionSpecifiers;
}
[[nodiscard]] bool DeclarationSpecifiers::isEmpty() const {
  return mStorageClassSpecifiers.empty() && mTypeSpecifiers.empty() &&
         mTypeQualifiers.empty() && mFunctionSpecifiers.empty();
}

TypeName::TypeName(TokIter begin,
                   DeclarationSpecifiers &&specifierQualifiers,
                   std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : Node(begin), mSpecifierQualifiers(std::move(specifierQualifiers)),
      mAbstractDeclarator(std::move(abstractDeclarator)) {}

[[nodiscard]] const DeclarationSpecifiers &
TypeName::getSpecifierQualifiers() const {
  return mSpecifierQualifiers;
}
[[nodiscard]] const AbstractDeclarator *
TypeName::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

Declaration::Declaration(TokIter begin,
                         DeclarationSpecifiers &&declarationSpecifiers,
                         std::vector<InitDeclarator> &&initDeclarators)
    : Node(begin), mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mInitDeclarators(std::move(initDeclarators)) {}
[[nodiscard]] const DeclarationSpecifiers &
Declaration::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}
[[nodiscard]] const std::vector<Declaration::InitDeclarator> &
Declaration::getInitDeclarators() const {
  return mInitDeclarators;
}

BreakStmt::BreakStmt(TokIter begin) : Node(begin) {}

ContinueStmt::ContinueStmt(TokIter begin) : Node(begin) {}

ReturnStmt::ReturnStmt(TokIter begin, std::unique_ptr<Expr> &&optExpr)
    : Node(begin), mOptExpr(std::move(optExpr)) {}
[[nodiscard]] const Expr *ReturnStmt::getExpression() const {
  return mOptExpr.get();
}

BlockStmt::BlockStmt(TokIter begin, std::vector<BlockItem> &&blockItems)
    : Node(begin), mBlockItems(std::move(blockItems)) {}
[[nodiscard]] const std::vector<BlockItem> &BlockStmt::getBlockItems() const {
  return mBlockItems;
}

DirectAbstractDeclaratorParentheses::DirectAbstractDeclaratorParentheses(
    TokIter begin, std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : Node(begin), mAbstractDeclarator(std::move(abstractDeclarator)) {}

[[nodiscard]] const AbstractDeclarator *
DirectAbstractDeclaratorParentheses::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

DirectAbstractDeclaratorAssignExpr::DirectAbstractDeclaratorAssignExpr(
    TokIter begin,
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers,
    std::unique_ptr<AssignExpr> &&assignmentExpression, bool hasStatic)
    : Node(begin), mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)),
      mAssignmentExpression(std::move(assignmentExpression)),
      mHasStatic(hasStatic) {}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorAssignExpr::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &
DirectAbstractDeclaratorAssignExpr::getTypeQualifiers() const {
  return mTypeQualifiers;
}

[[nodiscard]] const AssignExpr *
DirectAbstractDeclaratorAssignExpr::getAssignmentExpression() const {
  return mAssignmentExpression.get();
}

[[nodiscard]] bool DirectAbstractDeclaratorAssignExpr::hasStatic() const {
  return mHasStatic;
}

DirectAbstractDeclaratorAsterisk::DirectAbstractDeclaratorAsterisk(
    TokIter begin,
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator)
    : Node(begin),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorAsterisk::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

DirectAbstractDeclaratorParamTypeList::DirectAbstractDeclaratorParamTypeList(
    TokIter begin,
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
    std::unique_ptr<ParamTypeList> &&parameterTypeList)
    : Node(begin), mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorParamTypeList::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

[[nodiscard]] const ParamTypeList *
DirectAbstractDeclaratorParamTypeList::getParameterTypeList() const {
  return mParameterTypeList.get();
}

Pointer::Pointer(TokIter begin, std::vector<TypeQualifier> &&typeQualifiers)
    : Node(begin), mTypeQualifiers(std::move(typeQualifiers)) {}

[[nodiscard]] const std::vector<TypeQualifier> &
Pointer::getTypeQualifiers() const {
  return mTypeQualifiers;
}

AbstractDeclarator::AbstractDeclarator(
    TokIter begin, std::vector<Pointer> &&pointers,
    std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator)
    : Node(begin), mPointers(std::move(pointers)),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

[[nodiscard]] const std::vector<Pointer> &
AbstractDeclarator::getPointers() const {
  return mPointers;
}

[[nodiscard]] const DirectAbstractDeclarator *
AbstractDeclarator::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator ? &*mDirectAbstractDeclarator : nullptr;
}

ParameterDeclaration::ParameterDeclaration(
    TokIter begin, DeclarationSpecifiers declarationSpecifiers,
    std::variant<std::unique_ptr<Declarator>,
                 std::optional<std::unique_ptr<AbstractDeclarator>>>
        variant)
    : Node(begin), declarationSpecifiers(std::move(declarationSpecifiers)),
      declarator(std::move(variant)) {}

ParamList::ParamList(TokIter begin,
                     std::vector<ParameterDeclaration> &&parameterList)
    : Node(begin), mParameterList(std::move(parameterList)) {}

[[nodiscard]] const std::vector<ParameterDeclaration> &
ParamList::getParameterDeclarations() const {
  return mParameterList;
}

ParamTypeList::ParamTypeList(TokIter begin, ParamList &&parameterList,
                             bool hasEllipse)
    : Node(begin), mParameterList(std::move(parameterList)),
      mHasEllipse(hasEllipse) {}

[[nodiscard]] const ParamList &ParamTypeList::getParameterList() const {
  return mParameterList;
}

[[nodiscard]] bool ParamTypeList::hasEllipse() const { return mHasEllipse; }

DirectDeclaratorIdent::DirectDeclaratorIdent(TokIter begin,
                                             std::string_view ident)
    : Node(begin), mIdent(ident) {}

[[nodiscard]] const std::string_view &DirectDeclaratorIdent::getIdent() const {
  return mIdent;
}

DirectDeclaratorParentheses::DirectDeclaratorParentheses(
    TokIter begin, std::unique_ptr<Declarator> &&declarator)
    : Node(begin), mDeclarator(std::move(declarator)) {}

[[nodiscard]] const Declarator *
DirectDeclaratorParentheses::getDeclarator() const {
  return mDeclarator.get();
}

DirectDeclaratorParamTypeList::DirectDeclaratorParamTypeList(
    TokIter begin, std::unique_ptr<DirectDeclarator> &&directDeclarator,
    ParamTypeList &&parameterTypeList)
    : Node(begin), mDirectDeclarator(std::move(directDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

[[nodiscard]] const DirectDeclarator *
DirectDeclaratorParamTypeList::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const ParamTypeList &
DirectDeclaratorParamTypeList::getParameterTypeList() const {
  return mParameterTypeList;
}

DirectDeclaratorAssignExpr::DirectDeclaratorAssignExpr(
    TokIter begin, std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifierList,
    std::unique_ptr<AssignExpr> &&assignmentExpression, bool hasStatic)
    : Node(begin), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifierList(std::move(typeQualifierList)),
      mAssignmentExpression(std::move(assignmentExpression)),
      mHasStatic(hasStatic) {}

[[nodiscard]] const DirectDeclarator *
DirectDeclaratorAssignExpr::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &
DirectDeclaratorAssignExpr::getTypeQualifierList() const {
  return mTypeQualifierList;
}

[[nodiscard]] const AssignExpr *
DirectDeclaratorAssignExpr::getAssignmentExpression() const {
  return mAssignmentExpression == nullptr ? nullptr
                                          : mAssignmentExpression.get();
}

[[nodiscard]] bool DirectDeclaratorAssignExpr::hasStatic() const {
  return mHasStatic;
}

DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    TokIter begin, std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifierList)
    : Node(begin), mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifierList(std::move(typeQualifierList)) {}

[[nodiscard]] const DirectDeclarator *
DirectDeclaratorAsterisk::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &
DirectDeclaratorAsterisk::getTypeQualifierList() const {
  return mTypeQualifierList;
}

Declarator::Declarator(TokIter begin, std::vector<Pointer> &&pointers,
                       DirectDeclarator &&directDeclarator)
    : Node(begin), mPointers(std::move(pointers)),
      mDirectDeclarator(std::move(directDeclarator)) {}

[[nodiscard]] const std::vector<Pointer> &Declarator::getPointers() const {
  return mPointers;
}

[[nodiscard]] const DirectDeclarator &Declarator::getDirectDeclarator() const {
  return mDirectDeclarator;
}

StructOrUnionSpecifier::StructOrUnionSpecifier(
    TokIter begin, bool isUnion, std::string_view identifier,
    std::vector<StructDeclaration> &&structDeclarations)
    : Node(begin), mIsUnion(isUnion), mId(identifier),
      mStructDeclarations(std::move(structDeclarations)) {}

[[nodiscard]] bool StructOrUnionSpecifier::isUnion() const { return mIsUnion; }

[[nodiscard]] std::string_view StructOrUnionSpecifier::getTag() const {
  return mId;
}

[[nodiscard]] const std::vector<StructOrUnionSpecifier::StructDeclaration> &
StructOrUnionSpecifier::getStructDeclarations() const {
  return mStructDeclarations;
}

EnumSpecifier::EnumSpecifier(TokIter begin, std::string_view id,
                             std::vector<Enumerator> &&enumerators)
    : Node(begin), mId(id), mEnumerators(std::move(enumerators)) {}

[[nodiscard]] const std::string_view &EnumSpecifier::getName() const {
  return mId;
}
[[nodiscard]] const std::vector<EnumSpecifier::Enumerator> &
EnumSpecifier::getEnumerators() const {
  return mEnumerators;
}

Initializer::Initializer(TokIter begin, Initializer::variant &&variant)
    : Node(begin), mVariant(std::move(variant)) {}

[[nodiscard]] const Initializer::variant &Initializer::getVariant() const {
  return mVariant;
}

InitializerList::InitializerList(TokIter begin,
                                 InitializerList::vector &&initializer)
    : Node(begin), mInitializer(std::move(initializer)) {}

const InitializerList::vector &InitializerList::getInitializerList() const {
  return mInitializer;
}

FunctionDefinition::FunctionDefinition(
    TokIter begin, DeclarationSpecifiers &&declarationSpecifiers,
    Declarator &&declarator, BlockStmt &&compoundStatement)
    : Node(begin), mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mDeclarator(std::move(declarator)),
      mCompoundStatement(std::move(compoundStatement)) {}

[[nodiscard]] const DeclarationSpecifiers &
FunctionDefinition::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}

[[nodiscard]] const Declarator &FunctionDefinition::getDeclarator() const {
  return mDeclarator;
}

[[nodiscard]] const BlockStmt &
FunctionDefinition::getCompoundStatement() const {
  return mCompoundStatement;
}
} // namespace lcc::Syntax