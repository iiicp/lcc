#include "Syntax.h"
namespace lcc::Syntax{
PrimaryExprIdent::PrimaryExprIdent(std::string_view identifier)
    : mIdent(identifier) {}
[[nodiscard]] const std::string_view &PrimaryExprIdent::getIdentifier() const {
  return mIdent;
}

PrimaryExprConstant::PrimaryExprConstant(Variant variant)
    : mValue(std::move(variant)){};
[[nodiscard]] const PrimaryExprConstant::Variant &PrimaryExprConstant::getValue() const {
  return mValue;
}

PrimaryExprParentheses::PrimaryExprParentheses(Expr &&expr)
    : mExpr(std::make_unique<Expr>(std::move(expr))){}

[[nodiscard]] const Expr &PrimaryExprParentheses::getExpr() const {
  return *mExpr;
}

PostFixExprPrimaryExpr::PostFixExprPrimaryExpr(PrimaryExpr &&primaryExpr)
    : mPrimaryExpr(std::move(primaryExpr)){};

[[nodiscard]] const PrimaryExpr &PostFixExprPrimaryExpr::getPrimaryExpr() const {
  return mPrimaryExpr;
}

PostFixExprSubscript::PostFixExprSubscript(std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr)
    : mPostFixExpr(std::move(postFixExpr)),
      mExpr(std::make_unique<Expr>(std::move(expr))) {}

[[nodiscard]] const PostFixExpr *PostFixExprSubscript::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const Expr &PostFixExprSubscript::getExpr() const { return *mExpr; }

PostFixExprFuncCall::PostFixExprFuncCall(std::unique_ptr<PostFixExpr> &&postFixExpr,
                    std::vector<std::unique_ptr<AssignExpr>> &&optParams)
    : mPostFixExpr(std::move(postFixExpr)), mOptParams(std::move(optParams)) {
}

[[nodiscard]] const PostFixExpr *PostFixExprFuncCall::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::vector<std::unique_ptr<AssignExpr>> &
PostFixExprFuncCall::getOptionalAssignExpressions() const {
  return mOptParams;
}

PostFixExprDot::PostFixExprDot(std::unique_ptr<PostFixExpr> &&postFixExpr,
               std::string_view identifier)
    : mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}

[[nodiscard]] const PostFixExpr *PostFixExprDot::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::string_view &PostFixExprDot::getIdentifier() const { return mIdentifier; }


PostFixExprArrow::PostFixExprArrow(std::unique_ptr<PostFixExpr> &&postFixExpr,
                 std::string_view identifier)
    : mPostFixExpr(std::move(postFixExpr)),
      mIdentifier(std::move(identifier)) {}
[[nodiscard]] const PostFixExpr *PostFixExprArrow::getPostFixExpr() const {
  return mPostFixExpr.get();
}
[[nodiscard]] const std::string_view &PostFixExprArrow::getIdentifier() const { return mIdentifier; }

PostFixExprIncrement::PostFixExprIncrement(std::unique_ptr<PostFixExpr> &&postFixExpr)
    : mPostFixExpr(std::move(postFixExpr)) {}
[[nodiscard]] const PostFixExpr *PostFixExprIncrement::getPostFixExpr() const {
  return mPostFixExpr.get();
}

PostFixExprDecrement::PostFixExprDecrement(std::unique_ptr<PostFixExpr> &&postFixExpr)
    : mPostFixExpr(std::move(postFixExpr)) {}
[[nodiscard]] const PostFixExpr *PostFixExprDecrement::getPostFixExpr() const {
  return mPostFixExpr.get();
}

PostFixExprTypeInitializer::PostFixExprTypeInitializer(TypeName &&typeName,
                           InitializerList &&initializerList)
    : mTypeName(std::make_unique<TypeName>(std::move(typeName))),
      mInitializerList(std::make_unique<InitializerList>(std::move(initializerList))) {}

[[nodiscard]] const InitializerList *PostFixExprTypeInitializer::getInitializerList() const {
  return mInitializerList.get();
}
[[nodiscard]] const TypeName *PostFixExprTypeInitializer::getTypeName() const { return mTypeName.get(); }


UnaryExprPostFixExpr::UnaryExprPostFixExpr(PostFixExpr &&postExpr)
    : mPostExpr(std::move(postExpr)) {}

[[nodiscard]] const PostFixExpr &UnaryExprPostFixExpr::getPostExpr() const { return mPostExpr; }

UnaryExprUnaryOperator::UnaryExprUnaryOperator(UnaryOperator anOperator,
                       std::unique_ptr<CastExpr> &&castExpr)
    : mOperator(anOperator), mCastExpr(std::move(castExpr)) {}

[[nodiscard]] UnaryExprUnaryOperator::UnaryOperator UnaryExprUnaryOperator::getOperator() const { return mOperator; }
[[nodiscard]] const CastExpr *UnaryExprUnaryOperator::getCastExpr() const { return mCastExpr.get(); }

UnaryExprSizeOf::UnaryExprSizeOf(Variant &&variant) : mValue(std::move(variant)){};

[[nodiscard]] const UnaryExprSizeOf::Variant &UnaryExprSizeOf::getVariant() const { return mValue; }


TypeSpecifier::TypeSpecifier(variant &&variant) : mVariant(std::move(variant)) {}

[[nodiscard]] const TypeSpecifier::variant &TypeSpecifier::getVariant() const { return mVariant; }

TypeQualifier::TypeQualifier(Qualifier qualifier) : mQualifier(qualifier){};
[[nodiscard]] TypeQualifier::Qualifier TypeQualifier::getQualifier() const { return mQualifier; };

CastExpr::CastExpr(Variant &&unaryOrCast) : mVariant(std::move(unaryOrCast)) {}

[[nodiscard]] const CastExpr::Variant &CastExpr::getVariant() const { return mVariant; }

MultiExpr::MultiExpr(
    CastExpr &&castExpr,
    std::vector<std::pair<BinaryOperator, CastExpr>> &&optCastExps)
    : mCastExpr(std::move(castExpr)), mOptCastExps(std::move(optCastExps)) {}
[[nodiscard]] const CastExpr &MultiExpr::getCastExpr() const { return mCastExpr; }
[[nodiscard]] const std::vector<std::pair<MultiExpr::BinaryOperator, CastExpr>> &
MultiExpr::getOptionalCastExpr() const {
  return mOptCastExps;
}

AdditiveExpr::AdditiveExpr(
    MultiExpr &&multiExpr,
    std::vector<std::pair<BinaryOperator, MultiExpr>> &&optionalMultiExps)
    : mMultiExpr(std::move(multiExpr)),
      mOptionalMultiExpr(std::move(optionalMultiExps)) {}
[[nodiscard]] const MultiExpr &AdditiveExpr::getMultiExpr() const { return mMultiExpr; }
[[nodiscard]] const std::vector<std::pair<AdditiveExpr::BinaryOperator, MultiExpr>> &
AdditiveExpr::getOptionalMultiExpr() const {
  return mOptionalMultiExpr;
}

ShiftExpr::ShiftExpr(
    AdditiveExpr &&additiveExpr,
    std::vector<std::pair<BinaryOperator, AdditiveExpr>> &&optAdditiveExps)
    : mAdditiveExpr(std::move(additiveExpr)),
      mOptAdditiveExps(std::move(optAdditiveExps)) {}
[[nodiscard]] const AdditiveExpr &ShiftExpr::getAdditiveExpr() const {
  return mAdditiveExpr;
}
[[nodiscard]] const std::vector<std::pair<ShiftExpr::BinaryOperator, AdditiveExpr>> &
ShiftExpr::getOptAdditiveExps() const {
  return mOptAdditiveExps;
}

RelationalExpr::RelationalExpr(
    ShiftExpr &&shiftExpr,
    std::vector<std::pair<RelationalExpr::BinaryOperator, ShiftExpr>> &&optShiftExps)
    : mShiftExpr(std::move(shiftExpr)),
      mOptShiftExps(std::move(optShiftExps)) {}
[[nodiscard]] const ShiftExpr &RelationalExpr::getShiftExpr() const { return mShiftExpr; }
[[nodiscard]] const std::vector<std::pair<RelationalExpr::BinaryOperator, ShiftExpr>> &
RelationalExpr::getOptionalShiftExpressions() const {
  return mOptShiftExps;
}

EqualExpr::EqualExpr(RelationalExpr &&relationalExpr,
          std::vector<std::pair<BinaryOperator, RelationalExpr>>
              &&optRelationalExps)
    : mRelationalExpr(std::move(relationalExpr)),
      mOptRelationExps(std::move(optRelationalExps)) {}
[[nodiscard]] const RelationalExpr &EqualExpr::getRelationalExpr() const {
  return mRelationalExpr;
}

[[nodiscard]] const std::vector<std::pair<EqualExpr::BinaryOperator, RelationalExpr>> &
EqualExpr::getOptionalRelationalExpr() const {
  return mOptRelationExps;
}

BitAndExpr::BitAndExpr(EqualExpr &&equalExpr, std::vector<EqualExpr> &&optEqualExps)
    : mEqualExpr(std::move(equalExpr)),
      mOptEqualExps(std::move(optEqualExps)) {}
[[nodiscard]] const EqualExpr &BitAndExpr::getEqualExpr() const { return mEqualExpr; }

[[nodiscard]] const std::vector<EqualExpr> &BitAndExpr::getOptionalEqualExpr() const {
  return mOptEqualExps;
}

BitXorExpr::BitXorExpr(BitAndExpr &&bitAndExpr, std::vector<BitAndExpr> &&optBitAndExps)
    : mBitAndExpr(std::move(bitAndExpr)),
      mOptBitAndExps(std::move(optBitAndExps)) {}
[[nodiscard]] const BitAndExpr &BitXorExpr::getBitAndExpr() const { return mBitAndExpr; }
[[nodiscard]] const std::vector<BitAndExpr> &
BitXorExpr::getOptionalBitAndExpressions() const {
  return mOptBitAndExps;
}

BitOrExpr::BitOrExpr(BitXorExpr &&bitXorExpr, std::vector<BitXorExpr> &&optBitXorExps)
    : mBitXorExpr(std::move(bitXorExpr)),
      mOptBitXorExps(std::move(optBitXorExps)) {}
[[nodiscard]] const BitXorExpr &BitOrExpr::getBitXorExpression() const {
  return mBitXorExpr;
}

[[nodiscard]] const std::vector<BitXorExpr> &
BitOrExpr::getOptionalBitXorExpressions() const {
  return mOptBitXorExps;
}

LogAndExpr::LogAndExpr(BitOrExpr &&bitOrExpr, std::vector<BitOrExpr> &&optBitOrExps)
    : mBitOrExpr(std::move(bitOrExpr)),
      mOptBitOrExps(std::move(optBitOrExps)) {}
[[nodiscard]] const BitOrExpr &LogAndExpr::getBitOrExpression() const {
  return mBitOrExpr;
}
[[nodiscard]] const std::vector<BitOrExpr> &
LogAndExpr::getOptionalBitOrExpressions() const {
  return mOptBitOrExps;
}

LogOrExpr::LogOrExpr(LogAndExpr &&logAndExpr, std::vector<LogAndExpr> &&optLogAndExps)
    : mLogAndExpr(std::move(logAndExpr)),
      mOptLogAndExps(std::move(optLogAndExps)) {}
[[nodiscard]] const LogAndExpr &LogOrExpr::getAndExpression() const {
  return mLogAndExpr;
}
[[nodiscard]] const std::vector<LogAndExpr> &
LogOrExpr::getOptionalAndExpressions() const {
  return mOptLogAndExps;
}

ConditionalExpr::ConditionalExpr(
    LogOrExpr &&logOrExpr, std::unique_ptr<Expr> &&optExpr,
    std::unique_ptr<ConditionalExpr> &&optCondExpr)
    : Node(), mLogOrExpr(std::move(logOrExpr)), mOptExpr(std::move(optExpr)),
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

AssignExpr::AssignExpr(ConditionalExpr &&conditionalExpression,
           std::vector<std::pair<AssignmentOperator, ConditionalExpr>>
               &&optConditionExpr)
    : mCondExpr(std::move(conditionalExpression)),
      mOptConditionExpr(std::move(optConditionExpr)) {}

[[nodiscard]] const ConditionalExpr &AssignExpr::getConditionalExpr() const {
  return mCondExpr;
}
[[nodiscard]] const std::vector<
    std::pair<AssignExpr::AssignmentOperator, ConditionalExpr>> &
AssignExpr::getOptionalConditionalExpr() const {
  return mOptConditionExpr;
}

Expr::Expr(std::vector<AssignExpr> assignExpressions)
    : mAssignExpressions(std::move(assignExpressions)) {}

const std::vector<AssignExpr> &Expr::getAssignExpressions() const {
  return mAssignExpressions;
}

ExprStmt::ExprStmt(std::unique_ptr<Expr> &&optExpr)
    : mOptExpr(std::move(optExpr)) {}
[[nodiscard]] const Expr *ExprStmt::getOptionalExpression() const {
  return mOptExpr.get();
}
std::unique_ptr<Expr> ExprStmt::moveOptionalExpr() { return std::move(mOptExpr); }

IfStmt::IfStmt(Expr &&expr, std::unique_ptr<Stmt> &&thenStmt,
       std::unique_ptr<Stmt> &&optElseStmt)
    : mExpr(std::move(expr)), mThenStmt(std::move(thenStmt)),
      mOptElseStmt(std::move(optElseStmt)) {}

[[nodiscard]] const Expr &IfStmt::getExpression() const { return mExpr; }

[[nodiscard]] const Stmt *IfStmt::getThenStmt() const { return mThenStmt.get(); }

[[nodiscard]] const Stmt *IfStmt::getElseStmt() const { return mOptElseStmt.get(); }


SwitchStmt::SwitchStmt(Expr &&expression, std::unique_ptr<Stmt> &&statement)
    : mExpr(std::move(expression)), mStmt(std::move(statement)) {}

[[nodiscard]] const Expr &SwitchStmt::getExpression() const { return mExpr; }

[[nodiscard]] const Stmt *SwitchStmt::getStatement() const { return mStmt.get(); }

DefaultStmt::DefaultStmt(std::unique_ptr<Stmt> &&statement)
    : mStmt(std::move(statement)) {}
[[nodiscard]] const Stmt *DefaultStmt::getStatement() const { return mStmt.get(); }

CaseStmt::CaseStmt(ConstantExpr &&constantExpr, std::unique_ptr<Stmt> &&statement)
    : mConstantExpr(std::move(constantExpr)),
      mStatement(std::move(statement)) {}

[[nodiscard]] const ConstantExpr &CaseStmt::getConstantExpr() const {
  return mConstantExpr;
}
[[nodiscard]] const Stmt *CaseStmt::getStatement() const { return mStatement.get(); }

LabelStmt::LabelStmt(std::string_view identifier) : mIdentifier(identifier) {}
[[nodiscard]] const std::string_view &LabelStmt::getIdentifier() const { return mIdentifier; }

GotoStmt::GotoStmt(std::string_view identifier) : mIdentifier(identifier) {}
[[nodiscard]] const std::string_view &GotoStmt::getIdentifier() const { return mIdentifier; }

DoWhileStmt::DoWhileStmt(std::unique_ptr<Stmt> &&stmt, Expr &&expr)
    : mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
[[nodiscard]] const Stmt *DoWhileStmt::getStatement() const { return mStmt.get(); }
[[nodiscard]] const Expr &DoWhileStmt::getExpression() const { return mExpr; }

WhileStmt::WhileStmt(Expr &&expr, std::unique_ptr<Stmt> &&stmt)
    : mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
[[nodiscard]] const Expr &WhileStmt::getExpression() const { return mExpr; }
[[nodiscard]] const Stmt *WhileStmt::getStatement() const { return mStmt.get(); }

ForStmt::ForStmt(std::unique_ptr<Stmt> &&stmt,
        std::variant<std::unique_ptr<Declaration>, std::unique_ptr<Expr>>
            &&initial,
        std::unique_ptr<Expr> &&controlExpr,
        std::unique_ptr<Expr> &&postExpr)
    : mStmt(std::move(stmt)), mInitial(std::move(initial)),
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

StorageClassSpecifier::StorageClassSpecifier(StorageClassSpecifier::Specifiers specifier) : mSpecifier(specifier) {}
[[nodiscard]] StorageClassSpecifier::Specifiers StorageClassSpecifier::getSpecifier() const { return mSpecifier; }

FunctionSpecifier::FunctionSpecifier() {}

DeclarationSpecifiers::DeclarationSpecifiers() {}
void DeclarationSpecifiers::addStorageClassSpecifier(StorageClassSpecifier && specifier) {
  mStorageClassSpecifiers.push_back(std::move(specifier));
}
void DeclarationSpecifiers::addTypeSpecifier(TypeSpecifier && specifier) {
  mTypeSpecifiers.push_back(std::move(specifier));
}
void DeclarationSpecifiers::addTypeQualifier(TypeQualifier && qualifier) {
  mTypeQualifiers.push_back(std::move(qualifier));
}
void DeclarationSpecifiers::addFunctionSpecifier(FunctionSpecifier && specifier) {
  mFunctionSpecifiers.push_back(std::move(specifier));
}

[[nodiscard]] const std::vector<StorageClassSpecifier> &DeclarationSpecifiers::getStorageClassSpecifiers() const {
  return mStorageClassSpecifiers;
}
[[nodiscard]] const std::vector<TypeSpecifier> &DeclarationSpecifiers::getTypeSpecifiers() const {
  return mTypeSpecifiers;
}
[[nodiscard]] const std::vector<TypeQualifier> &DeclarationSpecifiers::getTypeQualifiers() const {
  return mTypeQualifiers;
}
[[nodiscard]] const std::vector<FunctionSpecifier> &DeclarationSpecifiers::getFunctionSpecifiers() const {
  return mFunctionSpecifiers;
}
[[nodiscard]] bool DeclarationSpecifiers::isEmpty() const {
  return mStorageClassSpecifiers.empty() && mTypeSpecifiers.empty()
         && mTypeQualifiers.empty() && mFunctionSpecifiers.empty();
}

TypeName::TypeName(DeclarationSpecifiers &&specifierQualifiers,
         std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : mSpecifierQualifiers(std::move(specifierQualifiers)),
      mAbstractDeclarator(std::move(abstractDeclarator)) {}

[[nodiscard]] const DeclarationSpecifiers &TypeName::getSpecifierQualifiers() const {
  return mSpecifierQualifiers;
}
[[nodiscard]] const AbstractDeclarator *TypeName::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

Declaration::Declaration(DeclarationSpecifiers &&declarationSpecifiers,
            std::vector<InitDeclarator> &&initDeclarators)
    : mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mInitDeclarators(std::move(initDeclarators)) {}
[[nodiscard]] const DeclarationSpecifiers &
Declaration::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}
[[nodiscard]] const std::vector<Declaration::InitDeclarator> &Declaration::getInitDeclarators() const {
  return mInitDeclarators;
}

BreakStmt::BreakStmt() {}

ContinueStmt::ContinueStmt() {}

ReturnStmt::ReturnStmt(std::unique_ptr<Expr> &&optExpr)
    : mOptExpr(std::move(optExpr)) {}
[[nodiscard]] const Expr *ReturnStmt::getExpression() const { return mOptExpr.get(); }

BlockStmt::BlockStmt(std::vector<BlockItem> &&blockItems)
    : mBlockItems(std::move(blockItems)) {}
[[nodiscard]] const std::vector<BlockItem> &BlockStmt::getBlockItems() const {
  return mBlockItems;
}

DirectAbstractDeclaratorParentheses::DirectAbstractDeclaratorParentheses(
    std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
    : mAbstractDeclarator(std::move(abstractDeclarator)) {}

[[nodiscard]] const AbstractDeclarator *DirectAbstractDeclaratorParentheses::getAbstractDeclarator() const {
  return mAbstractDeclarator.get();
}

DirectAbstractDeclaratorAssignExpr::DirectAbstractDeclaratorAssignExpr(
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
    std::vector<TypeQualifier> &&typeQualifiers,
    std::unique_ptr<AssignExpr> &&assignmentExpression,
    bool hasStatic)
    : mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mTypeQualifiers(std::move(typeQualifiers)),
      mAssignmentExpression(std::move(assignmentExpression)),
      mHasStatic(hasStatic){}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorAssignExpr::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &DirectAbstractDeclaratorAssignExpr::getTypeQualifiers() const {
  return mTypeQualifiers;
}

[[nodiscard]] const AssignExpr *DirectAbstractDeclaratorAssignExpr::getAssignmentExpression() const {
  return mAssignmentExpression.get();
}

[[nodiscard]] bool DirectAbstractDeclaratorAssignExpr::hasStatic() const {
  return mHasStatic;
}

DirectAbstractDeclaratorAsterisk::DirectAbstractDeclaratorAsterisk(
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator)
    : mDirectAbstractDeclarator(std::move(directAbstractDeclarator)){}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorAsterisk::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

DirectAbstractDeclaratorParamTypeList::DirectAbstractDeclaratorParamTypeList(
    std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
    std::unique_ptr<ParamTypeList> &&parameterTypeList)
    : mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

[[nodiscard]] const DirectAbstractDeclarator *
DirectAbstractDeclaratorParamTypeList::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator.get();
}

[[nodiscard]] const ParamTypeList *DirectAbstractDeclaratorParamTypeList::getParameterTypeList() const {
  return mParameterTypeList.get();
}

Pointer::Pointer(std::vector<TypeQualifier> &&typeQualifiers)
    : mTypeQualifiers(std::move(typeQualifiers)) {}

[[nodiscard]] const std::vector<TypeQualifier> &Pointer::getTypeQualifiers() const {
  return mTypeQualifiers;
}

AbstractDeclarator::AbstractDeclarator(
    std::vector<Pointer> &&pointers,
    std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator)
    : mPointers(std::move(pointers)),
      mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

[[nodiscard]] const std::vector<Pointer> &AbstractDeclarator::getPointers() const {
  return mPointers;
}

[[nodiscard]] const DirectAbstractDeclarator *
AbstractDeclarator::getDirectAbstractDeclarator() const {
  return mDirectAbstractDeclarator ? &*mDirectAbstractDeclarator : nullptr;
}

ParameterDeclaration::ParameterDeclaration(DeclarationSpecifiers declarationSpecifiers,
                     std::variant<std::unique_ptr<Declarator>,
                                  std::optional<std::unique_ptr<AbstractDeclarator>>>
                         variant)
    : declarationSpecifiers(std::move(declarationSpecifiers)),
      declarator(std::move(variant)) {}

ParamList::ParamList(std::vector<ParameterDeclaration> &&parameterList)
    : mParameterList(std::move(parameterList)) {}

[[nodiscard]] const std::vector<ParameterDeclaration> &
ParamList::getParameterDeclarations() const {
  return mParameterList;
}

ParamTypeList::ParamTypeList(ParamList &&parameterList, bool hasEllipse)
    : mParameterList(std::move(parameterList)), mHasEllipse(hasEllipse) {}

[[nodiscard]] const ParamList &ParamTypeList::getParameterList() const {
  return mParameterList;
}

[[nodiscard]] bool ParamTypeList::hasEllipse() const { return mHasEllipse; }

DirectDeclaratorIdent::DirectDeclaratorIdent(std::string_view ident)
    : mIdent(ident) {}

[[nodiscard]] const std::string_view& DirectDeclaratorIdent::getIdent() const { return mIdent; }

DirectDeclaratorParentheses::DirectDeclaratorParentheses(std::unique_ptr<Declarator> &&declarator)
    : mDeclarator(std::move(declarator)) {}

[[nodiscard]] const Declarator *DirectDeclaratorParentheses::getDeclarator() const {
  return mDeclarator.get();
}

DirectDeclaratorParamTypeList::DirectDeclaratorParamTypeList(std::unique_ptr<DirectDeclarator> &&directDeclarator,
                              ParamTypeList &&parameterTypeList)
    : mDirectDeclarator(std::move(directDeclarator)),
      mParameterTypeList(std::move(parameterTypeList)) {}

[[nodiscard]] const DirectDeclarator *DirectDeclaratorParamTypeList::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const ParamTypeList &DirectDeclaratorParamTypeList::getParameterTypeList() const {
  return mParameterTypeList;
}

DirectDeclaratorAssignExpr::DirectDeclaratorAssignExpr(
    std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifierList,
    std::unique_ptr<AssignExpr> &&assignmentExpression,
    bool hasStatic)
    : mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifierList(std::move(typeQualifierList)),
      mAssignmentExpression(std::move(assignmentExpression)),
      mHasStatic(hasStatic){}

[[nodiscard]] const DirectDeclarator *DirectDeclaratorAssignExpr::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &DirectDeclaratorAssignExpr::getTypeQualifierList() const {
  return mTypeQualifierList;
}

[[nodiscard]] const AssignExpr *
DirectDeclaratorAssignExpr::getAssignmentExpression() const {
  return mAssignmentExpression == nullptr ? nullptr : mAssignmentExpression.get();
}

[[nodiscard]] bool DirectDeclaratorAssignExpr::hasStatic() const {
  return mHasStatic;
}

DirectDeclaratorAsterisk::DirectDeclaratorAsterisk(
    std::unique_ptr<DirectDeclarator> &&directDeclarator,
    std::vector<TypeQualifier> &&typeQualifierList)
    : mDirectDeclarator(std::move(directDeclarator)),
      mTypeQualifierList(std::move(typeQualifierList)){}

[[nodiscard]] const DirectDeclarator *DirectDeclaratorAsterisk::getDirectDeclarator() const {
  return mDirectDeclarator.get();
}

[[nodiscard]] const std::vector<TypeQualifier> &DirectDeclaratorAsterisk::getTypeQualifierList() const {
  return mTypeQualifierList;
}

Declarator::Declarator(std::vector<Pointer> &&pointers,
           DirectDeclarator &&directDeclarator)
    : mPointers(std::move(pointers)),
      mDirectDeclarator(std::move(directDeclarator)) {}

[[nodiscard]] const std::vector<Pointer> &Declarator::getPointers() const {
  return mPointers;
}

[[nodiscard]] const DirectDeclarator &Declarator::getDirectDeclarator() const {
  return mDirectDeclarator;
}

StructOrUnionSpecifier::StructOrUnionSpecifier(bool isUnion, std::string_view identifier,
                       std::vector<StructDeclaration> &&structDeclarations)
    : mIsUnion(isUnion), mId(identifier),
      mStructDeclarations(std::move(structDeclarations)) {}

[[nodiscard]] bool StructOrUnionSpecifier::isUnion() const { return mIsUnion; }

[[nodiscard]] std::string_view StructOrUnionSpecifier::getTag() const { return mId; }

[[nodiscard]] const std::vector<StructOrUnionSpecifier::StructDeclaration> &
StructOrUnionSpecifier::getStructDeclarations() const {
  return mStructDeclarations;
}

EnumSpecifier::EnumSpecifier(std::string_view id, std::vector<Enumerator> &&enumerators)
    : mId(id), mEnumerators(std::move(enumerators)) {}

[[nodiscard]] const std::string_view &EnumSpecifier::getName() const {
  return mId;
}
[[nodiscard]] const std::vector<EnumSpecifier::Enumerator> &EnumSpecifier::getEnumerators() const {
  return mEnumerators;
}

Initializer::Initializer(Initializer::variant &&variant) : mVariant(std::move(variant)) {}

[[nodiscard]] const Initializer::variant &Initializer::getVariant() const { return mVariant; }

InitializerList::InitializerList(InitializerList::vector &&initializer)
    : mInitializer(std::move(initializer)) {}

const InitializerList::vector &InitializerList::getInitializerList() const { return mInitializer; }

FunctionDefinition::FunctionDefinition(DeclarationSpecifiers &&declarationSpecifiers,
                   Declarator &&declarator, BlockStmt &&compoundStatement)
    : mDeclarationSpecifiers(std::move(declarationSpecifiers)),
      mDeclarator(std::move(declarator)),
      mCompoundStatement(std::move(compoundStatement)) {}

[[nodiscard]] const DeclarationSpecifiers & FunctionDefinition::getDeclarationSpecifiers() const {
  return mDeclarationSpecifiers;
}

[[nodiscard]] const Declarator &FunctionDefinition::getDeclarator() const { return mDeclarator; }

[[nodiscard]] const BlockStmt &FunctionDefinition::getCompoundStatement() const {
  return mCompoundStatement;
}
}