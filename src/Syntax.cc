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

namespace lcc::parser {
PrimaryType::PrimaryType(std::vector<TypeKind> &types) noexcept
    : mTypes(types) {}
PointerType::PointerType(std::unique_ptr<Type> &&type) noexcept
    : mType(std::move(type)) {}

Expr::Expr(std::unique_ptr<AssignExpr> &&assignExpr,
           std::vector<std::unique_ptr<AssignExpr>> &&optAssignExps) noexcept
    : mAssignExpr(std::move(assignExpr)),
      mOptAssignExps(std::move(optAssignExps)) {}

ConstantExpr::ConstantExpr(Value &&value) noexcept : mValue(value) {}

AssignExpr::AssignExpr(std::unique_ptr<UnaryExpr> &&unaryExpr,
                       AssignOp assignOp,
                       std::unique_ptr<AssignExpr> &&assignExpr) noexcept
    : mUnaryExpr(std::move(unaryExpr)), mOp(assignOp),
      mAssignExpr(std::move(assignExpr)),
      mCategory(AssignExprCategory::UnaryExpr){};
AssignExpr::AssignExpr(std::unique_ptr<ConditionalExpr> &&condExpr) noexcept
    : mCondExpr(std::move(condExpr)),
      mCategory(AssignExprCategory::ConditionalExpr){};

ConditionalExpr::ConditionalExpr(
    std::unique_ptr<LogOrExpr> &&logOrExpr, std::unique_ptr<Expr> &&optExpr,
    std::unique_ptr<ConditionalExpr> &&optCondExpr) noexcept
    : mLogOrExpr(std::move(logOrExpr)), mOptExpr(std::move(optExpr)),
      mOptCondExpr(std::move(optCondExpr)) {}

LogOrExpr::LogOrExpr(
    std::unique_ptr<LogAndExpr> &&logAndExpr,
    std::vector<std::unique_ptr<LogAndExpr>> &&optLogAndExps) noexcept
    : mLogAndExpr(std::move(logAndExpr)),
      mOptLogAndExps(std::move(optLogAndExps)) {}

LogAndExpr::LogAndExpr(
    std::unique_ptr<BitOrExpr> &&bitOrExpr,
    std::vector<std::unique_ptr<BitOrExpr>> &&optBitOrExps) noexcept
    : mBitOrExpr(std::move(bitOrExpr)), mOptBitOrExps(std::move(optBitOrExps)) {
}

BitOrExpr::BitOrExpr(
    std::unique_ptr<BitXorExpr> &&bitXorExpr,
    std::vector<std::unique_ptr<BitXorExpr>> &&optBitXorExps) noexcept
    : mBitXorExpr(std::move(bitXorExpr)),
      mOptBitXorExps(std::move(optBitXorExps)) {}

BitXorExpr::BitXorExpr(
    std::unique_ptr<BitAndExpr> &&bitAndExpr,
    std::vector<std::unique_ptr<BitAndExpr>> &&optBitAndExps) noexcept
    : mBitAndExpr(std::move(bitAndExpr)),
      mOptBitAndExps(std::move(optBitAndExps)) {}

BitAndExpr::BitAndExpr(
    std::unique_ptr<EqualExpr> &&equalExpr,
    std::vector<std::unique_ptr<EqualExpr>> &&optEqualExps) noexcept
    : mEqualExpr(std::move(equalExpr)), mOptEqualExps(std::move(optEqualExps)) {
}

EqualExpr::EqualExpr(
    std::unique_ptr<RelationalExpr> &&relationalExpr, EqualOp op,
    std::vector<std::unique_ptr<RelationalExpr>> &&optRelationalExps) noexcept
    : mRelationalExpr(std::move(relationalExpr)), mOp(op),
      mOptRelationExps(std::move(optRelationalExps)) {}

RelationalExpr::RelationalExpr(
    std::unique_ptr<ShiftExpr> &&shiftExpr, RelationOp op,
    std::vector<std::unique_ptr<ShiftExpr>> &&optShiftExps) noexcept
    : mShiftExpr(std::move(shiftExpr)), mOp(op),
      mOptShiftExps(std::move(optShiftExps)) {}

ShiftExpr::ShiftExpr(
    std::unique_ptr<AdditiveExpr> &&additiveExpr, ShiftOp op,
    std::vector<std::unique_ptr<AdditiveExpr>> &&optAdditiveExps) noexcept
    : mAdditiveExpr(std::move(additiveExpr)), mOp(op),
      mOptAdditiveExps(std::move(optAdditiveExps)) {}

AdditiveExpr::AdditiveExpr(
    std::unique_ptr<MultiExpr> &&multiExpr, AdditiveOp op,
    std::vector<std::unique_ptr<MultiExpr>> &&optionalMultiExps) noexcept
    : mMultiExpr(std::move(multiExpr)), mOp(op),
      mOptionalMultiExps(std::move(optionalMultiExps)) {}

MultiExpr::MultiExpr(
    std::unique_ptr<CastExpr> &&castExpr, MultiOp op,
    std::vector<std::unique_ptr<CastExpr>> &&optCastExps) noexcept
    : mCastExpr(std::move(castExpr)), mOp(op),
      mOptCastExps(std::move(optCastExps)) {}

CastExpr::CastExpr(std::unique_ptr<UnaryExpr> &&unaryExpr) noexcept
    : mUnaryExpr(std::move(unaryExpr)), mCategory(CastExprCategory::Unary),
      mType(nullptr), mCastExpr(nullptr) {}

CastExpr::CastExpr(std::unique_ptr<Type> &&type,
                   std::unique_ptr<CastExpr> &&castExpr) noexcept
    : mUnaryExpr(nullptr), mCategory(CastExprCategory::LeftParent),
      mType(std::move(type)), mCastExpr(std::move(castExpr)) {}

UnaryExpr::UnaryExpr(std::unique_ptr<PostFixExpr> &&postFixExpr) noexcept
    : mPostFixExpr(std::move(postFixExpr)),
      mCategory(UnaryCategory::PostFixExpr), mUnaryExpr(nullptr),
      mCastExpr(nullptr), mType(nullptr) {}
UnaryExpr::UnaryExpr(UnaryCategory unaryCategory,
                     std::unique_ptr<UnaryExpr> &&unaryExpr) noexcept
    : mPostFixExpr(nullptr), mCategory(unaryCategory),
      mUnaryExpr(std::move(unaryExpr)), mCastExpr(nullptr), mType(nullptr) {}
UnaryExpr::UnaryExpr(UnaryOp unaryOp,
                     std::unique_ptr<CastExpr> &&castExpr) noexcept
    : mPostFixExpr(nullptr), mCategory(UnaryCategory::UnaryOp),
      mUnaryExpr(nullptr), mCastExpr(std::move(castExpr)), mType(nullptr) {}
UnaryExpr::UnaryExpr(std::unique_ptr<Type> &&type) noexcept
    : mPostFixExpr(nullptr), mCategory(UnaryCategory::SizeofType),
      mUnaryExpr(nullptr), mCastExpr(nullptr), mType(std::move(type)) {}

PostFixExpr::PostFixExpr(std::unique_ptr<PrimaryExpr> &&primaryExpr,
                         std::unique_ptr<Expr> &&indexExpr) noexcept
    : mCategory(PostFixExprCategory::ArrIndex),
      mPrimaryExpr(std::move(primaryExpr)), mIndexExpr(std::move(indexExpr)),
      mIdentifierName("") {}
PostFixExpr::PostFixExpr(
    std::unique_ptr<PrimaryExpr> &&primaryExpr,
    std::vector<std::unique_ptr<AssignExpr>> &&funcParams) noexcept
    : mCategory(PostFixExprCategory::FuncCall),
      mPrimaryExpr(std::move(primaryExpr)), mIndexExpr(nullptr),
      mFuncParams(std::move(funcParams)), mIdentifierName("") {}
PostFixExpr::PostFixExpr(std::unique_ptr<PrimaryExpr> &&primaryExpr,
                         PostFixExprCategory category,
                         std::string identifier) noexcept
    : mCategory(category), mPrimaryExpr(std::move(primaryExpr)),
      mIndexExpr(nullptr), mIdentifierName(identifier) {}

PrimaryExpr::PrimaryExpr(std::string &identifier) noexcept
    : mCategory(PrimaryExprCategory::Identifier), mIdentifierName(identifier),
      mConstantExpr(nullptr), mExpr(nullptr) {}
PrimaryExpr::PrimaryExpr(std::unique_ptr<ConstantExpr> &&constantExpr) noexcept
    : mCategory(PrimaryExprCategory::Constant), mIdentifierName(""),
      mConstantExpr(std::move(constantExpr)), mExpr(nullptr) {}
PrimaryExpr::PrimaryExpr(std::unique_ptr<Expr> &&parentExpr) noexcept
    : mCategory(PrimaryExprCategory::ParentExpr), mIdentifierName(""),
      mConstantExpr(nullptr), mExpr(std::move(parentExpr)) {}
ExprStmt::ExprStmt(std::unique_ptr<Expr> &&optExpr) noexcept
    : mOptExpr(std::move(optExpr)) {}

IfStmt::IfStmt(std::unique_ptr<Expr> &&expr, std::unique_ptr<Stmt> &&thenStmt,
               std::unique_ptr<Stmt> &&optElseStmt) noexcept
    : mExpr(std::move(expr)), mThenStmt(std::move(thenStmt)),
      mOptElseStmt(std::move(optElseStmt)) {}

DoWhileStmt::DoWhileStmt(std::unique_ptr<Stmt> &&stmt,
                         std::unique_ptr<Expr> &&expr) noexcept
    : mStmt(std::move(stmt)), mExpr(std::move(expr)) {}

WhileStmt::WhileStmt(std::unique_ptr<Expr> &&expr,
                     std::unique_ptr<Stmt> &&stmt) noexcept
    : mExpr(std::move(expr)), mStmt(std::move(stmt)) {}

ForStmt::ForStmt(std::unique_ptr<Expr> &&initExpr,
                 std::unique_ptr<Expr> &&controlExpr,
                 std::unique_ptr<Expr> &&postExpr,
                 std::unique_ptr<Stmt> &&stmt) noexcept
    : mInitExpr(std::move(initExpr)), mControlExpr(std::move(controlExpr)),
      mPostExpr(std::move(postExpr)), mStmt(std::move(stmt)) {}

Declaration::Declaration(std::unique_ptr<Type> &&type, std::string name,
                         std::unique_ptr<ConstantExpr> &&optValue) noexcept
    : mType(std::move(type)), mName(name), mOptValue(std::move(optValue)) {}

ForDeclarationStmt::ForDeclarationStmt(std::unique_ptr<Declaration> &&initDecl,
                                       std::unique_ptr<Expr> &&controlExpr,
                                       std::unique_ptr<Expr> &&postExpr,
                                       std::unique_ptr<Stmt> &&stmt) noexcept
    : mInitDecl(std::move(initDecl)), mControlExpr(std::move(controlExpr)),
      mPostExpr(std::move(postExpr)), mStmt(std::move(stmt)) {}

BlockStmt::BlockStmt(std::vector<std::unique_ptr<Stmt>> &&stmts) noexcept
    : mStmts(std::move(stmts)) {}

Function::Function(
    std::unique_ptr<Type> &&retType, std::string name,
    std::vector<std::pair<std::unique_ptr<Type>, std::string>> &&params,
    std::unique_ptr<BlockStmt> &&optBlockStmt) noexcept
    : mRetType(std::move(retType)), mName(name), mParam(std::move(params)),
      mOptBlockStmt(std::move(optBlockStmt)) {}

GlobalDecl::GlobalDecl(std::unique_ptr<Type> &&type, std::string name,
                       std::unique_ptr<ConstantExpr> &&optValue) noexcept
    : mType(std::move(type)), mName(name), mOptValue(std::move(optValue)) {}

Program::Program(
    std::vector<std::unique_ptr<Function>> &&functions,
    std::vector<std::unique_ptr<GlobalDecl>> &&declarations) noexcept
    : mFunctions(std::move(functions)), mDeclarations(std::move(declarations)) {
}
} // namespace lcc::parser