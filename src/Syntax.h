/***********************************
 * File:     Syntax.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/13
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SYNTAX_H
#define LCC_SYNTAX_H
#include <string>
#include <variant>
#include <vector>
#include "Token.h"
namespace lcc::parser {
class Node {
public:
  Node() = default;
  virtual ~Node() = default;
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
  Node(Node &&) = default;
  Node &operator=(Node &&) = default;
};

class Type {
public:
  Type() = default;
  virtual ~Type() = default;
};

enum class TypeKind {
  Auto,
  Char,
  Short,
  Int,
  Long,
  Float,
  Double,
  Signed,
  UnSigned
};
class PrimaryType final : public Type {
private:
  std::vector<TypeKind> mTypes;

public:
  explicit PrimaryType(std::vector<TypeKind> &&types) noexcept;
};

class PointerType final : public Type {
private:
  std::unique_ptr<Type> mType;

public:
  explicit PointerType(std::unique_ptr<Type> &&type) noexcept;
};

class AssignExpr;
class ConditionalExpr;
class UnaryExpr;
class LogOrExpr;
class LogAndExpr;
class BitOrExpr;
class BitXorExpr;
class BitAndExpr;
class EqualExpr;
class RelationalExpr;
class ShiftExpr;
class AdditiveExpr;
class MultiExpr;
class CastExpr;
class PostFixExpr;
class PrimaryExpr;

class Expr final : public Node {
private:
  std::unique_ptr<AssignExpr> mAssignExpr;
  std::vector<std::unique_ptr<AssignExpr>> mOptAssignExps;

public:
  explicit Expr(
      std::unique_ptr<AssignExpr> &&assignExpr,
      std::vector<std::unique_ptr<AssignExpr>> &&optAssignExps) noexcept;
};

class ConstantExpr final : public Node {
public:
  using ConstantValue = std::variant<int32_t, uint32_t, int64_t, uint64_t, float,
                             double, std::string>;

private:
  ConstantValue mValue;

public:
  explicit ConstantExpr(ConstantValue &value);
};

class AssignExpr final : public Node {
private:
  std::unique_ptr<ConditionalExpr> mCondExpr;
  lexer::TokenType mTokenType;
  std::unique_ptr<AssignExpr> mAssignExpr;

public:
  explicit AssignExpr(std::unique_ptr<ConditionalExpr> &&condExpr, lexer::TokenType tokenType,
                      std::unique_ptr<AssignExpr> &&assignExpr) noexcept;
};

class ConditionalExpr final : public Node {
private:
  std::unique_ptr<LogOrExpr> mLogOrExpr;
  std::unique_ptr<Expr> mOptExpr;
  std::unique_ptr<ConditionalExpr> mOptCondExpr;

public:
  explicit ConditionalExpr(
      std::unique_ptr<LogOrExpr> &&logOrExpr,
      std::unique_ptr<Expr> &&optExpr = nullptr,
      std::unique_ptr<ConditionalExpr> &&optCondExpr = nullptr) noexcept;
};

class LogOrExpr final : public Node {
private:
  std::unique_ptr<LogAndExpr> mLogAndExpr;
  std::vector<std::unique_ptr<LogAndExpr>> mOptLogAndExps;

public:
  explicit LogOrExpr(
      std::unique_ptr<LogAndExpr> &&logAndExpr,
      std::vector<std::unique_ptr<LogAndExpr>> &&optLogAndExps) noexcept;
};

class LogAndExpr final : public Node {
private:
  std::unique_ptr<BitOrExpr> mBitOrExpr;
  std::vector<std::unique_ptr<BitOrExpr>> mOptBitOrExps;

public:
  explicit LogAndExpr(
      std::unique_ptr<BitOrExpr> &&bitOrExpr,
      std::vector<std::unique_ptr<BitOrExpr>> &&optBitOrExps) noexcept;
};

class BitOrExpr final : public Node {
private:
  std::unique_ptr<BitXorExpr> mBitXorExpr;
  std::vector<std::unique_ptr<BitXorExpr>> mOptBitXorExps;

public:
  explicit BitOrExpr(
      std::unique_ptr<BitXorExpr> &&bitXorExpr,
      std::vector<std::unique_ptr<BitXorExpr>> &&optBitXorExps) noexcept;
};

class BitXorExpr final : public Node {
private:
  std::unique_ptr<BitAndExpr> mBitAndExpr;
  std::vector<std::unique_ptr<BitAndExpr>> mOptBitAndExps;

public:
  explicit BitXorExpr(
      std::unique_ptr<BitAndExpr> &&bitAndExpr,
      std::vector<std::unique_ptr<BitAndExpr>> &&optBitAndExps) noexcept;
};

class BitAndExpr final : public Node {
private:
  std::unique_ptr<EqualExpr> mEqualExpr;
  std::vector<std::unique_ptr<EqualExpr>> mOptEqualExps;

public:
  explicit BitAndExpr(
      std::unique_ptr<EqualExpr> &&equalExpr,
      std::vector<std::unique_ptr<EqualExpr>> &&optEqualExps) noexcept;
};

enum class EqualOp { Equal, NotEqual };

class EqualExpr final : public Node {
private:
  std::unique_ptr<RelationalExpr> mRelationalExpr;
  EqualOp mOp;
  std::vector<std::unique_ptr<RelationalExpr>> mOptRelationExps;

public:
  explicit EqualExpr(std::unique_ptr<RelationalExpr> &&relationalExpr,
                     EqualOp op,
                     std::vector<std::unique_ptr<RelationalExpr>>
                         &&optRelationalExps) noexcept;
};

enum class RelationOp { Less, Greater, LessEqual, GreaterEqual };

class RelationalExpr final : public Node {
private:
  std::unique_ptr<ShiftExpr> mShiftExpr;
  RelationOp mOp;
  std::vector<std::unique_ptr<ShiftExpr>> mOptShiftExps;

public:
  explicit RelationalExpr(
      std::unique_ptr<ShiftExpr> &&shiftExpr, RelationOp op,
      std::vector<std::unique_ptr<ShiftExpr>> &&optShiftExps) noexcept;
};

enum class ShiftOp { ShiftLeft, ShiftRight };

class ShiftExpr final : public Node {
private:
  std::unique_ptr<AdditiveExpr> mAdditiveExpr;
  ShiftOp mOp;
  std::vector<std::unique_ptr<AdditiveExpr>> mOptAdditiveExps;

public:
  explicit ShiftExpr(
      std::unique_ptr<AdditiveExpr> &&additiveExpr, ShiftOp op,
      std::vector<std::unique_ptr<AdditiveExpr>> &&optAdditiveExps) noexcept;
};

enum class AdditiveOp { Plus, Minus };

class AdditiveExpr final : public Node {
private:
  std::unique_ptr<MultiExpr> mMultiExpr;
  AdditiveOp mOp;
  std::vector<std::unique_ptr<MultiExpr>> mOptionalMultiExps;

public:
  explicit AdditiveExpr(
      std::unique_ptr<MultiExpr> &&multiExpr, AdditiveOp op,
      std::vector<std::unique_ptr<MultiExpr>> &&optionalMultiExps) noexcept;
};

enum class MultiOp { Mul, Div, Remainder };

class MultiExpr final : public Node {
private:
  std::unique_ptr<CastExpr> mCastExpr;
  MultiOp mOp;
  std::vector<std::unique_ptr<CastExpr>> mOptCastExps;

public:
  explicit MultiExpr(
      std::unique_ptr<CastExpr> &&castExpr, MultiOp op,
      std::vector<std::unique_ptr<CastExpr>> &&optCastExps) noexcept;
};

enum class CastExprCategory { Unary, LeftParent };

class CastExpr final : public Node {
private:
  CastExprCategory mCategory;
  std::unique_ptr<UnaryExpr> mUnaryExpr;
  std::unique_ptr<Type> mType;
  std::unique_ptr<CastExpr> mCastExpr;

public:
  explicit CastExpr(std::unique_ptr<UnaryExpr> &&unaryExpr) noexcept;
  explicit CastExpr(std::unique_ptr<Type> &&type,
                    std::unique_ptr<CastExpr> &&castExpr) noexcept;
};

enum class UnaryOp { Amp, Star, Plus, Minus, Tilde, Exclaim };

enum class UnaryCategory {
  PostFixExpr,
  PreInc,
  PreDec,
  UnaryOp,
  SizeOfUnary,
  SizeofType
};

class UnaryExpr final : public Node {
private:
  UnaryCategory mCategory;
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::unique_ptr<UnaryExpr> mUnaryExpr;
  UnaryOp mUnaryOp;
  std::unique_ptr<CastExpr> mCastExpr;
  std::unique_ptr<Type> mType;

public:
  explicit UnaryExpr(std::unique_ptr<PostFixExpr> &&postFixExpr) noexcept;
  explicit UnaryExpr(UnaryCategory unaryCategory,
                     std::unique_ptr<UnaryExpr> &&unaryExpr) noexcept;
  explicit UnaryExpr(UnaryOp unaryOp,
                     std::unique_ptr<CastExpr> &&castExpr) noexcept;
  explicit UnaryExpr(std::unique_ptr<Type> &&type) noexcept;
};

enum class PostFixExprCategory {
  ArrIndex,
  FuncCall,
  MemberDot,
  MemberArrow,
  PostInc,
  PostDec
};

class PostFixExpr final : public Node {
private:
  PostFixExprCategory mCategory;
  std::unique_ptr<PrimaryExpr> mPrimaryExpr;
  std::unique_ptr<Expr> mIndexExpr;
  std::vector<std::unique_ptr<AssignExpr>> mFuncParams;
  std::string mIdentifierName;

public:
  explicit PostFixExpr(std::unique_ptr<PrimaryExpr> &&primaryExpr,
                       std::unique_ptr<Expr> &&indexExpr) noexcept;
  explicit PostFixExpr(
      std::unique_ptr<PrimaryExpr> &&primaryExpr,
      std::vector<std::unique_ptr<AssignExpr>> &&funcParams) noexcept;
  explicit PostFixExpr(std::unique_ptr<PrimaryExpr> &&primaryExpr,
                       PostFixExprCategory category,
                       std::string identifier = "") noexcept;
};

enum class PrimaryExprCategory { Identifier, Constant, ParentExpr };

class PrimaryExpr final : public Node {
private:
  PrimaryExprCategory mCategory;
  std::string mIdentifierName;
  std::unique_ptr<ConstantExpr> mConstantExpr;
  std::unique_ptr<Expr> mExpr;

public:
  explicit PrimaryExpr(std::string &identifier) noexcept;
  explicit PrimaryExpr(std::unique_ptr<ConstantExpr> &&constantExpr) noexcept;
  explicit PrimaryExpr(std::unique_ptr<Expr> &&parentExpr) noexcept;
};

class Stmt : public Node {
public:
  Stmt() = default;
  virtual ~Stmt() = default;
};

class ExprStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  explicit ExprStmt(std::unique_ptr<Expr> &&optExpr = nullptr) noexcept;
};

class IfStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mExpr;
  std::unique_ptr<Stmt> mThenStmt;
  std::unique_ptr<Stmt> mOptElseStmt;

public:
  explicit IfStmt(std::unique_ptr<Expr> &&expr,
                  std::unique_ptr<Stmt> &&thenStmt,
                  std::unique_ptr<Stmt> &&optElseStmt = nullptr) noexcept;
};

class DoWhileStmt final : public Stmt {
private:
  std::unique_ptr<Stmt> mStmt;
  std::unique_ptr<Expr> mExpr;

public:
  explicit DoWhileStmt(std::unique_ptr<Stmt> &&stmt,
                       std::unique_ptr<Expr> &&expr) noexcept;
};

class WhileStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  explicit WhileStmt(std::unique_ptr<Expr> &&expr,
                     std::unique_ptr<Stmt> &&stmt) noexcept;
};

class ForStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mInitExpr;
  std::unique_ptr<Expr> mControlExpr;
  std::unique_ptr<Expr> mPostExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  explicit ForStmt(std::unique_ptr<Expr> &&initExpr,
                   std::unique_ptr<Expr> &&controlExpr,
                   std::unique_ptr<Expr> &&postExpr,
                   std::unique_ptr<Stmt> &&stmt) noexcept;
};

class Declaration final : public Stmt {
private:
  std::unique_ptr<Type> mType;
  std::string mName;
  std::unique_ptr<Expr> mOptValue;

public:
  explicit Declaration(
      std::unique_ptr<Type> &&type, std::string name,
      std::unique_ptr<Expr> &&optValue = nullptr) noexcept;
};

class ForDeclarationStmt final : public Stmt {
private:
  std::unique_ptr<Declaration> mInitDecl;
  std::unique_ptr<Expr> mControlExpr;
  std::unique_ptr<Expr> mPostExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  explicit ForDeclarationStmt(std::unique_ptr<Declaration> &&initDecl,
                              std::unique_ptr<Expr> &&controlExpr,
                              std::unique_ptr<Expr> &&postExpr,
                              std::unique_ptr<Stmt> &&stmt) noexcept;
};

class BreakStmt final : public Stmt {
public:
};

class ContinueStmt final : public Stmt {
public:
};

class ReturnStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mOptExpr;
public:
  explicit ReturnStmt(std::unique_ptr<Expr> &&optExpr = nullptr) noexcept;
};

class BlockStmt final : public Stmt {
private:
  std::vector<std::unique_ptr<Stmt>> mStmts;

public:
  explicit BlockStmt(std::vector<std::unique_ptr<Stmt>> &&stmts) noexcept;
};

class Function final : public Node {
private:
  std::unique_ptr<Type> mRetType;
  std::string mName;
  std::vector<std::pair<std::unique_ptr<Type>, std::string>> mParam;
  std::unique_ptr<BlockStmt> mOptBlockStmt;

public:
  explicit Function(
      std::unique_ptr<Type> &&retType, std::string name,
      std::vector<std::pair<std::unique_ptr<Type>, std::string>> &&params,
      std::unique_ptr<BlockStmt> &&optBlockStmt = nullptr) noexcept;
};

class GlobalDecl final : public Node {
private:
  std::unique_ptr<Type> mType;
  std::string mName;
  std::unique_ptr<ConstantExpr> mOptValue;

public:
  explicit GlobalDecl(
      std::unique_ptr<Type> &&type, std::string name,
      std::unique_ptr<ConstantExpr> &&optValue = nullptr) noexcept;
};

class Program final : public Node {
private:
  std::vector<std::unique_ptr<Function>> mFunctions;
  std::vector<std::unique_ptr<GlobalDecl>> mDeclarations;

public:
  explicit Program(
      std::vector<std::unique_ptr<Function>> &&functions,
      std::vector<std::unique_ptr<GlobalDecl>> &&declarations) noexcept;
};
} // namespace lcc::parser

#endif // LCC_SYNTAX_H
