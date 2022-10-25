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
#include "CodeGenContext.h"
#include "Token.h"
#include <string>
#include <variant>
#include <vector>
namespace lcc::parser {
class Type {
public:
  Type() = default;
  virtual ~Type() = default;
  virtual bool IsSigned() const = 0;
  virtual bool IsVoid() const = 0;
  virtual LLVMTypePtr TypeGen(CodeGenContext &context) = 0;
};

class PrimaryType final : public Type {
public:
  std::vector<lexer::TokenType> mTypes;
  bool mSign{true};
  bool mVoid{false};
public:
  explicit PrimaryType(std::vector<lexer::TokenType> &&types) noexcept;
  bool IsSigned() const override;
  bool IsVoid() const override;
  LLVMTypePtr TypeGen(CodeGenContext &context) override;
};

class PointerType final : public Type {
public:
  std::unique_ptr<Type> mType;

public:
  explicit PointerType(std::unique_ptr<Type> &&type) noexcept;
  bool IsSigned() const override;
  bool IsVoid() const override;
  LLVMTypePtr TypeGen(CodeGenContext &context) override;
};

class Node {
public:
  Node() = default;
  virtual ~Node() = default;
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
  Node(Node &&) = default;
  Node &operator=(Node &&) = default;
  virtual LLVMValueSignPair Codegen(CodeGenContext &context) const = 0;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class ConstantExpr final : public Node {
public:
  using ConstantValue = std::variant<int32_t, uint32_t, int64_t, uint64_t,
                                     float, double, std::string>;

private:
  ConstantValue mValue;

public:
  explicit ConstantExpr(ConstantValue &value);
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class AssignExpr final : public Node {
private:
  std::unique_ptr<ConditionalExpr> mCondExpr;
  lexer::TokenType mTokType;
  std::unique_ptr<AssignExpr> mAssignExpr;

public:
  explicit AssignExpr(
      std::unique_ptr<ConditionalExpr> &&condExpr,
      lexer::TokenType tokenType = lexer::unknown,
      std::unique_ptr<AssignExpr> &&assignExpr = nullptr) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class LogOrExpr final : public Node {
private:
  std::unique_ptr<LogAndExpr> mLogAndExpr;
  std::vector<std::unique_ptr<LogAndExpr>> mOptLogAndExps;

public:
  explicit LogOrExpr(
      std::unique_ptr<LogAndExpr> &&logAndExpr,
      std::vector<std::unique_ptr<LogAndExpr>> &&optLogAndExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class LogAndExpr final : public Node {
private:
  std::unique_ptr<BitOrExpr> mBitOrExpr;
  std::vector<std::unique_ptr<BitOrExpr>> mOptBitOrExps;

public:
  explicit LogAndExpr(
      std::unique_ptr<BitOrExpr> &&bitOrExpr,
      std::vector<std::unique_ptr<BitOrExpr>> &&optBitOrExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class BitOrExpr final : public Node {
private:
  std::unique_ptr<BitXorExpr> mBitXorExpr;
  std::vector<std::unique_ptr<BitXorExpr>> mOptBitXorExps;

public:
  explicit BitOrExpr(
      std::unique_ptr<BitXorExpr> &&bitXorExpr,
      std::vector<std::unique_ptr<BitXorExpr>> &&optBitXorExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class BitXorExpr final : public Node {
private:
  std::unique_ptr<BitAndExpr> mBitAndExpr;
  std::vector<std::unique_ptr<BitAndExpr>> mOptBitAndExps;

public:
  explicit BitXorExpr(
      std::unique_ptr<BitAndExpr> &&bitAndExpr,
      std::vector<std::unique_ptr<BitAndExpr>> &&optBitAndExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class BitAndExpr final : public Node {
private:
  std::unique_ptr<EqualExpr> mEqualExpr;
  std::vector<std::unique_ptr<EqualExpr>> mOptEqualExps;

public:
  explicit BitAndExpr(
      std::unique_ptr<EqualExpr> &&equalExpr,
      std::vector<std::unique_ptr<EqualExpr>> &&optEqualExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class EqualExpr final : public Node {
private:
  std::unique_ptr<RelationalExpr> mRelationalExpr;
  lexer::TokenType mTokType;
  std::vector<std::unique_ptr<RelationalExpr>> mOptRelationExps;

public:
  explicit EqualExpr(std::unique_ptr<RelationalExpr> &&relationalExpr,
                     lexer::TokenType tokenType,
                     std::vector<std::unique_ptr<RelationalExpr>>
                         &&optRelationalExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class RelationalExpr final : public Node {
private:
  std::unique_ptr<ShiftExpr> mShiftExpr;
  lexer::TokenType mTokType;
  std::vector<std::unique_ptr<ShiftExpr>> mOptShiftExps;

public:
  explicit RelationalExpr(
      std::unique_ptr<ShiftExpr> &&shiftExpr, lexer::TokenType tokenType,
      std::vector<std::unique_ptr<ShiftExpr>> &&optShiftExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class ShiftExpr final : public Node {
private:
  std::unique_ptr<AdditiveExpr> mAdditiveExpr;
  lexer::TokenType mTokType;
  std::vector<std::unique_ptr<AdditiveExpr>> mOptAdditiveExps;

public:
  explicit ShiftExpr(
      std::unique_ptr<AdditiveExpr> &&additiveExpr, lexer::TokenType tokenType,
      std::vector<std::unique_ptr<AdditiveExpr>> &&optAdditiveExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class AdditiveExpr final : public Node {
private:
  std::unique_ptr<MultiExpr> mMultiExpr;
  lexer::TokenType mTokType;
  std::vector<std::unique_ptr<MultiExpr>> mOptionalMultiExps;

public:
  explicit AdditiveExpr(
      std::unique_ptr<MultiExpr> &&multiExpr, lexer::TokenType tokenType,
      std::vector<std::unique_ptr<MultiExpr>> &&optionalMultiExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class MultiExpr final : public Node {
private:
  std::unique_ptr<CastExpr> mCastExpr;
  lexer::TokenType mTokType;
  std::vector<std::unique_ptr<CastExpr>> mOptCastExps;

public:
  explicit MultiExpr(
      std::unique_ptr<CastExpr> &&castExpr, lexer::TokenType tokenType,
      std::vector<std::unique_ptr<CastExpr>> &&optCastExps) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class UnaryExpr final : public Node {
public:
  struct PreIncTag {
    std::unique_ptr<UnaryExpr> mUnaryExpr;
  };

  struct PreDecTag {
    std::unique_ptr<UnaryExpr> mUnaryExpr;
  };

  struct UnaryOpTag {
    lexer::TokenType mTokType;
    std::unique_ptr<CastExpr> mCastExpr;
  };

  struct SizeofUnaryTag {
    std::unique_ptr<UnaryExpr> mUnaryExpr;
  };

  struct SizeofTypeTag {
    std::unique_ptr<Type> mType;
  };

  struct PostFixTag {
    std::unique_ptr<PostFixExpr> mPostFixExpr;
  };

private:
  using Variant = std::variant<PreIncTag, PreDecTag, UnaryOpTag, SizeofUnaryTag,
                               SizeofTypeTag, PostFixTag>;
  Variant mTag;

public:
  template <typename T>
  explicit UnaryExpr(T &&tag) noexcept : mTag(std::forward<T>(tag)) {}
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class PostFixExpr final : public Node {
public:
  struct ArrayIndexTag {
    std::unique_ptr<Expr> mExpr;
  };
  struct FuncCallTag {
    std::vector<std::unique_ptr<AssignExpr>> mOptParams;
  };
  struct MemberDotTag {
    std::string mIdentifier;
  };
  struct MemberArrowTag {
    std::string mIdentifier;
  };
  struct PostIncTag {};
  struct PostDecTag {};
  using Variant = std::variant<ArrayIndexTag, FuncCallTag, MemberDotTag,
                               MemberArrowTag, PostIncTag, PostDecTag>;

private:
  std::vector<Variant> mOptVariants;
  std::unique_ptr<PrimaryExpr> mPrimaryExpr;

public:
  explicit PostFixExpr(std::unique_ptr<PrimaryExpr> &&primaryExpr,
                       std::vector<Variant> &&optVariant) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class PrimaryExpr final : public Node {
public:
  struct IdentifierTag {
    std::string mIdentifier;
  };
  struct ConstantTag {
    std::unique_ptr<ConstantExpr> mConstantExpr;
  };
  struct ExprTag {
    std::unique_ptr<Expr> mExpr;
  };

private:
  using Variant = std::variant<IdentifierTag, ConstantTag, ExprTag>;
  Variant mTag;

public:
  template <typename T>
  explicit PrimaryExpr(T &&tag) noexcept : mTag(std::forward<T>(tag)) {}
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class DoWhileStmt final : public Stmt {
private:
  std::unique_ptr<Stmt> mStmt;
  std::unique_ptr<Expr> mExpr;

public:
  explicit DoWhileStmt(std::unique_ptr<Stmt> &&stmt,
                       std::unique_ptr<Expr> &&expr) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class WhileStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  explicit WhileStmt(std::unique_ptr<Expr> &&expr,
                     std::unique_ptr<Stmt> &&stmt) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class Declaration final : public Stmt {
private:
  std::unique_ptr<Type> mType;
  std::string mName;
  std::unique_ptr<Expr> mOptValue;

public:
  explicit Declaration(std::unique_ptr<Type> &&type, std::string name,
                       std::unique_ptr<Expr> &&optValue = nullptr) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class BreakStmt final : public Stmt {
public:
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class ContinueStmt final : public Stmt {
public:
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class ReturnStmt final : public Stmt {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> &&optExpr = nullptr) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class BlockStmt final : public Stmt {
private:
  std::vector<std::unique_ptr<Stmt>> mStmts;

public:
  explicit BlockStmt(std::vector<std::unique_ptr<Stmt>> &&stmts) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class ExternalDeclaration : public Node {
protected:
  ExternalDeclaration() = default;
};

class Function final : public ExternalDeclaration {
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
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class GlobalDecl final : public ExternalDeclaration {
private:
  std::unique_ptr<Type> mType;
  std::string mName;
  std::unique_ptr<ConstantExpr> mOptValue;

public:
  explicit GlobalDecl(
      std::unique_ptr<Type> &&type, std::string name,
      std::unique_ptr<ConstantExpr> &&optValue = nullptr) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};

class Program final : public Node {
private:
  std::vector<std::unique_ptr<ExternalDeclaration>> mExternalDecl;

public:
  explicit Program(std::vector<std::unique_ptr<ExternalDeclaration>>
                       &&externalDecl) noexcept;
  LLVMValueSignPair Codegen(CodeGenContext &context) const override;
};
} // namespace lcc::parser

#endif // LCC_SYNTAX_H
