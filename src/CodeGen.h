/***********************************
 * File:     CodeGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/21
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_CODEGEN_H
#define LCC_CODEGEN_H
#include "Expected.h"
#include "FailureReason.h"
#include "Semantics.h"
#include "Syntax.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <map>

namespace lcc {
using NodeRetType =
    Expected<std::pair<llvm::Value *, Semantics::Type>, FailureReason>;
using TypeRetType = llvm::Type *;
class CodeGen {
public:
  std::vector<std::map<std::string, std::pair<llvm::Value *, Semantics::Type>>>
      m_namedValues{1};
  std::vector<std::map<std::string, Semantics::Type>> m_structsUnions{1};
  std::vector<std::map<std::string, Semantics::Type>> m_typedefs{1};

  llvm::LLVMContext context;
  llvm::IRBuilder<> builder{context};
  std::vector<llvm::BasicBlock *> continueBlocks;
  std::vector<llvm::BasicBlock *> breakBlocks;
  std::vector<std::pair<llvm::SwitchInst *, bool>> switchStack;
  const Semantics::FunctionType *currentFunction;

  llvm::Constant *createZeroValue(llvm::Type *type);

  std::map<std::string, std::reference_wrapper<const Semantics::Type>>
  gatherTypedefs() const;

  std::map<std::string, Semantics::RecordType> gatherStructsAndUnions() const;

  void popScope() {
    m_namedValues.pop_back();
    m_structsUnions.pop_back();
    m_typedefs.pop_back();
  }

  const std::pair<llvm::Value *, Semantics::Type> *
  findValue(const std::string &name) const {
    for (auto iter = m_namedValues.rbegin(); iter != m_namedValues.rend();
         iter++) {
      auto result = iter->find(name);
      if (result != iter->end()) {
        return &result->second;
      }
    }
    return nullptr;
  }

  const Semantics::Type *findStructUnionOrEnum(const std::string &name) const {
    for (auto iter = m_structsUnions.rbegin(); iter != m_structsUnions.rend();
         iter++) {
      auto result = iter->find(name);
      if (result != iter->end()) {
        return &result->second;
      }
    }
    return nullptr;
  }

  void pushScope() {
    m_namedValues.emplace_back();
    m_structsUnions.emplace_back();
    m_typedefs.emplace_back();
  }

  void clearScope() {
    m_namedValues.clear();
    pushScope();
  }

  bool inGlobalScope() const { return m_namedValues.size() == 1; }

  NodeRetType makeDivide(Semantics::Type leftType, llvm::Value *left,
                         Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeRemainder(Semantics::Type leftType, llvm::Value *left,
                            Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeMultiply(Semantics::Type leftType, llvm::Value *left,
                           Semantics::Type rightType, llvm::Value *right);

  NodeRetType makePlus(Semantics::Type leftType, llvm::Value *left,
                       Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeMinus(Semantics::Type leftType, llvm::Value *left,
                        Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeLeftShift(Semantics::Type leftType, llvm::Value *left,
                            Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeRightShift(Semantics::Type leftType, llvm::Value *left,
                             Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeBitAnd(Semantics::Type leftType, llvm::Value *left,
                         Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeBitXor(Semantics::Type leftType, llvm::Value *left,
                         Semantics::Type rightType, llvm::Value *right);

  NodeRetType makeBitOr(Semantics::Type leftType, llvm::Value *left,
                        Semantics::Type rightType, llvm::Value *right);

  llvm::Value *castTo(const Semantics::Type &sourceType, llvm::Value *source,
                      const Semantics::Type &destinationType,
                      bool explicitConversion = false);

  llvm::Value *toBool(llvm::Value *source);

  void arithmeticCast(Semantics::Type &type, llvm::Value *&value,
                      const Semantics::Type &otherType);

  Semantics::Type integerPromotion(const Semantics::Type &type,
                                   llvm::Value **optionalValue = nullptr);
  llvm::Type *convertType(const Semantics::Type &type);

public:
  std::unique_ptr<llvm::Module> module;

  NodeRetType visit(const Syntax::Expr &node);

  NodeRetType visit(const Syntax::PrimaryExprIdentifier &node);

  NodeRetType visit(const Syntax::PrimaryExprConstant &node);

  NodeRetType visit(const Syntax::PrimaryExprParent &node);

  NodeRetType visit(const Syntax::PostFixExprPrimary &node);

  NodeRetType visit(const Syntax::PostFixExprSubscript &node);

  NodeRetType visit(const Syntax::PostFixExprIncrement &node);

  NodeRetType visit(const Syntax::PostFixExprDecrement &node);

  NodeRetType visit(const Syntax::PostFixExprDot &node);

  NodeRetType visit(const Syntax::PostFixExprArrow &node);

  NodeRetType visit(const Syntax::PostFixExprFuncCall &node);

  NodeRetType visit(const Syntax::AssignExprAssign &node);

  NodeRetType visit(const Syntax::UnaryExprPostFixExpr &node);

  NodeRetType visit(const Syntax::UnaryExprUnaryOperator &node);

  NodeRetType visit(const Syntax::UnaryExprSizeOf &node);

  NodeRetType visit(const Syntax::CastExpr &node);

  NodeRetType visit(const Syntax::MultiExpr &node);

  NodeRetType visit(const Syntax::AdditiveExpr &node);

  NodeRetType visit(const Syntax::ShiftExpr &node);

  NodeRetType visit(const Syntax::RelationalExpr &node);

  NodeRetType visit(const Syntax::EqualExpr &node);

  NodeRetType visit(const Syntax::BitAndExpr &node);

  NodeRetType visit(const Syntax::BitXorExpr &node);

  NodeRetType visit(const Syntax::BitOrExpr &node);

  NodeRetType visit(const Syntax::LogAndExpr &node);

  NodeRetType visit(const Syntax::LogOrExpr &node);

  NodeRetType visit(const Syntax::ConditionalExpr &node);

  std::optional<FailureReason> visit(const Syntax::ReturnStmt &node);

  std::optional<FailureReason> visit(const Syntax::ExprStmt &node);

  std::optional<FailureReason> visit(const Syntax::IfStmt &node);

  std::optional<FailureReason> visit(const Syntax::SwitchStmt &node);

  std::optional<FailureReason> visit(const Syntax::DefaultStmt &node);

  std::optional<FailureReason> visit(const Syntax::CaseStmt &node);

  std::optional<FailureReason> visit(const Syntax::BlockStmt &node,
                                     bool pushScope = true);

  std::optional<FailureReason> visit(const Syntax::ForStmt &node);

  NodeRetType visit(const Syntax::InitializerList &node);

  std::optional<FailureReason> visit(const Syntax::Declaration &node);

  std::optional<FailureReason> visit(const Syntax::ForDeclarationStmt &node);

  std::optional<FailureReason> visit(const Syntax::WhileStmt &node);

  std::optional<FailureReason> visit(const Syntax::DoWhileStmt &node);

  std::optional<FailureReason> visit(const Syntax::BreakStmt &node);

  std::optional<FailureReason> visit(const Syntax::ContinueStmt &node);

  std::optional<FailureReason> visit(const Syntax::FunctionDefinition &node);

  std::optional<FailureReason> visit(const Syntax::TranslationUnit &node);

  NodeRetType visit(const Syntax::PrimaryExpr &node);

  NodeRetType visit(const Syntax::PostFixExpr &node);

  NodeRetType visit(const Syntax::UnaryExpr &node);

  NodeRetType visit(const Syntax::AssignExpr &node);

  NodeRetType visit(const Syntax::Initializer &node);

  std::optional<FailureReason> visit(const Syntax::BlockItem &node);

  std::optional<FailureReason> visit(const Syntax::Stmt &node);

  std::optional<FailureReason> visit(const Syntax::ExternalDeclaration &node);

  std::optional<FailureReason> visit(const Syntax::LabelStmt &node);

  std::optional<FailureReason> visit(const Syntax::GotoStmt &node);

  llvm::Type *visit(const Semantics::Type &node);

  llvm::Type *visit(const Semantics::PrimitiveType &node);

  llvm::Type *visit(const Semantics::ArrayType &node);

  llvm::Type *visit(const Semantics::AbstractArrayType &node);

  llvm::Type *visit(const Semantics::ValArrayType &node);

  llvm::Type *visit(const Semantics::FunctionType &node);

  llvm::Type *visit(const Semantics::RecordType &node);

  llvm::Type *visit(const Semantics::EnumType &node);

  llvm::Type *visit(const Semantics::PointerType &node);
};
} // namespace lcc

#endif // LCC_CODEGEN_H
