/***********************************
 * File:     CodeGen.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/21
 *
 * Sign:     enjoy life
 ***********************************/
#include "CodeGen.h"
namespace lcc::parser{
bool PrimaryType::IsSigned() const {
  return mSign;
}
LLVMTypePtr PrimaryType::TypeGen(CodeGenContext &context) {
  int size = mTypes.size();
  assert(size>0);
  int cur = 0;
  if (mTypes[cur] == lexer::TokenType::kw_const) {
    cur++;
  }
  assert(cur < size);
  if (mTypes[cur] == lexer::TokenType::kw_signed) {
    mSign = true;
    cur++;
  }else if (mTypes[cur] == lexer::TokenType::kw_unsigned) {
    mSign = false;
    cur++;
  }
  if (cur == size) {
    return context.mIrBuilder.getInt32Ty();
  }
  switch (mTypes[cur]) {
  case lexer::TokenType::kw_char:
    return context.mIrBuilder.getInt8Ty();
  case lexer::TokenType::kw_short:
    return context.mIrBuilder.getInt16Ty();
  case lexer::TokenType::kw_int:
    return context.mIrBuilder.getInt32Ty();
  case lexer::TokenType::kw_long: {
    if (cur < size && mTypes[cur] == lexer::TokenType::kw_long) {
      ++cur;
      return context.mIrBuilder.getInt64Ty();
    } else {
      return context.mIrBuilder.getInt32Ty();
    }
  }
  case lexer::TokenType::kw_float:
    return context.mIrBuilder.getFloatTy();
  case lexer::TokenType::kw_double:
    return context.mIrBuilder.getDoubleTy();
  default:
    return nullptr;
  }
}
bool PointerType::IsSigned() const {
    return false;
}
LLVMTypePtr PointerType::TypeGen(CodeGenContext &context) {
    return llvm::PointerType::getUnqual(mType->TypeGen(context));
}
LLVMValueSignPair Program::Codegen(CodeGenContext &context) const {
    context.mModule = std::make_unique<llvm::Module>("main", context.mContext);
    for (auto &ext : mExternalDecl) {
      ext->Codegen(context);
    }
    return {nullptr, false};
}
LLVMValueSignPair GlobalDecl::Codegen(lcc::parser::CodeGenContext &context) const {
  LLVMTypePtr type = mType->TypeGen(context);
  llvm::Value *value = nullptr;
  if (!mOptValue) {
    if (type->isIntegerTy()) {
      value = llvm::ConstantInt::get(type, 0);
    }else if (type->isFloatingPointTy()) {
      value = llvm::ConstantFP::get(type, 0);
    }
  }
  auto [constant, sign] = mOptValue ? mOptValue->Codegen(context) : LLVMValueSignPair{value, mType->IsSigned()};
  context.mModule->getOrInsertGlobal(mName, type);
  auto *globalVar = context.mModule->getGlobalVariable(mName);
  globalVar->setInitializer(llvm::cast<llvm::Constant>(constant));
  return {constant, sign};
}
LLVMValueSignPair Function::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair BlockStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair IfStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair WhileStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair DoWhileStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ForStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair
ForDeclarationStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ExprStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ReturnStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair BreakStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ContinueStmt::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair Declaration::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair Expr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair AssignExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ConditionalExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair LogOrExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair LogAndExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair BitOrExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair BitXorExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair BitAndExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair EqualExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair RelationalExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ShiftExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair AdditiveExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair MultiExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair CastExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair UnaryExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair PostFixExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair PrimaryExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ConstantExpr::Codegen(lcc::parser::CodeGenContext &context) const {
  return {};
}
}
namespace lcc::codegen {

}