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
bool PrimaryType::IsVoid() const {
  return mVoid;
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
  case lexer::TokenType::kw_void:
    return context.mIrBuilder.getVoidTy();
  case lexer::TokenType::kw_char:
    return context.mIrBuilder.getInt8Ty();
  case lexer::TokenType::kw_short:
    return context.mIrBuilder.getInt16Ty();
  case lexer::TokenType::kw_int:
    return context.mIrBuilder.getInt32Ty();
  case lexer::TokenType::kw_long: {
    if (cur < size && mTypes[cur+1] == lexer::TokenType::kw_long) {
      ++cur;
      return context.mIrBuilder.getInt64Ty();
    } else {
      return context.mIrBuilder.getInt64Ty();
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
bool PointerType::IsVoid() const {
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
    if (llvm::verifyModule(*context.mModule)) {
      context.mModule->print(llvm::errs(), nullptr);
      std::terminate();
    }else {
      context.mModule->print(llvm::outs(), nullptr);
    }
    return {nullptr, false};
}
LLVMValueSignPair GlobalDecl::Codegen(lcc::CodeGenContext &context) const {
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
LLVMValueSignPair Function::Codegen(lcc::CodeGenContext &context) const {
  LLVMTypePtr retType = mRetType->TypeGen(context);
  std::vector<LLVMTypePtr> paramType;
  std::vector<std::string> paramName;
  for (auto &p : mParam) {
    paramType.push_back(p.first->TypeGen(context));
    paramName.push_back(p.second);
  }
  auto *funType = llvm::FunctionType::get(retType, paramType, false);
  auto *func = llvm::Function::Create(funType, llvm::GlobalVariable::ExternalLinkage, mName, context.mModule.get());
  int i = -1;
  for (auto &iter : func->args()) {
    ++i;
    if (paramName[i].empty())
      continue;
    iter.setName(paramName[i]);
  }

  if (!mOptBlockStmt)
    return {func, false};

  context.mCurrentFunc = func;
  auto *entryBB = llvm::BasicBlock::Create(context.mContext, "entry", func);
  context.mIrBuilder.SetInsertPoint(entryBB);
  for (auto &iter : func->args()) {
    auto *address = context.mIrBuilder.CreateAlloca(iter.getType(), nullptr, iter.getName());
    context.mIrBuilder.CreateStore(&iter, address);
  }
  mOptBlockStmt->Codegen(context);

  auto &block = func->back();
  if (block.empty() || !block.back().isTerminator()) {
    if (retType->isVoidTy()) {
      context.mIrBuilder.CreateRetVoid();
    }else {
      assert(0);
    }
  }
  if (llvm::verifyFunction(*func, &llvm::errs())) {
    func->print(llvm::errs());
    std::terminate();
  }
  return {func, false};
}
LLVMValueSignPair BlockStmt::Codegen(lcc::CodeGenContext &context) const {
  for (auto &stmt : mStmts)
    stmt->Codegen(context);
  return {nullptr, false};
}
LLVMValueSignPair IfStmt::Codegen(lcc::CodeGenContext &context) const {
  auto[value, sign] = mExpr->Codegen(context);
//  auto [value, sign] = LLVMValueSignPair{context.mIrBuilder.getInt32(1), true};
  if (value->getType()->isIntegerTy()) {
      value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }
  auto* function = context.mCurrentFunc;
  auto *thenBB = llvm::BasicBlock::Create(context.mContext, "", function);
  auto *elseBB =  mOptElseStmt ? llvm::BasicBlock::Create(context.mContext) : nullptr;
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mIrBuilder.CreateCondBr(value, thenBB, elseBB?elseBB:endBB);
  context.mIrBuilder.SetInsertPoint(thenBB);
  mThenStmt->Codegen(context);
  if (!thenBB->back().isTerminator()) {
    context.mIrBuilder.CreateBr(endBB);
  }
  if (elseBB) {
    function->getBasicBlockList().push_back(elseBB);
    context.mIrBuilder.SetInsertPoint(elseBB);
    mOptElseStmt->Codegen(context);
    if (!elseBB->back().isTerminator()) {
      context.mIrBuilder.CreateBr(endBB);
    }
  }
  function->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);
  return {nullptr, false};
}
LLVMValueSignPair WhileStmt::Codegen(lcc::CodeGenContext &context) const {
  auto *condBB = llvm::BasicBlock::Create(context.mContext, "", context.mCurrentFunc);
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext);
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mIrBuilder.CreateBr(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, sign] = mExpr->Codegen(context);
  if (value->getType()->isIntegerTy()) {
    value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }
  context.mIrBuilder.CreateCondBr(value, bodyBB, endBB);

  context.mCurrentFunc->getBasicBlockList().push_back(bodyBB);
  context.mIrBuilder.SetInsertPoint(bodyBB);
  mStmt->Codegen(context);
  context.mIrBuilder.CreateBr(condBB);

  context.mCurrentFunc->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);
  return {nullptr, false};
}
LLVMValueSignPair DoWhileStmt::Codegen(lcc::CodeGenContext &context) const {
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext, "", context.mCurrentFunc);
  auto *condBB = llvm::BasicBlock::Create(context.mContext, "");
  auto *endBB = llvm::BasicBlock::Create(context.mContext);

  context.mIrBuilder.CreateBr(bodyBB);
  context.mIrBuilder.SetInsertPoint(bodyBB);
  mStmt->Codegen(context);
  context.mIrBuilder.CreateBr(condBB);

  context.mCurrentFunc->getBasicBlockList().push_back(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, sign] = mExpr->Codegen(context);
  if (value->getType()->isIntegerTy()) {
    value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }
  context.mIrBuilder.CreateCondBr(value, bodyBB, endBB);

  context.mCurrentFunc->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);
  return {nullptr, false};
}

namespace {
void GenForIR(const lcc::parser::Expr *cond,
              const lcc::parser::Expr *post,
              const lcc::parser::Stmt *body,
              lcc::CodeGenContext &context) {
  auto *function = context.mCurrentFunc;
  auto *condBB = llvm::BasicBlock::Create(context.mContext,"",function);
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext);
  auto *postBB = llvm::BasicBlock::Create(context.mContext);
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mIrBuilder.CreateBr(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, sign] = cond ? cond->Codegen(context) : LLVMValueSignPair{context.mIrBuilder.getInt32(1), true};
  if (value->getType()->isIntegerTy()) {
    value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }
  context.mIrBuilder.CreateCondBr(value, bodyBB, endBB);
  function->getBasicBlockList().push_back(bodyBB);
  context.mIrBuilder.SetInsertPoint(bodyBB);
  body->Codegen(context);
  function->getBasicBlockList().push_back(postBB);
  context.mIrBuilder.CreateBr(postBB);
  context.mIrBuilder.SetInsertPoint(postBB);
  if (post) {
    post->Codegen(context);
  }
  context.mIrBuilder.CreateBr(condBB);
  function->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);
}
}

LLVMValueSignPair ForStmt::Codegen(lcc::CodeGenContext &context) const {
  if (mInitExpr) {
    mInitExpr->Codegen(context);
  }
  GenForIR(mControlExpr.get(), mPostExpr.get(), mStmt.get(), context);
  return {nullptr, false};
}
LLVMValueSignPair
ForDeclarationStmt::Codegen(lcc::CodeGenContext &context) const {
  if (mInitDecl)
    mInitDecl->Codegen(context);
  GenForIR(mControlExpr.get(), mPostExpr.get(), mStmt.get(), context);
  return {nullptr, false};
}
LLVMValueSignPair ExprStmt::Codegen(lcc::CodeGenContext &context) const {
  return mOptExpr ? mOptExpr->Codegen(context) : LLVMValueSignPair{nullptr, false};
}
LLVMValueSignPair ReturnStmt::Codegen(lcc::CodeGenContext &context) const {
  llvm::Value *val = nullptr;
  auto[value, sign] = mOptExpr ? mOptExpr->Codegen(context) : LLVMValueSignPair{val, false};
  context.mIrBuilder.CreateRet(value);
  return {value, sign};
}
LLVMValueSignPair BreakStmt::Codegen(lcc::CodeGenContext &context) const {
  // todo
  //context.mIrBuilder.CreateBr();
  return {};
}
LLVMValueSignPair ContinueStmt::Codegen(lcc::CodeGenContext &context) const {
  // todo
  //context.mIrBuilder.CreateBr();
  return {};
}
LLVMValueSignPair Declaration::Codegen(lcc::CodeGenContext &context) const {
  LLVMTypePtr allocaType = mType->TypeGen(context);
  auto *alloca = context.mIrBuilder.CreateAlloca(allocaType, nullptr, mName);
  if (mOptValue) {
    auto [value, sign] = mOptValue->Codegen(context);
    context.mIrBuilder.CreateStore(value, alloca);
  }
  return {alloca, mType->IsSigned()};
}
LLVMValueSignPair Expr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mAssignExpr->Codegen(context);
  for (auto &assign : mOptAssignExps) {
     LLVMValueSignPair p = assign->Codegen(context);
     left = p.first;
     sign = p.second;
  }
  return {left, sign};
}
LLVMValueSignPair AssignExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mCondExpr->Codegen(context);
  llvm::Value *currentVal = nullptr;
  switch (mTokType) {
  case lexer::equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    context.mIrBuilder.CreateStore(newValue, left);
    break;
  }
  case lexer::plus_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFAdd(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::slash_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    if (currentVal->getType()->isIntegerTy()) {
      if (sign) {
        currentVal = context.mIrBuilder.CreateSDiv(currentVal, newValue);
      }else {
        currentVal = context.mIrBuilder.CreateUDiv(currentVal, newValue);
      }
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFDiv(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::star_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    if (currentVal->getType()->isIntegerTy()) {
        currentVal = context.mIrBuilder.CreateMul(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
        currentVal = context.mIrBuilder.CreateFMul(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::minus_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateSub(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFSub(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::percent_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateSRem(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::less_less_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateShl(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::greater_greater_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateAShr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::amp_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::pipe_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateOr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  case lexer::caret_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    currentVal = context.mIrBuilder.CreateLoad(newValue->getType(),left);
    currentVal = context.mIrBuilder.CreateXor(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, left);
    break;
  }
  default:
    assert(0);
  }
  return {currentVal, sign};
}
LLVMValueSignPair ConditionalExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mLogOrExpr->Codegen(context);
  if (mOptExpr && mOptCondExpr) {
    if (left->getType()->isIntegerTy()) {
      left = context.mIrBuilder.CreateICmpNE(left, context.mIrBuilder.getInt32(0));
    }else if (left->getType()->isFloatingPointTy()) {
      left = context.mIrBuilder.CreateFCmpUNE(left, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
    }
    auto *func = context.mCurrentFunc;
    auto *thenBB = llvm::BasicBlock::Create(context.mContext, "", func);
    auto *elseBB = llvm::BasicBlock::Create(context.mContext);
    auto *endBB = llvm::BasicBlock::Create(context.mContext);
    context.mIrBuilder.CreateCondBr(left, thenBB, elseBB);

    context.mIrBuilder.SetInsertPoint(thenBB);
    auto [thenV, thenSign] = mOptExpr->Codegen(context);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(elseBB);
    context.mIrBuilder.SetInsertPoint(elseBB);
    auto [elseV, elseSign] = mOptCondExpr->Codegen(context);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(endBB);
    context.mIrBuilder.SetInsertPoint(endBB);

    auto *phi = context.mIrBuilder.CreatePHI(thenV->getType(), 2);
    phi->addIncoming(thenV, thenBB);
    phi->addIncoming(elseV, elseBB);

    return {phi, thenSign | elseSign};
  }
  return {left, sign};
}
LLVMValueSignPair LogOrExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mLogAndExpr->Codegen(context);
  for (auto &expr : mOptLogAndExps) {
    if (left->getType()->isIntegerTy()) {
      left = context.mIrBuilder.CreateICmpEQ(left, context.mIrBuilder.getInt32(0));
    }else if (left->getType()->isFloatingPointTy()) {
      left = context.mIrBuilder.CreateFCmpUEQ(left, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
    }
    auto *func = context.mCurrentFunc;
    auto *thenBB = llvm::BasicBlock::Create(context.mContext, "", func);
    auto *elseBB = llvm::BasicBlock::Create(context.mContext);
    auto *endBB = llvm::BasicBlock::Create(context.mContext);
    context.mIrBuilder.CreateCondBr(left, thenBB, elseBB);

    context.mIrBuilder.SetInsertPoint(thenBB);
    auto [thenValue, thenSign] = expr->Codegen(context);
    if (thenValue->getType()->isIntegerTy()) {
      thenValue = context.mIrBuilder.CreateICmpNE(thenValue, context.mIrBuilder.getInt32(0));
    }else if (thenValue->getType()->isFloatingPointTy()) {
      thenValue = context.mIrBuilder.CreateFCmpUNE(thenValue, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
    }
    thenValue = context.mIrBuilder.CreateZExt(thenValue, context.mIrBuilder.getInt32Ty());
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(elseBB);
    context.mIrBuilder.SetInsertPoint(elseBB);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(endBB);
    context.mIrBuilder.SetInsertPoint(endBB);
    auto *phi = context.mIrBuilder.CreatePHI(context.mIrBuilder.getInt32Ty(), 2);
    phi->addIncoming(thenValue, thenBB);
    phi->addIncoming(context.mIrBuilder.getInt32(1), elseBB);

    left = phi;
    sign = true;
  }
  return {left, sign};
}
LLVMValueSignPair LogAndExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mBitOrExpr->Codegen(context);
  for (auto &expr : mOptBitOrExps) {
    if (left->getType()->isIntegerTy()) {
      left = context.mIrBuilder.CreateICmpNE(left, context.mIrBuilder.getInt32(0));
    }else if (left->getType()->isFloatingPointTy()) {
      left = context.mIrBuilder.CreateFCmpUNE(left, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
    }
    auto *func = context.mCurrentFunc;
    auto *thenBB = llvm::BasicBlock::Create(context.mContext, "", func);
    auto *elseBB = llvm::BasicBlock::Create(context.mContext);
    auto *endBB = llvm::BasicBlock::Create(context.mContext);
    context.mIrBuilder.CreateCondBr(left, thenBB, elseBB);

    context.mIrBuilder.SetInsertPoint(thenBB);
    auto [thenVal, thenSign] = expr->Codegen(context);
    if (thenVal->getType()->isIntegerTy()) {
      thenVal = context.mIrBuilder.CreateICmpNE(thenVal, context.mIrBuilder.getInt32(0));
    }else if (left->getType()->isFloatingPointTy()) {
      thenVal = context.mIrBuilder.CreateFCmpUNE(thenVal, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
    }
    thenVal = context.mIrBuilder.CreateZExt(thenVal, context.mIrBuilder.getInt32Ty());
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(elseBB);
    context.mIrBuilder.SetInsertPoint(elseBB);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(endBB);
    context.mIrBuilder.SetInsertPoint(endBB);
    auto *phi = context.mIrBuilder.CreatePHI(context.mIrBuilder.getInt32Ty(), 2);
    phi->addIncoming(thenVal, thenBB);
    phi->addIncoming(context.mIrBuilder.getInt32(0), elseBB);

    left = phi;
    sign = true;
  }
  return {left, sign};
}
LLVMValueSignPair BitOrExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mBitXorExpr->Codegen(context);
  for (auto &expr : mOptBitXorExps) {
    auto [newValue, newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateOr(left, newValue);
    sign = sign | newSign;
  }
  return {left, sign};
}
LLVMValueSignPair BitXorExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mBitAndExpr->Codegen(context);
  for (auto &expr : mOptBitAndExps) {
    auto [newValue, newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateXor(left, newValue);
    sign = sign | newSign;
  }
  return {left, sign};
}
LLVMValueSignPair BitAndExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mEqualExpr->Codegen(context);
  for (auto &expr : mOptEqualExps) {
    auto [newValue, newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateAnd(left, newValue);
    sign = sign | newSign;
  }
  return {left, sign};
}
LLVMValueSignPair EqualExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mRelationalExpr->Codegen(context);
  for (auto &expr : mOptRelationExps) {

  }
  return {};
}
LLVMValueSignPair RelationalExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ShiftExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair AdditiveExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair MultiExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair CastExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair UnaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair PostFixExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair PrimaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
LLVMValueSignPair ConstantExpr::Codegen(lcc::CodeGenContext &context) const {
  return {};
}
}
namespace lcc::codegen {

}