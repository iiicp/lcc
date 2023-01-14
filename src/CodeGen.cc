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
namespace lcc{
namespace {
template<typename ... Ts>
struct Overload : Ts ... {
  using Ts::operator() ...;
};
template<class... Ts> Overload(Ts...) -> Overload<Ts...>;
}
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
  if (mTypes[cur] == tok::kw_const) {
    cur++;
  }
  assert(cur < size);
  if (mTypes[cur] == tok::kw_signed) {
    mSign = true;
    cur++;
  }else if (mTypes[cur] == tok::kw_unsigned) {
    mSign = false;
    cur++;
  }
  if (cur == size) {
    return context.mIrBuilder.getInt32Ty();
  }
  switch (mTypes[cur]) {
  case tok::kw_void:
    return context.mIrBuilder.getVoidTy();
  case tok::kw_char:
    return context.mIrBuilder.getInt8Ty();
  case tok::kw_short:
    return context.mIrBuilder.getInt16Ty();
  case tok::kw_int:
    return context.mIrBuilder.getInt32Ty();
  case tok::kw_long: {
    if (cur < size && mTypes[cur+1] == tok::kw_long) {
      ++cur;
      return context.mIrBuilder.getInt64Ty();
    } else {
      return context.mIrBuilder.getInt64Ty();
    }
  }
  case tok::kw_float:
    return context.mIrBuilder.getFloatTy();
  case tok::kw_double:
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
NodeRetValue Program::Codegen(CodeGenContext &context) const {
    context.mModule = std::make_unique<llvm::Module>("main", context.mContext);
    for (auto &ext : mExternalDecl) {
      ext->Codegen(context);
    }
//    if (llvm::verifyModule(*context.mModule)) {
//      context.mModule->print(llvm::errs(), nullptr);
//      std::terminate();
//    }else {
//      context.mModule->print(llvm::outs(), nullptr);
//    }
    return {nullptr, nullptr, false};
}
NodeRetValue GlobalDecl::Codegen(lcc::CodeGenContext &context) const {
  LLVMTypePtr type = mType->TypeGen(context);
  llvm::Value *value = nullptr;
  if (!mOptValue) {
    if (type->isIntegerTy()) {
      value = llvm::ConstantInt::get(type, 0);
    }else if (type->isFloatingPointTy()) {
      value = llvm::ConstantFP::get(type, 0);
    }
  }
  auto [constant, baseTy, sign] = mOptValue ? mOptValue->Codegen(context) : NodeRetValue{value, type,mType->IsSigned()};
  context.mModule->getOrInsertGlobal(mName, type);
  auto *globalVar = context.mModule->getGlobalVariable(mName);
  globalVar->setInitializer(llvm::cast<llvm::Constant>(constant));
  context.AddGlobal(mName, {globalVar, baseTy, sign});
  return {constant, baseTy, sign};
}
NodeRetValue Function::Codegen(lcc::CodeGenContext &context) const {
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
    return {func, func->getReturnType(), false};

  context.mCurrentFunc = func;
  context.AddGlobal(mName, {func, func->getReturnType(), false});
  context.ClearScope();
  auto *entryBB = llvm::BasicBlock::Create(context.mContext, "entry", func);
  context.mIrBuilder.SetInsertPoint(entryBB);
  i = -1;
  for (auto &iter : func->args()) {
    ++i;
    auto *address = context.mIrBuilder.CreateAlloca(iter.getType());
    context.mIrBuilder.CreateStore(&iter, address);
    context.AddLocal(paramName[i], {address, iter.getType(), true});
  }
  mOptBlockStmt->Codegen(context);

  auto &block = func->back();
  if (block.empty() || !block.back().isTerminator()) {
    if (retType->isVoidTy()) {
      context.mIrBuilder.CreateRetVoid();
    }else {
      func->print(llvm::errs());
      assert(0);
    }
  }
  if (llvm::verifyFunction(*func, &llvm::errs())) {
    func->print(llvm::errs());
    std::terminate();
  }
  return {func, func->getReturnType(), false};
}
NodeRetValue BlockStmt::Codegen(lcc::CodeGenContext &context) const {
  context.PushScope();
  for (auto &stmt : mStmts)
    stmt->Codegen(context);
  context.PopScope();
  return {nullptr, nullptr, false};
}
NodeRetValue IfStmt::Codegen(lcc::CodeGenContext &context) const {
  auto[value, baseTy, sign] = mExpr->Codegen(context);

  if (value->getType()->isIntegerTy()) {
      value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
      value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  } else {
    assert(0);
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
  if (endBB->hasNPredecessorsOrMore(1)) {
    function->getBasicBlockList().push_back(endBB);
    context.mIrBuilder.SetInsertPoint(endBB);
  }
  return {nullptr, nullptr, false};
}
NodeRetValue WhileStmt::Codegen(lcc::CodeGenContext &context) const {
  auto *condBB = llvm::BasicBlock::Create(context.mContext, "", context.mCurrentFunc);
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext);
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mBreaks.push_back(endBB);
  context.mContinues.push_back(condBB);

  context.mIrBuilder.CreateBr(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, _, sign] = mExpr->Codegen(context);
  if (value->getType()->isIntegerTy()) {
    value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }else {
    assert(0);
  }
  context.mIrBuilder.CreateCondBr(value, bodyBB, endBB);

  context.mCurrentFunc->getBasicBlockList().push_back(bodyBB);
  context.mIrBuilder.SetInsertPoint(bodyBB);
  mStmt->Codegen(context);
  context.mIrBuilder.CreateBr(condBB);
  context.mCurrentFunc->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);

  context.mBreaks.pop_back();
  context.mContinues.pop_back();
  return {nullptr, nullptr, false};
}
NodeRetValue DoWhileStmt::Codegen(lcc::CodeGenContext &context) const {
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext, "", context.mCurrentFunc);
  auto *condBB = llvm::BasicBlock::Create(context.mContext, "");
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mBreaks.push_back(endBB);
  context.mContinues.push_back(condBB);

  context.mIrBuilder.CreateBr(bodyBB);
  context.mIrBuilder.SetInsertPoint(bodyBB);
  mStmt->Codegen(context);
  context.mIrBuilder.CreateBr(condBB);

  context.mCurrentFunc->getBasicBlockList().push_back(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, _, sign] = mExpr->Codegen(context);
  if (value->getType()->isIntegerTy()) {
    value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
  }else if (value->getType()->isFloatingPointTy()) {
    value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), 0));
  }else {
    assert(0);
  }
  context.mIrBuilder.CreateCondBr(value, bodyBB, endBB);

  context.mCurrentFunc->getBasicBlockList().push_back(endBB);
  context.mIrBuilder.SetInsertPoint(endBB);

  context.mBreaks.pop_back();
  context.mContinues.pop_back();
  return {nullptr, nullptr, false};
}

namespace {
void GenForIR(const lcc::Expr *cond,
              const lcc::Expr *post,
              const lcc::Stmt *body,
              lcc::CodeGenContext &context) {
  auto *function = context.mCurrentFunc;
  auto *condBB = llvm::BasicBlock::Create(context.mContext,"",function);
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext);
  auto *postBB = llvm::BasicBlock::Create(context.mContext);
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mBreaks.push_back(endBB);
  context.mContinues.push_back(postBB);

  context.mIrBuilder.CreateBr(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, baseTy, sign] = cond ? cond->Codegen(context) : NodeRetValue{context.mIrBuilder.getInt32(1), context.mIrBuilder.getInt32Ty(),true};
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

  context.mBreaks.pop_back();
  context.mContinues.pop_back();
}
}

NodeRetValue ForStmt::Codegen(lcc::CodeGenContext &context) const {
  if (mInitExpr) {
    mInitExpr->Codegen(context);
  }
  GenForIR(mControlExpr.get(), mPostExpr.get(), mStmt.get(), context);
  return {nullptr, nullptr, false};
}
NodeRetValue ForDeclarationStmt::Codegen(lcc::CodeGenContext &context) const {
  context.PushScope();
  if (mInitDecl)
    mInitDecl->Codegen(context);
  GenForIR(mControlExpr.get(), mPostExpr.get(), mStmt.get(), context);
  context.PopScope();
  return {nullptr, nullptr, false};
}
NodeRetValue ExprStmt::Codegen(lcc::CodeGenContext &context) const {
  return mOptExpr ? mOptExpr->Codegen(context) : NodeRetValue{nullptr, nullptr, false};
}
NodeRetValue ReturnStmt::Codegen(lcc::CodeGenContext &context) const {
  llvm::Value *val = nullptr;
  auto[value, baseTy, sign] = mOptExpr ? mOptExpr->Codegen(context) : NodeRetValue{val, nullptr, false};
  context.mIrBuilder.CreateRet(value);
  return {value, baseTy, sign};
}
NodeRetValue BreakStmt::Codegen(lcc::CodeGenContext &context) const {
  context.mIrBuilder.CreateBr(context.mBreaks.back());
  return {nullptr, nullptr, false};
}
NodeRetValue ContinueStmt::Codegen(lcc::CodeGenContext &context) const {
  context.mIrBuilder.CreateBr(context.mContinues.back());
  return {nullptr, nullptr, false};
}
NodeRetValue Declaration::Codegen(lcc::CodeGenContext &context) const {
  LLVMTypePtr allocaType = mType->TypeGen(context);
  llvm::IRBuilder<> tmp(&context.mCurrentFunc->getEntryBlock(), context.mCurrentFunc->getEntryBlock().end());
  auto *alloca = tmp.CreateAlloca(allocaType, nullptr, mName);
  if (mOptValue) {
    auto [value, _, sign] = mOptValue->Codegen(context);
    context.mIrBuilder.CreateStore(value, alloca);
  }
  context.AddLocal(mName, {alloca, allocaType, true});
  return {alloca, allocaType, mType->IsSigned()};
}
NodeRetValue Expr::Codegen(lcc::CodeGenContext &context) const {
  NodeRetValue ret = mAssignExpr->Codegen(context);
  for (auto &assign : mOptAssignExps) {
    NodeRetValue p = assign->Codegen(context);
     ret = p;
  }
  return ret;
}
NodeRetValue AssignExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mCondExpr->Codegen(context);
  if (mTokType == tok::unknown) {
    return {left, left->getType(), sign};
  }
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = baseTy;

  switch (mTokType) {
  case tok::equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    context.mIrBuilder.CreateStore(newValue, elePtr);
    break;
  }
  case tok::plus_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFAdd(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::slash_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      if (sign) {
        currentVal = context.mIrBuilder.CreateSDiv(currentVal, newValue);
      }else {
        currentVal = context.mIrBuilder.CreateUDiv(currentVal, newValue);
      }
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFDiv(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::star_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
        currentVal = context.mIrBuilder.CreateMul(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
        currentVal = context.mIrBuilder.CreateFMul(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::minus_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateSub(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFSub(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::percent_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    currentVal = context.mIrBuilder.CreateSRem(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::less_less_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateShl(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::greater_greater_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateAShr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::amp_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::pipe_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateOr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case tok::caret_equal: {
    auto [newValue, _, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateXor(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
//  default:
//    assert(0);
  }
  return {context.mIrBuilder.CreateLoad(eleType, elePtr), eleType, sign};
}
NodeRetValue ConditionalExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mLogOrExpr->Codegen(context);
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
    auto [thenV, _, thenSign] = mOptExpr->Codegen(context);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(elseBB);
    context.mIrBuilder.SetInsertPoint(elseBB);
    auto [elseV, _1, elseSign] = mOptCondExpr->Codegen(context);
    context.mIrBuilder.CreateBr(endBB);

    func->getBasicBlockList().push_back(endBB);
    context.mIrBuilder.SetInsertPoint(endBB);

    auto *phi = context.mIrBuilder.CreatePHI(thenV->getType(), 2);
    phi->addIncoming(thenV, thenBB);
    phi->addIncoming(elseV, elseBB);

    return {phi, thenV->getType(), thenSign | elseSign};
  }
  return {left, left->getType(), sign};
}
NodeRetValue LogOrExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mLogAndExpr->Codegen(context);
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
    auto [thenValue, _, thenSign] = expr->Codegen(context);
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
  return {left, left->getType(), sign};
}
NodeRetValue LogAndExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mBitOrExpr->Codegen(context);
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
    auto [thenVal, _, thenSign] = expr->Codegen(context);
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
  return {left, left->getType(), sign};
}
NodeRetValue BitOrExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mBitXorExpr->Codegen(context);
  for (auto &expr : mOptBitXorExps) {
    auto [newValue, _,newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateOr(left, newValue);
    sign = sign | newSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue BitXorExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mBitAndExpr->Codegen(context);
  for (auto &expr : mOptBitAndExps) {
    auto [newValue, _, newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateXor(left, newValue);
    sign = sign | newSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue BitAndExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mEqualExpr->Codegen(context);
  for (auto &expr : mOptEqualExps) {
    auto [newValue, _, newSign] = expr->Codegen(context);
    left = context.mIrBuilder.CreateAnd(left, newValue);
    sign = sign | newSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue EqualExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mRelationalExpr->Codegen(context);
  for (auto &[op, expr] : mOptRelationExps) {
    auto [right, _, rSign] = expr->Codegen(context);
    switch (op) {
      case tok::equal_equal: {
        if (left->getType()->isIntegerTy()) {
          left = context.mIrBuilder.CreateICmpEQ(left, right);
        }else if (left->getType()->isFloatingPointTy()) {
          left = context.mIrBuilder.CreateFCmpUEQ(left, right);
        }
        break;
      }
      case tok::exclaim_equal: {
        if (left->getType()->isIntegerTy()) {
          left = context.mIrBuilder.CreateICmpNE(left, right);
        }else if (left->getType()->isFloatingPointTy()) {
          left = context.mIrBuilder.CreateFCmpUNE(left, right);
        }
        break;
      }
      default:
        assert(0);
    }
    sign = true;
    left = context.mIrBuilder.CreateZExt(left, context.mIrBuilder.getInt32Ty());
  }
  return {left, left->getType(), sign};
}
NodeRetValue RelationalExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mShiftExpr->Codegen(context);
  for (auto&[op, expr] : mOptShiftExps) {
    auto [right, _, rSign] = expr->Codegen(context);
    switch (op) {
    case tok::less: {
      if (left->getType()->isIntegerTy()) {
        if (sign) {
          left = context.mIrBuilder.CreateICmpSLT(left, right);
        }else {
          left = context.mIrBuilder.CreateICmpULT(left, right);
        }
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFCmpULT(left, right);
      }
      break;
    }
    case tok::greater: {
      if (left->getType()->isIntegerTy()) {
        if (sign) {
          left = context.mIrBuilder.CreateICmpSGT(left, right);
        }else {
          left = context.mIrBuilder.CreateICmpUGT(left, right);
        }
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFCmpUGT(left, right);
      }
      break;
    }
    case tok::less_equal: {
      if (left->getType()->isIntegerTy()) {
        if (sign) {
          left = context.mIrBuilder.CreateICmpSLE(left, right);
        }else {
          left = context.mIrBuilder.CreateICmpULE(left, right);
        }
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFCmpULE(left, right);
      }
      break;
    }
    case tok::greater_equal: {
      if (left->getType()->isIntegerTy()) {
        if (sign) {
          left = context.mIrBuilder.CreateICmpSGE(left, right);
        }else {
          left = context.mIrBuilder.CreateICmpUGE(left, right);
        }
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFCmpUGE(left, right);
      }
      break;
    }
    default:
      assert(0);
    }
    sign = true;
    left = context.mIrBuilder.CreateZExt(left, context.mIrBuilder.getInt32Ty());
  }
  return {left, left->getType(), sign};
}
NodeRetValue ShiftExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mAdditiveExpr->Codegen(context);
  for (auto &[op, expr] : mOptAdditiveExps) {
    auto [right, _, rSign] = expr->Codegen(context);
    switch (op) {
    case tok::less_less: {
      left = context.mIrBuilder.CreateShl(left, right);
      break;
    }
    case tok::greater_greater: {
      left = context.mIrBuilder.CreateAShr(left, right);
      break;
    }
    default:
      assert(0);
    }
    sign = sign | rSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue AdditiveExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mMultiExpr->Codegen(context);
  for (auto &[op, expr] : mOptionalMultiExps) {
    auto [right, _, rSign] = expr->Codegen(context);
    switch (op) {
    case tok::plus: {
      if (left->getType()->isIntegerTy()) {
        left = context.mIrBuilder.CreateAdd(left, right);
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFAdd(left, right);
      }
      break;
    }
    case tok::minus: {
      if (left->getType()->isIntegerTy())
        left = context.mIrBuilder.CreateSub(left, right);
      else if (left->getType()->isFloatingPointTy())
        left = context.mIrBuilder.CreateFSub(left, right);
      break;
    }
    default:
      assert(0);
    }
    sign = sign | rSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue MultiExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mCastExpr->Codegen(context);
  for (auto &[op, expr] : mOptCastExps) {
    auto [right, _, rSign] = expr->Codegen(context);
    switch (op) {
    case tok::star: {
      if (left->getType()->isIntegerTy()) {
        left = context.mIrBuilder.CreateMul(left, right);
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFMul(left, right);
      }
      break;
    }
    case tok::slash: {
      if (left->getType()->isIntegerTy()) {
        if (sign)
          left = context.mIrBuilder.CreateSDiv(left, right);
        else
          left = context.mIrBuilder.CreateUDiv(left, right);
      }else if (left->getType()->isFloatingPointTy())
        left = context.mIrBuilder.CreateFDiv(left, right);
      break;
    }
    case tok::percent: {
      if (left->getType()->isIntegerTy()) {
        if (sign)
          left = context.mIrBuilder.CreateSRem(left, right);
        else
          left = context.mIrBuilder.CreateURem(left, right);
      }else if (left->getType()->isFloatingPointTy())
        left = context.mIrBuilder.CreateFRem(left, right);
      break;
    }
    default:
      assert(0);
    }
    sign = sign | rSign;
  }
  return {left, left->getType(), sign};
}
NodeRetValue CastExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
        Overload{
          [&context](const std::unique_ptr<UnaryExpr> & unaryExpr) -> NodeRetValue {
            return unaryExpr->Codegen(context);
          },
          [&context](const std::pair<std::unique_ptr<Type>, std::unique_ptr<CastExpr>>& typeCast) -> NodeRetValue {
            // todo type cast
            return typeCast.second->Codegen(context);
          }
      },
      mVariant);
}
NodeRetValue UnaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](const std::unique_ptr<UnaryExprUnaryOperator> &unaryExprUnaryOperator) -> NodeRetValue {
            return unaryExprUnaryOperator->Codegen(context);
          },
          [&context](const std::unique_ptr<UnaryExprSizeOf> &unaryExprSizeOf) -> NodeRetValue {
            return unaryExprSizeOf->Codegen(context);
          },
          [&context](const std::unique_ptr<UnaryExprPostFixExpr> &unaryExprPostFixExpr) -> NodeRetValue {
            return unaryExprPostFixExpr->Codegen(context);
          }
      }, mVariant);
}
NodeRetValue
UnaryExprUnaryOperator::Codegen(lcc::CodeGenContext &context) const {
  auto [value, baseTy, sign] = mUnaryExpr->Codegen(context);
  switch (mTok) {
  case tok::amp: {
    auto loadInst = llvm::cast<llvm::LoadInst>(value);
    assert(loadInst);
    return {loadInst->getPointerOperand(), baseTy, sign};
  }
  case tok::star: {
    assert(value->getType()->isPointerTy());
    return {context.mIrBuilder.CreateLoad(baseTy, value), baseTy, sign};
  }
  case tok::plus: {
    return {value, value->getType(), sign};
  }
  case tok::minus: {
    if (value->getType()->isIntegerTy()) {
      return {context.mIrBuilder.CreateNeg(value), value->getType(), true};
    }else {
      return {context.mIrBuilder.CreateFNeg(value), value->getType(), true};
    }
  }
  case tok::tilde: {
    assert(value->getType()->isIntegerTy());
    return {context.mIrBuilder.CreateNot(value),value->getType(), sign};
  }
  case tok::exclaim: {
    if (value->getType()->isIntegerTy()) {
      value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
    }else if (value->getType()->isFloatingPointTy()) {
      value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
    }else {
      assert(0);
    }
    return {context.mIrBuilder.CreateZExt(context.mIrBuilder.CreateNot(value), context.mIrBuilder.getInt32Ty()), context.mIrBuilder.getInt32Ty(), sign};
  }
  case tok::plus_plus: {
    assert(llvm::isa<llvm::LoadInst>(value));
    auto* elePtr = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
    auto* eleType = baseTy;
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateAdd(currentVal, context.mIrBuilder.getInt32(1));
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFAdd(currentVal, context.mIrBuilder.getInt32(1));
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    return {currentVal, currentVal->getType(), true};
  }
  case tok::minus_minus: {
    assert(llvm::isa<llvm::LoadInst>(value));
    auto* elePtr = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
    auto* eleType = baseTy;
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateSub(currentVal, context.mIrBuilder.getInt32(1));
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFSub(currentVal, context.mIrBuilder.getInt32(1));
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    return {currentVal, currentVal->getType(), true};
  }
//  default:
//    assert(0);
  }
  return {nullptr, nullptr, false};
}
NodeRetValue UnaryExprSizeOf::Codegen(lcc::CodeGenContext &context) const {
  // todo
  return {};
}
NodeRetValue UnaryExprPostFixExpr::Codegen(lcc::CodeGenContext &context) const {
  return mPostExpr->Codegen(context);
}
NodeRetValue PostFixExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](
              const std::unique_ptr<PostFixExprDecrement> &postFixExprDecrement)
              -> NodeRetValue {
            return postFixExprDecrement->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprIncrement> &postFixExprIncrement)
              -> NodeRetValue {
            return postFixExprIncrement->Codegen(context);
          },
          [&context](const std::unique_ptr<PostFixExprArrow> &postFixExprArrow)
              -> NodeRetValue {
            return postFixExprArrow->Codegen(context);
          },
          [&context](const std::unique_ptr<PostFixExprDot> &postFixExprDot)
              -> NodeRetValue { return postFixExprDot->Codegen(context); },
          [&context](
              const std::unique_ptr<PostFixExprSubscript> &postFixExprSubscript)
              -> NodeRetValue {
            return postFixExprSubscript->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprFuncCall> &postFixExprFuncCall)
              -> NodeRetValue {
            return postFixExprFuncCall->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprPrimary> &postFixExprPrimary)
              -> NodeRetValue {
            return postFixExprPrimary->Codegen(context);
          },
      },
      mVariant);
}
NodeRetValue PostFixExprDecrement::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mPostFixExpr->Codegen(context);
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = baseTy;
  llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
  llvm::Value* retVal = currentVal;
  if (currentVal->getType()->isIntegerTy()) {
    currentVal = context.mIrBuilder.CreateNSWSub(currentVal, context.mIrBuilder.getInt32(1));
  }else if (currentVal->getType()->isFloatingPointTy()) {
    currentVal = context.mIrBuilder.CreateFSub(currentVal, context.mIrBuilder.getInt32(1));
  }else {
    assert(0);
  }
  context.mIrBuilder.CreateStore(currentVal, elePtr);
  return {retVal, currentVal->getType(), true};
}
NodeRetValue PostFixExprIncrement::Codegen(lcc::CodeGenContext &context) const {
  auto [left, baseTy, sign] = mPostFixExpr->Codegen(context);
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = baseTy;
  llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
  llvm::Value* retVal = currentVal;
  if (currentVal->getType()->isIntegerTy()) {
    currentVal = context.mIrBuilder.CreateAdd(currentVal, context.mIrBuilder.getInt32(1));
  }else if (currentVal->getType()->isFloatingPointTy()) {
    currentVal = context.mIrBuilder.CreateFAdd(currentVal, context.mIrBuilder.getInt32(1));
  }
  context.mIrBuilder.CreateStore(currentVal, elePtr);
  return {retVal, currentVal->getType(), true};
}
NodeRetValue PostFixExprArrow::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, nullptr, false };
}
NodeRetValue PostFixExprDot::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, nullptr, false };
}
NodeRetValue PostFixExprSubscript::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, nullptr, false };
}
NodeRetValue PostFixExprFuncCall::Codegen(lcc::CodeGenContext &context) const {
  auto [value, ty, sign] = mPostFixExpr->Codegen(context);
  std::vector<llvm::Value *> arguments;
  for (auto &assignExpr : mOptParams) {
    arguments.push_back(std::get<0>(assignExpr->Codegen(context)));
  }
  auto *func = static_cast<llvm::Function *>(value);
  return {context.mIrBuilder.CreateCall(func->getFunctionType(), func, arguments), func->getReturnType(), false};
}
NodeRetValue PostFixExprPrimary::Codegen(lcc::CodeGenContext &context) const {
  return mPrimaryExpr->Codegen(context);
}
NodeRetValue PrimaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](
              const std::unique_ptr<PrimaryExprConstant> &primaryExprConstant)
              -> NodeRetValue {
            return primaryExprConstant->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PrimaryExprIdentifier> &primaryExprIdentifier)
              -> NodeRetValue {
            return primaryExprIdentifier->Codegen(context);
          },
          [&context](const std::unique_ptr<PrimaryExprParent> &primaryExprParent)
              -> NodeRetValue {
            return primaryExprParent->Codegen(context);
          }
      },
      mVariant);
}
NodeRetValue
PrimaryExprIdentifier::Codegen(lcc::CodeGenContext &context) const {
  auto [value, baseTy, sign] = context.FindVar(mIdentifier);
  assert(value);
  llvm::Function *func = context.mModule->getFunction(mIdentifier);
  if (func) {
    return {func, func->getReturnType(), sign};
  }
  return {context.mIrBuilder.CreateLoad(baseTy, value), baseTy, sign};
}
NodeRetValue PrimaryExprConstant::Codegen(lcc::CodeGenContext &context) const {
  // llvm::APSInt, llvm::APFloat, std::string
  return std::visit(
      Overload{
          [&context](llvm::APSInt value) -> NodeRetValue {
            return {llvm::ConstantInt::get(context.mIrBuilder.getInt32Ty(), value), context.mIrBuilder.getInt32Ty(), true};
          },
          [&context](llvm::APFloat value) -> NodeRetValue {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), value), context.mIrBuilder.getFloatTy(),
                true};
          },
          [&context](const std::string &value) -> NodeRetValue {
            return {nullptr, nullptr, false};
          },
      },
      mVariant);
}
NodeRetValue PrimaryExprParent::Codegen(lcc::CodeGenContext &context) const {
  return mExpr->Codegen(context);
}
NodeRetValue ConstantExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](int32_t value) -> NodeRetValue {
            return {context.mIrBuilder.getInt32(value), context.mIrBuilder.getInt32Ty(), true};
          },
          [&context](int64_t value) -> NodeRetValue {
            return {context.mIrBuilder.getInt64(value), context.mIrBuilder.getInt64Ty(), true};
          },
          [&context](uint32_t value) -> NodeRetValue {
            return {context.mIrBuilder.getInt32(value), context.mIrBuilder.getInt32Ty(), false};
          },
          [&context](uint64_t value) -> NodeRetValue {
            return {context.mIrBuilder.getInt64(value), context.mIrBuilder.getInt64Ty(), false};
          },
          [&context](float value) -> NodeRetValue {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), value), context.mIrBuilder.getFloatTy(),
                true};
          },
          [&context](double value) -> NodeRetValue {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getDoubleTy(), value), context.mIrBuilder.getDoubleTy(),
                true};
          },
          [&context](const std::string &value) -> NodeRetValue {
            return {nullptr, nullptr, false};
          },
      },
      mValue);
}
}
namespace lcc::codegen {

}