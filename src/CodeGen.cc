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
//    if (llvm::verifyModule(*context.mModule)) {
//      context.mModule->print(llvm::errs(), nullptr);
//      std::terminate();
//    }else {
//      context.mModule->print(llvm::outs(), nullptr);
//    }
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
  context.AddGlobal(mName, {globalVar, sign});
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
  context.AddGlobal(mName, {func, false});
  context.ClearScope();
  auto *entryBB = llvm::BasicBlock::Create(context.mContext, "entry", func);
  context.mIrBuilder.SetInsertPoint(entryBB);
  i = -1;
  for (auto &iter : func->args()) {
    ++i;
    auto *address = context.mIrBuilder.CreateAlloca(iter.getType());
    context.mIrBuilder.CreateStore(&iter, address);
    context.AddLocal(paramName[i], {address, true});
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
  return {func, false};
}
LLVMValueSignPair BlockStmt::Codegen(lcc::CodeGenContext &context) const {
  context.PushScope();
  for (auto &stmt : mStmts)
    stmt->Codegen(context);
  context.PopScope();
  return {nullptr, false};
}
LLVMValueSignPair IfStmt::Codegen(lcc::CodeGenContext &context) const {
  auto[value, sign] = mExpr->Codegen(context);

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
  return {nullptr, false};
}
LLVMValueSignPair WhileStmt::Codegen(lcc::CodeGenContext &context) const {
  auto *condBB = llvm::BasicBlock::Create(context.mContext, "", context.mCurrentFunc);
  auto *bodyBB = llvm::BasicBlock::Create(context.mContext);
  auto *endBB = llvm::BasicBlock::Create(context.mContext);
  context.mBreaks.push_back(endBB);
  context.mContinues.push_back(condBB);

  context.mIrBuilder.CreateBr(condBB);
  context.mIrBuilder.SetInsertPoint(condBB);
  auto [value, sign] = mExpr->Codegen(context);
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
  return {nullptr, false};
}
LLVMValueSignPair DoWhileStmt::Codegen(lcc::CodeGenContext &context) const {
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
  auto [value, sign] = mExpr->Codegen(context);
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
  context.mBreaks.push_back(endBB);
  context.mContinues.push_back(postBB);

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

  context.mBreaks.pop_back();
  context.mContinues.pop_back();
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
  context.PushScope();
  if (mInitDecl)
    mInitDecl->Codegen(context);
  GenForIR(mControlExpr.get(), mPostExpr.get(), mStmt.get(), context);
  context.PopScope();
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
  context.mIrBuilder.CreateBr(context.mBreaks.back());
  return {nullptr, false};
}
LLVMValueSignPair ContinueStmt::Codegen(lcc::CodeGenContext &context) const {
  context.mIrBuilder.CreateBr(context.mContinues.back());
  return {nullptr, false};
}
LLVMValueSignPair Declaration::Codegen(lcc::CodeGenContext &context) const {
  LLVMTypePtr allocaType = mType->TypeGen(context);
  llvm::IRBuilder<> tmp(&context.mCurrentFunc->getEntryBlock(), context.mCurrentFunc->getEntryBlock().end());
  auto *alloca = tmp.CreateAlloca(allocaType, nullptr, mName);
  if (mOptValue) {
    auto [value, sign] = mOptValue->Codegen(context);
    context.mIrBuilder.CreateStore(value, alloca);
  }
  context.AddLocal(mName, {alloca, true});
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
  if (mTokType == lexer::unknown) {
    return {left, sign};
  }
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = elePtr->getType()->getPointerElementType();

  switch (mTokType) {
  case lexer::equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    context.mIrBuilder.CreateStore(newValue, elePtr);
    break;
  }
  case lexer::plus_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFAdd(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::slash_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
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
  case lexer::star_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
        currentVal = context.mIrBuilder.CreateMul(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
        currentVal = context.mIrBuilder.CreateFMul(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::minus_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateSub(currentVal, newValue);
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFSub(currentVal, newValue);
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::percent_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    currentVal = context.mIrBuilder.CreateSRem(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::less_less_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateShl(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::greater_greater_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateAShr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::amp_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateAdd(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::pipe_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateOr(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
  case lexer::caret_equal: {
    auto [newValue, newSign] = mAssignExpr->Codegen(context);
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,left);
    currentVal = context.mIrBuilder.CreateXor(currentVal, newValue);
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    break;
  }
//  default:
//    assert(0);
  }
  return {context.mIrBuilder.CreateLoad(eleType, elePtr), sign};
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
  for (auto &[op, expr] : mOptRelationExps) {
    auto [right, rSign] = expr->Codegen(context);
    switch (op) {
      case lexer::TokenType::equal_equal: {
        if (left->getType()->isIntegerTy()) {
          left = context.mIrBuilder.CreateICmpEQ(left, right);
        }else if (left->getType()->isFloatingPointTy()) {
          left = context.mIrBuilder.CreateFCmpUEQ(left, right);
        }
        break;
      }
      case lexer::TokenType::exclaim_equal: {
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
  return {left, sign};
}
LLVMValueSignPair RelationalExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mShiftExpr->Codegen(context);
  for (auto&[op, expr] : mOptShiftExps) {
    auto [right, rSign] = expr->Codegen(context);
    switch (op) {
    case lexer::TokenType::less: {
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
    case lexer::TokenType::greater: {
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
    case lexer::TokenType::less_equal: {
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
    case lexer::TokenType::greater_equal: {
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
  return {left, sign};
}
LLVMValueSignPair ShiftExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mAdditiveExpr->Codegen(context);
  for (auto &[op, expr] : mOptAdditiveExps) {
    auto [right, rSign] = expr->Codegen(context);
    switch (op) {
    case lexer::TokenType::less_less: {
      left = context.mIrBuilder.CreateShl(left, right);
      break;
    }
    case lexer::TokenType::greater_greater: {
      left = context.mIrBuilder.CreateAShr(left, right);
      break;
    }
    default:
      assert(0);
    }
    sign = sign | rSign;
  }
  return {left, sign};
}
LLVMValueSignPair AdditiveExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mMultiExpr->Codegen(context);
  for (auto &[op, expr] : mOptionalMultiExps) {
    auto [right, rSign] = expr->Codegen(context);
    switch (op) {
    case lexer::TokenType::plus: {
      if (left->getType()->isIntegerTy()) {
        left = context.mIrBuilder.CreateAdd(left, right);
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFAdd(left, right);
      }
      break;
    }
    case lexer::TokenType::minus: {
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
  return {left, sign};
}
LLVMValueSignPair MultiExpr::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mCastExpr->Codegen(context);
  for (auto &[op, expr] : mOptCastExps) {
    auto [right, rSign] = expr->Codegen(context);
    switch (op) {
    case lexer::TokenType::star: {
      if (left->getType()->isIntegerTy()) {
        left = context.mIrBuilder.CreateMul(left, right);
      }else if (left->getType()->isFloatingPointTy()) {
        left = context.mIrBuilder.CreateFMul(left, right);
      }
      break;
    }
    case lexer::TokenType::slash: {
      if (left->getType()->isIntegerTy()) {
        if (sign)
          left = context.mIrBuilder.CreateSDiv(left, right);
        else
          left = context.mIrBuilder.CreateUDiv(left, right);
      }else if (left->getType()->isFloatingPointTy())
        left = context.mIrBuilder.CreateFDiv(left, right);
      break;
    }
    case lexer::TokenType::percent: {
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
  return {left, sign};
}
LLVMValueSignPair CastExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
        Overload{
          [&context](const std::unique_ptr<UnaryExpr> & unaryExpr) -> LLVMValueSignPair {
            return unaryExpr->Codegen(context);
          },
          [&context](const std::pair<std::unique_ptr<Type>, std::unique_ptr<CastExpr>>& typeCast) -> LLVMValueSignPair  {
            // todo type cast
            return typeCast.second->Codegen(context);
          }
      },
      mVariant);
}
LLVMValueSignPair UnaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](const std::unique_ptr<UnaryExprUnaryOperator> &unaryExprUnaryOperator) -> LLVMValueSignPair  {
            return unaryExprUnaryOperator->Codegen(context);
          },
          [&context](const std::unique_ptr<UnaryExprSizeOf> &unaryExprSizeOf) -> LLVMValueSignPair {
            return unaryExprSizeOf->Codegen(context);
          },
          [&context](const std::unique_ptr<UnaryExprPostFixExpr> &unaryExprPostFixExpr) -> LLVMValueSignPair {
            return unaryExprPostFixExpr->Codegen(context);
          }
      }, mVariant);
}
LLVMValueSignPair UnaryExprUnaryOperator::Codegen(lcc::CodeGenContext &context) const {
  auto [value, sign] = mUnaryExpr->Codegen(context);
  switch (mTok) {
  case lexer::amp: {
    auto loadInst = llvm::cast<llvm::LoadInst>(value);
    assert(loadInst);
    return {loadInst->getPointerOperand(), sign};
  }
  case lexer::star: {
    assert(value->getType()->isPointerTy());
    return {context.mIrBuilder.CreateLoad(value->getType()->getPointerElementType(), value), sign};
  }
  case lexer::plus: {
    return {value, sign};
  }
  case lexer::minus: {
    if (value->getType()->isIntegerTy()) {
      return {context.mIrBuilder.CreateNeg(value), true};
    }else {
      return {context.mIrBuilder.CreateFNeg(value), true};
    }
  }
  case lexer::tilde: {
    assert(value->getType()->isIntegerTy());
    return {context.mIrBuilder.CreateNot(value), sign};
  }
  case lexer::exclaim: {
    if (value->getType()->isIntegerTy()) {
      value = context.mIrBuilder.CreateICmpNE(value, context.mIrBuilder.getInt32(0));
    }else if (value->getType()->isFloatingPointTy()) {
      value = context.mIrBuilder.CreateFCmpUNE(value, llvm::ConstantFP::get(value->getType(), 0));
    }else {
      assert(0);
    }
    return {context.mIrBuilder.CreateZExt(context.mIrBuilder.CreateNot(value), context.mIrBuilder.getInt32Ty()), sign};
  }
  case lexer::plus_plus: {
    assert(llvm::isa<llvm::LoadInst>(value));
    auto* elePtr = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
    auto* eleType = elePtr->getType()->getPointerElementType();
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateAdd(currentVal, context.mIrBuilder.getInt32(1));
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFAdd(currentVal, context.mIrBuilder.getInt32(1));
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    return {currentVal, true};
  }
  case lexer::minus_minus: {
    assert(llvm::isa<llvm::LoadInst>(value));
    auto* elePtr = llvm::cast<llvm::LoadInst>(value)->getPointerOperand();
    auto* eleType = elePtr->getType()->getPointerElementType();
    llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
    if (currentVal->getType()->isIntegerTy()) {
      currentVal = context.mIrBuilder.CreateSub(currentVal, context.mIrBuilder.getInt32(1));
    }else if (currentVal->getType()->isFloatingPointTy()) {
      currentVal = context.mIrBuilder.CreateFSub(currentVal, context.mIrBuilder.getInt32(1));
    }
    context.mIrBuilder.CreateStore(currentVal, elePtr);
    return {currentVal, true};
  }
//  default:
//    assert(0);
  }
  return {nullptr, false};
}
LLVMValueSignPair UnaryExprSizeOf::Codegen(lcc::CodeGenContext &context) const {
  // todo
  return {};
}
LLVMValueSignPair UnaryExprPostFixExpr::Codegen(lcc::CodeGenContext &context) const {
  return mPostExpr->Codegen(context);
}
LLVMValueSignPair PostFixExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](
              const std::unique_ptr<PostFixExprDecrement> &postFixExprDecrement)
              -> LLVMValueSignPair {
            return postFixExprDecrement->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprIncrement> &postFixExprIncrement)
              -> LLVMValueSignPair {
            return postFixExprIncrement->Codegen(context);
          },
          [&context](const std::unique_ptr<PostFixExprArrow> &postFixExprArrow)
              -> LLVMValueSignPair {
            return postFixExprArrow->Codegen(context);
          },
          [&context](const std::unique_ptr<PostFixExprDot> &postFixExprDot)
              -> LLVMValueSignPair { return postFixExprDot->Codegen(context); },
          [&context](
              const std::unique_ptr<PostFixExprSubscript> &postFixExprSubscript)
              -> LLVMValueSignPair {
            return postFixExprSubscript->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprFuncCall> &postFixExprFuncCall)
              -> LLVMValueSignPair {
            return postFixExprFuncCall->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PostFixExprPrimary> &postFixExprPrimary)
              -> LLVMValueSignPair {
            return postFixExprPrimary->Codegen(context);
          },
      },
      mVariant);
}
LLVMValueSignPair PostFixExprDecrement::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mPostFixExpr->Codegen(context);
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = elePtr->getType()->getPointerElementType();
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
  return {retVal, true};
}
LLVMValueSignPair PostFixExprIncrement::Codegen(lcc::CodeGenContext &context) const {
  auto [left, sign] = mPostFixExpr->Codegen(context);
  assert(llvm::isa<llvm::LoadInst>(left));
  auto* elePtr = llvm::cast<llvm::LoadInst>(left)->getPointerOperand();
  auto* eleType = elePtr->getType()->getPointerElementType();
  llvm::Value* currentVal = context.mIrBuilder.CreateLoad(eleType,elePtr);
  llvm::Value* retVal = currentVal;
  if (currentVal->getType()->isIntegerTy()) {
    currentVal = context.mIrBuilder.CreateAdd(currentVal, context.mIrBuilder.getInt32(1));
  }else if (currentVal->getType()->isFloatingPointTy()) {
    currentVal = context.mIrBuilder.CreateFAdd(currentVal, context.mIrBuilder.getInt32(1));
  }
  context.mIrBuilder.CreateStore(currentVal, elePtr);
  return {retVal, true};
}
LLVMValueSignPair PostFixExprArrow::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, false };
}
LLVMValueSignPair PostFixExprDot::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, false };
}
LLVMValueSignPair
PostFixExprSubscript::Codegen(lcc::CodeGenContext &context) const {
  return { nullptr, false };
}
LLVMValueSignPair PostFixExprFuncCall::Codegen(lcc::CodeGenContext &context) const {
  auto [value, sign] = mPostFixExpr->Codegen(context);
  std::vector<llvm::Value *> arguments;
  for (auto &assignExpr : mOptParams) {
    arguments.push_back(assignExpr->Codegen(context).first);
  }
  auto *func = static_cast<llvm::Function *>(value);
  return {context.mIrBuilder.CreateCall(func->getFunctionType(), func, arguments), false};
}
LLVMValueSignPair PostFixExprPrimary::Codegen(lcc::CodeGenContext &context) const {
  return mPrimaryExpr->Codegen(context);
}
LLVMValueSignPair PrimaryExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](
              const std::unique_ptr<PrimaryExprConstant> &primaryExprConstant)
              -> LLVMValueSignPair {
            return primaryExprConstant->Codegen(context);
          },
          [&context](
              const std::unique_ptr<PrimaryExprIdentifier> &primaryExprIdentifier)
              -> LLVMValueSignPair {
            return primaryExprIdentifier->Codegen(context);
          },
          [&context](const std::unique_ptr<PrimaryExprParent> &primaryExprParent)
              -> LLVMValueSignPair {
            return primaryExprParent->Codegen(context);
          }
      },
      mVariant);
}
LLVMValueSignPair PrimaryExprIdentifier::Codegen(lcc::CodeGenContext &context) const {
  auto [value, sign] = context.FindVar(mIdentifier);
  assert(value);
  llvm::Function *func = context.mModule->getFunction(mIdentifier);
  if (func) {
    return {func, sign};
  }
  return {context.mIrBuilder.CreateLoad(value->getType()->getPointerElementType(), value), sign};
}
LLVMValueSignPair PrimaryExprConstant::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](int32_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt32(value), true};
          },
          [&context](int64_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt64(value), true};
          },
          [&context](uint32_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt32(value), false};
          },
          [&context](uint64_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt64(value), false};
          },
          [&context](float value) -> LLVMValueSignPair {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), value),
                true};
          },
          [&context](double value) -> LLVMValueSignPair {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getDoubleTy(), value),
                true};
          },
          [&context](const std::string &value) -> LLVMValueSignPair {
            return {nullptr, false};
          },
      },
      mVariant);
}
LLVMValueSignPair PrimaryExprParent::Codegen(lcc::CodeGenContext &context) const {
  return mExpr->Codegen(context);
}
LLVMValueSignPair ConstantExpr::Codegen(lcc::CodeGenContext &context) const {
  return std::visit(
      Overload{
          [&context](int32_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt32(value), true};
          },
          [&context](int64_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt64(value), true};
          },
          [&context](uint32_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt32(value), false};
          },
          [&context](uint64_t value) -> LLVMValueSignPair {
            return {context.mIrBuilder.getInt64(value), false};
          },
          [&context](float value) -> LLVMValueSignPair {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getFloatTy(), value),
                true};
          },
          [&context](double value) -> LLVMValueSignPair {
            return {
                llvm::ConstantFP::get(context.mIrBuilder.getDoubleTy(), value),
                true};
          },
          [&context](const std::string &value) -> LLVMValueSignPair {
            return {nullptr, false};
          },
      },
      mValue);
}
}
namespace lcc::codegen {

}