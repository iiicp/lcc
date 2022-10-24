/***********************************
 * File:     c_while.cpp
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/23
 *
 * Sign:     enjoy life
 ***********************************/
#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>
using namespace llvm;
int main() {
  std::unique_ptr<LLVMContext> context = std::make_unique<LLVMContext>();
  std::unique_ptr<Module> m = std::make_unique<Module>("ir", *context);
  std::unique_ptr<IRBuilder<>> builder = std::make_unique<IRBuilder<>>(*context);

  auto *funTy = FunctionType::get(builder->getVoidTy(), false);
  auto *function = Function::Create(funTy, Function::LinkageTypes::ExternalLinkage, "f", m.get());

  auto* condBB = llvm::BasicBlock::Create(*context, "cond", function);
  builder->CreateBr(condBB);
  auto* blockBB = llvm::BasicBlock::Create(*context, "block");
  auto* endBB = llvm::BasicBlock::Create(*context, "end");

  for (auto &f : function->getBasicBlockList()) {
    f.print(llvm::errs());
  }

  builder->SetInsertPoint(condBB);
  auto* value = builder->getInt32(5);
  if (value->getType()->isIntegerTy())
  {
    if (value->getType()->getIntegerBitWidth() > 1)
    {
      value = static_cast<ConstantInt *>(builder->CreateICmpNE(
          value, builder->getIntN(value->getType()->getIntegerBitWidth(), 0)));
    }
  }
  else if (value->getType()->isFloatingPointTy())
  {
    value = static_cast<ConstantInt *>(builder->CreateFCmpUNE(
        value, llvm::ConstantFP::get(value->getType(), 0)));
  }
  builder->CreateCondBr(value, blockBB, endBB);
  function->getBasicBlockList().push_back(blockBB);
  builder->SetInsertPoint(blockBB);
  builder->CreateBr(condBB);
  function->getBasicBlockList().push_back(endBB);
  builder->SetInsertPoint(endBB);
  builder->CreateRet(builder->getInt32(0));
  if (endBB->back().isTerminator()) {
    std::cout << "isterminator" << std::endl;
  }

  m->print(errs(), nullptr);

  return 0;
}