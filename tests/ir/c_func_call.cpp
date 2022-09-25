/***********************************
* File:     c_func_call.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/25
*
* Sign:     不管几岁，快乐万岁
***********************************/

/**
 *
 * int fib(int n) {
 * }
 */

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;

int fib(int n) {
    if (n <= 1)
        return 1;
    return fib(n-1) + fib(n-2);
}

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    auto fType = FunctionType::get(irBuilder->getInt32Ty(), {irBuilder->getInt32Ty()}, false);
    auto func = Function::Create(fType, GlobalValue::ExternalLinkage, "fib", *module);
    std::vector<std::string> param{"n"};
    int idx = 0;
    for (Function::arg_iterator it = func->arg_begin(); it != func->arg_end(); ++it, ++idx) {
        it->setName(param[idx]);
    }
    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "", func);
    BasicBlock *exitBB = BasicBlock::Create(*llvmContext, "", func);
    BasicBlock *recursiveBB = BasicBlock::Create(*llvmContext, "", func);
    irBuilder->SetInsertPoint(entryBB);
    Value *condition = irBuilder->CreateICmpSLE(func->getArg(0), irBuilder->getInt32(1));
    irBuilder->CreateCondBr(condition, exitBB, recursiveBB);

    irBuilder->SetInsertPoint(exitBB);
    irBuilder->CreateRet(irBuilder->getInt32(1));

    irBuilder->SetInsertPoint(recursiveBB);
    Value *n_1 = irBuilder->CreateSub(func->getArg(0), irBuilder->getInt32(1));
    Value *n_1_ret = irBuilder->CreateCall(func, {n_1});
    Value *n_2 = irBuilder->CreateSub(func->getArg(0), irBuilder->getInt32(2));
    Value *n_2_ret = irBuilder->CreateCall(func, {n_2});
    Value *ret = irBuilder->CreateAdd(n_1_ret, n_2_ret);
    irBuilder->CreateRet(ret);

    verifyModule(*module);
    verifyFunction(*func, &llvm::errs());

    module->print(errs(), nullptr);

    return 0;
}