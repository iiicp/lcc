/***********************************
* File:     c_if_else.cpp
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
 int max(int a, int b) {
    int c;
    if (a < b) {
        c = b;
    }else {
        c = a;
    }
    return c;
 }
 */

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include <vector>
#include <string>

using namespace llvm;

#define PARAM_ALLOC_ON_STACK

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    Type *intType = irBuilder->getInt32Ty();
    FunctionType *fType = FunctionType::get(intType, {intType, intType}, false);
    auto *func = Function::Create(fType, GlobalValue::ExternalLinkage, "max", *module);
    std::vector<std::string> v{"a", "b"};
    int idx = 0;
    for (Function::arg_iterator it = func->arg_begin(); it != func->arg_end(); ++it, ++idx) {
        it->setName(v[idx]);
    }

    auto *entryBB = BasicBlock::Create(*llvmContext, "entry", func);
    auto *thenBB = BasicBlock::Create(*llvmContext, "then", func);
    auto *elseBB = BasicBlock::Create(*llvmContext, "else", func);
    auto *mergeBB = BasicBlock::Create(*llvmContext, "merge", func);

    irBuilder->SetInsertPoint(entryBB);
#ifdef PARAM_ALLOC_ON_STACK
    Value *a_t = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "a_t");
    Value *b_t = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "b_t");
    irBuilder->CreateStore(func->getArg(0), a_t);
    irBuilder->CreateStore(func->getArg(1), b_t);
    Value *a_t_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), a_t, "a_t_val");
    Value *b_t_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), b_t, "b_t_val");
#else
    Value *a_t_val = func->getArg(0);
    Value *b_t_val = func->getArg(1);
#endif
    Value *c_t = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "c_t");

    Value *condition = irBuilder->CreateICmpSLT(a_t_val, b_t_val, "cmp_val");
    irBuilder->CreateCondBr(condition, thenBB, elseBB);

    irBuilder->SetInsertPoint(thenBB);
    irBuilder->CreateStore(b_t_val, c_t);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(elseBB);
    irBuilder->CreateStore(a_t_val, c_t);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(mergeBB);
    Value *ret = irBuilder->CreateLoad(irBuilder->getInt32Ty(), c_t, "ret");
    irBuilder->CreateRet(ret);

    verifyFunction(*func);

    module->print(errs(), nullptr);

    return 0;
}
