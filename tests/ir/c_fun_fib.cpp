/***********************************
* File:     c_fun_fib.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/29
*
* Sign:     不管几岁，快乐万岁
***********************************/

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"
#include <iostream>

using namespace llvm;

int fib(int n) {
    if (n <= 1)
        return 1;
    return fib(n-1) + fib(n-2);
}

int main()
{
    std::unique_ptr<LLVMContext> llvmContext = std::make_unique<LLVMContext>();
    std::unique_ptr<Module> module = std::make_unique<Module>("fib_mod", *llvmContext);
    std::unique_ptr<IRBuilder<>> irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    Type *intType = irBuilder->getInt32Ty();
    FunctionType *fType = FunctionType::get(intType, {intType}, false);
    Function *fib = Function::Create(fType, GlobalValue::ExternalLinkage, "fib", *module);
    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "entry", fib);
    BasicBlock *trueBB = BasicBlock::Create(*llvmContext, "true", fib);
    BasicBlock *elseBB = BasicBlock::Create(*llvmContext, "else", fib);
    BasicBlock *mergeBB = BasicBlock::Create(*llvmContext, "merge", fib);
    irBuilder->SetInsertPoint(entryBB);

    Value *ll0 = irBuilder->CreateAlloca(intType);
    Value *ll1 = irBuilder->CreateAlloca(intType);

    irBuilder->CreateStore(fib->getArg(0), ll0);

    Value *n = irBuilder->CreateLoad(intType, ll0);
    if (llvm::isa<LoadInst>(n)) {
      std::cout << "1" << std::endl;
      auto *t1 = llvm::cast<llvm::LoadInst>(n)->getPointerOperand();
      auto *t2 = llvm::cast<llvm::LoadInst>(n)->getPointerOperandType();
      assert(t2->getPointerElementType()->isIntegerTy());
      std::cout << "3" << std::endl;
    }else {
      std::cout << "2" << std::endl;
    }
    Value *value = irBuilder->CreateICmpSLE(n, irBuilder->getInt32(2));
    irBuilder->CreateCondBr(value, trueBB, elseBB);

    irBuilder->SetInsertPoint(trueBB);
    n = irBuilder->CreateLoad(intType, ll0);
    irBuilder->CreateStore(n, ll1);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(elseBB);
    n = irBuilder->CreateLoad(intType, ll0);
    Value *v0 = irBuilder->CreateSub(n, irBuilder->getInt32(1), "", false, true);
    Value *f1 = irBuilder->CreateCall(fib, {v0});
    n = irBuilder->CreateLoad(intType, ll0);
    Value *v1 = irBuilder->CreateSub(n, irBuilder->getInt32(2), "", false, true);
    Value *f2 = irBuilder->CreateCall(fib, {v1});
    Value *ff3 = irBuilder->CreateAdd(f1, f2, "", false, true);
    irBuilder->CreateStore(ff3, ll1);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(mergeBB);
    Value *r = irBuilder->CreateLoad(intType, ll1);
    irBuilder->CreateRet(r);

    module->print(errs(), nullptr);

    /// 此处要初始化jit环境
    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();

    int ret = -1;
    {
        EngineBuilder builder(std::move(module));
        auto b = builder.setEngineKind(EngineKind::JIT).setOptLevel(llvm::CodeGenOpt::Level::None).create();
        std::unique_ptr<ExecutionEngine> ee(b);
        // 获取函数地址
        void *address = (void *)ee->getFunctionAddress("fib");
        ret = ((int(*)(int))address)(5);
    }
    llvm_shutdown();

    std::cout << ret << std::endl;

    return 0;
}