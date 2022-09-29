/***********************************
* File:     c_fun_for.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/25
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include "llvm/IR/LLVMContext.h"  // 公共的数据结构
#include "llvm/IR/Module.h"       // 一个源文件的抽象 { 全局变量，函数 {基本块组成} }
#include "llvm/IR/IRBuilder.h"    // 指令生成器, 加法、减法等等、还可以获取类型
#include "llvm/IR/Verifier.h"    //校验模块、校验函数
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include <vector>
#include "iostream"

using namespace llvm;

//int sum(int n) {
//    int s = 0;
//    for (int i = 0; i < n; ++i) {
//        s += i;
//    }
//    return s;
//}

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir_function", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    Type *intType = irBuilder->getInt32Ty();
    auto fType = FunctionType::get(intType, {intType}, false);
    auto sumFuc = Function::Create(fType, GlobalValue::ExternalLinkage, "sum", *module);
    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "entry", sumFuc);
    BasicBlock *loopConditionBB = BasicBlock::Create(*llvmContext, "", sumFuc);
    BasicBlock *loopBB = BasicBlock::Create(*llvmContext, "", sumFuc);
    BasicBlock *exitBB = BasicBlock::Create(*llvmContext, "", sumFuc);
    BasicBlock *loopIncrementBB = BasicBlock::Create(*llvmContext, "", sumFuc);

    irBuilder->SetInsertPoint(entryBB);
    Value *n = irBuilder->CreateAlloca(intType, nullptr);
    Value *s = irBuilder->CreateAlloca(intType, nullptr);
    Value *i = irBuilder->CreateAlloca(intType, nullptr);
    irBuilder->CreateStore(sumFuc->getArg(0), n);
    irBuilder->CreateStore(irBuilder->getInt32(0), s);
    irBuilder->CreateStore(irBuilder->getInt32(0), i);
    irBuilder->CreateBr(loopConditionBB);

    irBuilder->SetInsertPoint(loopConditionBB);
    Value *i_val = irBuilder->CreateLoad(intType, i);
    Value *n_val = irBuilder->CreateLoad(intType, n);
    Value *cmp_val = irBuilder->CreateICmpSLT(i_val, n_val);
    irBuilder->CreateCondBr(cmp_val, loopBB, exitBB);

    irBuilder->SetInsertPoint(loopBB);
    Value *s_val = irBuilder->CreateLoad(intType, s);
    i_val = irBuilder->CreateLoad(intType, i);
    Value *s_val_t = irBuilder->CreateNSWAdd(s_val, i_val);
    irBuilder->CreateStore(s_val_t, s);
    irBuilder->CreateBr(loopIncrementBB);

    irBuilder->SetInsertPoint(loopIncrementBB);
    i_val = irBuilder->CreateLoad(intType, i);
    Value *i_val_t = irBuilder->CreateNSWAdd(i_val, irBuilder->getInt32(1));
    irBuilder->CreateStore(i_val_t, i);
    irBuilder->CreateBr(loopConditionBB);

    irBuilder->SetInsertPoint(exitBB);
    s_val = irBuilder->CreateLoad(intType, s);
    irBuilder->CreateRet(s_val);

    module->print(errs(), nullptr);

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();

    int res;
    {
        llvm::EngineBuilder builder(std::move(module));
        auto e = builder
                .setEngineKind(llvm::EngineKind::JIT)
                .setOptLevel(llvm::CodeGenOpt::Level::None)
                .create();
        std::unique_ptr<llvm::ExecutionEngine> ee(e);
        void *address = (void *) ee->getFunctionAddress("sum");
        res = ((int (*)(int)) address)(100);
    }
    llvm_shutdown();
    std::cout << res << std::endl;
    return 0;
}
