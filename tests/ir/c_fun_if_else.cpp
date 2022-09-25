/***********************************
* File:     c_fun_show.cpp
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
#include <vector>

using namespace llvm;

//int max(int a, int b) {
//    int c;
//    if (a < b) {
//        c = b;
//    }else {
//        c = a;
//    }
//    return c;
//}

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir_function", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    Type *intType = irBuilder->getInt32Ty();
    auto fType = FunctionType::get(intType, {intType, intType}, false);
    Function *func = Function::Create(fType, GlobalValue::ExternalLinkage, "max", *module);

    std::vector<std::string> param{"a", "b"};
    int idx = 0;
    for (Function::arg_iterator it = func->arg_begin(); it != func->arg_end(); ++it, ++idx) {
        it->setName(param[idx]);
    }

    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "entry", func);
    BasicBlock *thenBB = BasicBlock::Create(*llvmContext, "", func);
    BasicBlock *elseBB = BasicBlock::Create(*llvmContext, "", func);
    BasicBlock *mergeBB = BasicBlock::Create(*llvmContext, "", func);
    irBuilder->SetInsertPoint(entryBB);
    Value *c = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "c");
    Value *cmp_ret = irBuilder->CreateICmpSLT(func->getArg(0), func->getArg(1));
    irBuilder->CreateCondBr(cmp_ret, thenBB, elseBB);

    irBuilder->SetInsertPoint(thenBB);
    irBuilder->CreateStore(func->getArg(1), c);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(elseBB);
    irBuilder->CreateStore(func->getArg(0), c);
    irBuilder->CreateBr(mergeBB);

    irBuilder->SetInsertPoint(mergeBB);
    Value *c_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), c);
    irBuilder->CreateRet(c_val);

    verifyFunction(*func);
    module->print(errs(), nullptr);

    return 0;
}
