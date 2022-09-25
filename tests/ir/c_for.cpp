/***********************************
* File:     c_while.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/25
*
* Sign:     不管几岁，快乐万岁
***********************************/

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <vector>
#include "llvm/IR/Verifier.h"

/**
 int sum(int a, int b) {
    int cnt = b - a;
    int ret = a;
    for (int i = 0; i < cnt; ++i) {
        ret += i;
    }
    return ret;
 }
*/
using namespace llvm;

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    Type *intType = irBuilder->getInt32Ty();
    FunctionType *fType = FunctionType::get(intType, {intType, intType}, false);
    Function *sumFunc = Function::Create(fType, GlobalVariable::ExternalLinkage, "sum", *module);
    std::vector<std::string> param{"a", "b"};
    int idx = 0;
    for (Function::arg_iterator it = sumFunc->arg_begin(); it != sumFunc->arg_end(); ++it, ++idx) {
        it->setName(param[idx]);
    }
    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "entry", sumFunc);
    BasicBlock *loop_initBB = BasicBlock::Create(*llvmContext, "loop_init", sumFunc);
    BasicBlock *loop_condition = BasicBlock::Create(*llvmContext, "loop_condition", sumFunc);
    BasicBlock *loop = BasicBlock::Create(*llvmContext, "loop", sumFunc);
    BasicBlock *loop_increment = BasicBlock::Create(*llvmContext, "loop_increment", sumFunc);
    BasicBlock *loop_end = BasicBlock::Create(*llvmContext, "loop_end", sumFunc);

    irBuilder->SetInsertPoint(entryBB);
    Value *cnt = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "cnt");
    Value *ret = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "ret");
    Value *i = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "i");
    irBuilder->CreateStore(sumFunc->getArg(0), ret);
    Value *diff = irBuilder->CreateSub(sumFunc->getArg(0), sumFunc->getArg(1), "diff");
    irBuilder->CreateStore(diff, cnt);
    irBuilder->CreateBr(loop_initBB);

    irBuilder->SetInsertPoint(loop_initBB);
    irBuilder->CreateStore(irBuilder->getInt32(0), i);
    irBuilder->CreateBr(loop_condition);

    irBuilder->SetInsertPoint(loop_condition);
    Value *i_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), i, "i_val");
    Value *cnt_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), cnt, "cnt_val");
    Value *condition_val = irBuilder->CreateICmpSLT(i_val, cnt_val, "condition_val");
    irBuilder->CreateCondBr(condition_val, loop, loop_end);

    irBuilder->SetInsertPoint(loop);
    Value *ret_val = irBuilder->CreateLoad(intType, ret, "ret_val");
    Value *ret_val_t = irBuilder->CreateAdd(ret_val, irBuilder->getInt32(1), "ret_val_t");
    irBuilder->CreateStore(ret_val_t, ret_val);
    irBuilder->CreateBr(loop_increment);

    irBuilder->SetInsertPoint(loop_increment);
    i_val = irBuilder->CreateLoad(irBuilder->getInt32Ty(), i, "i_val");
    Value *i_val_t = irBuilder->CreateAdd(i_val, irBuilder->getInt32(1), "i_val_t");
    irBuilder->CreateStore(i_val_t, i);
    irBuilder->CreateBr(loop_condition);

    irBuilder->SetInsertPoint(loop_end);
    ret_val = irBuilder->CreateLoad(intType, ret, "ret_val");
    irBuilder->CreateRet(ret_val);

    verifyFunction(*sumFunc);

    module->print(errs(), nullptr);

    return 0;
}