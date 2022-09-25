/***********************************
* File:     c_basic_type_local.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/24
*
* Sign:     不管几岁，快乐万岁
***********************************/

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Verifier.h"
#include <vector>
#include <string>

using namespace llvm;

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    FunctionType *functionType = FunctionType::get(
            irBuilder->getInt32Ty(),
            {irBuilder->getInt32Ty(), irBuilder->getInt32Ty()},
            false);

    Function *func = Function::Create(functionType, llvm::GlobalValue::ExternalLinkage, "func", *module);
    std::vector<std::string> v = {"a", "b"};
    int i = 0;
    for (Function::arg_iterator it = func->arg_begin(); it != func->arg_end(); ++it, ++i) {
        it->setName(v[i]);
    }

    BasicBlock *entryBB = BasicBlock::Create(*llvmContext, "entry", func);
    irBuilder->SetInsertPoint(entryBB);
    Value *tmp = irBuilder->CreateAlloca(irBuilder->getInt32Ty(), nullptr, "tmp");
    Value *c = irBuilder->CreateAdd(func->getArg(0), func->getArg(1), "c");
    irBuilder->CreateStore(c, tmp);
    Value *ret = irBuilder->CreateLoad(irBuilder->getInt32Ty(), tmp, "ret");
    irBuilder->CreateRet(ret);

    verifyFunction(*func);

    module->print(errs(), nullptr);

    return 0;
}