/***********************************
* File:     c_pointer_global.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/24
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include <iostream>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
using namespace llvm;
int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    module->getOrInsertGlobal("i32_g_var", irBuilder->getInt32Ty());
    GlobalVariable *i32_g_var = module->getGlobalVariable("i32_g_var");
    i32_g_var->setInitializer(irBuilder->getInt32(1024));

    /// void *void_ptr = &i32_g_val;
    /// only used to i8* ?
    module->getOrInsertGlobal("void_ptr", irBuilder->getInt8PtrTy());
    GlobalVariable *void_ptr = module->getGlobalVariable("void_ptr");
    void_ptr->setInitializer(i32_g_var);

    module->getOrInsertGlobal("int_ptr", PointerType::getInt32PtrTy(*llvmContext));
    GlobalVariable *int_ptr = module->getGlobalVariable("int_ptr");
    int_ptr->setInitializer(i32_g_var);

    module->print(errs(), nullptr);
    return 0;
}