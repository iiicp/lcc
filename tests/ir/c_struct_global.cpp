/***********************************
* File:     c_struct_global.cpp
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

using namespace llvm;

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    module->getOrInsertGlobal("double_g_val", irBuilder->getDoubleTy());
    GlobalVariable *double_g_val = module->getGlobalVariable("double_g_val");
    double_g_val->setInitializer(ConstantFP::get(irBuilder->getDoubleTy(), "1.22"));

    StructType *structType = StructType::create(*llvmContext, "struct_type");
    structType->setBody({irBuilder->getInt32Ty(),
                         irBuilder->getDoubleTy(),
                         PointerType::getInt32PtrTy(*llvmContext),
                         PointerType::getDoublePtrTy(*llvmContext)});
    module->getOrInsertGlobal("struct_g_val", structType);
    GlobalVariable *struct_g_val = module->getGlobalVariable("struct_g_val");
    struct_g_val->setInitializer(ConstantStruct::get(structType, {
        irBuilder->getInt32(1024),
        ConstantFP::get(irBuilder->getDoubleTy(), 3.14),
        ConstantPointerNull::get(PointerType::getInt32PtrTy(*llvmContext)),
        double_g_val
    }));

    module->print(errs(), nullptr);

    return 0;
}