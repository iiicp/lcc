/***********************************
* File:     c_array_global.cpp
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

    ArrayType *arrayType = ArrayType::get(irBuilder->getFloatTy(), 2);
    module->getOrInsertGlobal("arr_g_val", arrayType);
    GlobalVariable *arr_g_val = module->getGlobalVariable("arr_g_val");
    arr_g_val->setInitializer(ConstantArray::get(arrayType,
                                                 {ConstantFP::get(irBuilder->getFloatTy(), 1.2f),
                                                  ConstantFP::get(irBuilder->getFloatTy(), 2.3f)}));


    module->getOrInsertGlobal("double_g_val", irBuilder->getDoubleTy());
    GlobalVariable *double_g_val = module->getGlobalVariable("double_g_val");
    double_g_val->setInitializer(ConstantFP::get(irBuilder->getDoubleTy(), 1.2));

    ArrayType *arrayType1 = ArrayType::get(PointerType::getDoublePtrTy(*llvmContext), 2);
    module->getOrInsertGlobal("arr_point_g_val", arrayType1);
    GlobalVariable *arr_point_g_val = module->getGlobalVariable("arr_point_g_val");
    arr_point_g_val->setInitializer(ConstantArray::get(arrayType1, {
        double_g_val,
        double_g_val
    }));

    module->print(errs(), nullptr);
    return 0;
}
