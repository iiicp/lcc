/***********************************
* File:     c_integer_type.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/23
*
* Sign:     不管几岁，快乐万岁
***********************************/

#include <iostream>
#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

int main() {
    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    module->getOrInsertGlobal("i1_gv", IntegerType::get(*llvmContext, 1));
    module->getOrInsertGlobal("i8_gv", IntegerType::get(*llvmContext, 8));
    module->getOrInsertGlobal("i16_gv", IntegerType::get(*llvmContext, 16));
    module->getOrInsertGlobal("i32_gv", IntegerType::get(*llvmContext, 32));
    module->getOrInsertGlobal("i64_gv", IntegerType::get(*llvmContext, 64));
    module->getOrInsertGlobal("i128_gv", IntegerType::get(*llvmContext, 128));

    module->getOrInsertGlobal("i1_gv_2", irBuilder->getInt1Ty());
    module->getOrInsertGlobal("i8_gv_2", irBuilder->getInt8Ty());
    module->getOrInsertGlobal("i16_gv_2", irBuilder->getInt16Ty());
    module->getOrInsertGlobal("i32_gv_2", irBuilder->getInt32Ty());
    module->getOrInsertGlobal("i64_gv_2", irBuilder->getInt64Ty());
    module->getOrInsertGlobal("i128_gv_2", irBuilder->getInt128Ty());

    module->getOrInsertGlobal("float_gv", irBuilder->getFloatTy());
    module->getOrInsertGlobal("double_gv", irBuilder->getDoubleTy());

    module->getOrInsertGlobal("i1_gv_3", irBuilder->getInt1Ty());
    GlobalVariable *i1_gv_3 = module->getGlobalVariable("i1_gv_3");
    i1_gv_3->setInitializer(irBuilder->getInt1(true));

    module->getOrInsertGlobal("i8_gv_3", irBuilder->getInt8Ty());
    GlobalVariable *i8_gv_3 = module->getGlobalVariable("i8_gv_3");
    i8_gv_3->setInitializer(irBuilder->getInt8(255)); /// 补码存储

    module->getOrInsertGlobal("i16_gv_3", irBuilder->getInt16Ty());
    GlobalVariable *i16_gv_3 = module->getGlobalVariable("i16_gv_3");
    i16_gv_3->setInitializer(irBuilder->getInt16(16161));

    module->getOrInsertGlobal("i32_gv_3", irBuilder->getInt32Ty());
    GlobalVariable *i32_gv_3 = module->getGlobalVariable("i32_gv_3");
    i32_gv_3->setInitializer(irBuilder->getInt32(32323232));

    module->getOrInsertGlobal("i64_gv_3", irBuilder->getInt64Ty());
    GlobalVariable *i64_gv_3 = module->getGlobalVariable("i64_gv_3");
    i64_gv_3->setConstant(true);
    i64_gv_3->setInitializer(irBuilder->getInt64(6464646464));

    module->getOrInsertGlobal("float_gv_1", irBuilder->getFloatTy());
    GlobalVariable *float_gv_1 = module->getGlobalVariable("float_gv_1");
    float_gv_1->setInitializer(ConstantFP::get(irBuilder->getFloatTy(), 1.1f));

    module->getOrInsertGlobal("double_gv_1", irBuilder->getDoubleTy());
    GlobalVariable *double_gv_1 = module->getGlobalVariable("double_gv_1");
    double_gv_1->setInitializer(ConstantFP::get(irBuilder->getDoubleTy(), 1.1));

    module->print(errs(), nullptr);
    return 0;
}