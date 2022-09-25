/***********************************
* File:     c_global_show.cpp
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

using namespace llvm;

int main() {

    auto llvmContext = std::make_unique<LLVMContext>();
    auto module = std::make_unique<Module>("ir_global", *llvmContext);
    auto irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    // int a = 10;
    // const int a = 10;
    module->getOrInsertGlobal("a", irBuilder->getInt32Ty());
    GlobalVariable *a = module->getNamedGlobal("a");
    a->setConstant(true);
    a->setInitializer(irBuilder->getInt32(10));


    // int *p = &a;
    GlobalVariable *p =static_cast<GlobalVariable *>(module->getOrInsertGlobal("p", PointerType::getInt32PtrTy(*llvmContext)));
    p->setInitializer(a);

    /// double d[2] = {1.1, 2.2};
    ArrayType *arrayType = ArrayType::get(irBuilder->getDoubleTy(), 2);
    module->getOrInsertGlobal("d", arrayType);
    GlobalVariable *d = module->getNamedGlobal("d");
    d->setInitializer(ConstantArray::get(arrayType, {
        ConstantFP::get(irBuilder->getDoubleTy(), 1.1),
        ConstantFP::get(irBuilder->getDoubleTy(), 2.2)
    }));

    /// struct A {int a, float b, double d, int *p};
    StructType *structType = StructType::create(*llvmContext, "A");
    structType->setBody({
        irBuilder->getInt32Ty(),
        irBuilder->getFloatTy(),
        irBuilder->getDoubleTy(),
        PointerType::getInt32PtrTy(*llvmContext)
    });
    // struct A s = {1,1.1,2.2, NULL};
    module->getOrInsertGlobal("s", structType);
    GlobalVariable *s = module->getNamedGlobal("s");
    s->setInitializer(ConstantStruct::get(structType, {
        irBuilder->getInt32(1),
        ConstantFP::get(irBuilder->getFloatTy(), 1.1),
        ConstantFP::get(irBuilder->getDoubleTy(), 2.2),
        ConstantPointerNull::get(PointerType::getInt32PtrTy(*llvmContext))
    }));

    /// double *dp[2] = {};
    /// double *fp = dp;
    module->print(errs(), nullptr);

    return 0;
}