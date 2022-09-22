#include <iostream>
#include <string>
#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

int main() {
    std::cout << "Hello, lcc!" << std::endl;

    std::unique_ptr<LLVMContext> llvmContext = std::make_unique<LLVMContext>();
    std::unique_ptr<Module> module = std::make_unique<Module>("llc", *llvmContext);
    std::unique_ptr<IRBuilder<>> irBuilder = std::make_unique<IRBuilder<>>(*llvmContext);

    std::string variableName = "global_variable";
    module->getOrInsertGlobal(variableName, irBuilder->getInt32Ty());
    GlobalVariable *gVar = module->getGlobalVariable(variableName);
    gVar->setInitializer(irBuilder->getInt32(10));

    module->print(errs(), nullptr);

    return 0;
}
