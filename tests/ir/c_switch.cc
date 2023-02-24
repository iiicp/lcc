/***********************************
 * File:     c_switch.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/23
 ***********************************/
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/Verifier.h"
#include <memory>
using namespace llvm;

int test_switch(int a) {
  switch (a) {
  case 1: return 1;
  case 5: return 25;
  default: return a;
  }
  return a;
}

int main() {

  auto context = std::make_unique<LLVMContext>();
  auto module = std::make_unique<Module>("c_switch", *context);
  auto irBuild = std::make_unique<IRBuilder<>>(*context);

  auto *intType = irBuild->getInt32Ty();
  auto *funcType = FunctionType::get(intType, {intType}, false);
  auto *func = Function::Create(funcType, GlobalValue::LinkageTypes::ExternalLinkage, "test_switch", module.get());
  std::vector<std::string> param{"a"};
  int idx = 0;
  for (Function::arg_iterator it = func->arg_begin(); it != func->arg_end(); ++it, ++idx) {
    it->setName(param[idx]);
  }

  BasicBlock *entryBB = BasicBlock::Create(*context, "entry", func);
  auto* defaultBB = llvm::BasicBlock::Create(*context, "switch.default", func);
  auto* caseBB1 = llvm::BasicBlock::Create(*context, "switch.case1", func);
  auto* caseBB2 = llvm::BasicBlock::Create(*context, "switch.case2", func);
  irBuild->SetInsertPoint(entryBB);
  Value *a = func->getArg(0);
  auto *switchInst =  irBuild->CreateSwitch(a, defaultBB, 2);
  switchInst->addCase(irBuild->getInt32(1), caseBB1);
  switchInst->addCase(irBuild->getInt32(5), caseBB1);

  irBuild->SetInsertPoint(caseBB1);
  irBuild->CreateRet(irBuild->getInt32(1));

  irBuild->SetInsertPoint(caseBB2);
  irBuild->CreateRet(irBuild->getInt32(5*5));

  irBuild->SetInsertPoint(defaultBB);
  irBuild->CreateRet(a);

  verifyFunction(*func);
  module->print(errs(), nullptr);

  /// 此处要初始化jit环境
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  LLVMLinkInMCJIT();

  int ret = -1;
  {
    EngineBuilder builder(std::move(module));
    auto b = builder.setEngineKind(EngineKind::JIT).setOptLevel(llvm::CodeGenOpt::Level::None).create();
    std::unique_ptr<ExecutionEngine> ee(b);
    // 获取函数地址
    void *address = (void *)ee->getFunctionAddress("test_switch");
    ret = ((int(*)(int))address)(10);
  }
  llvm_shutdown();

  outs() << ret << "\n";
  return 0;
}