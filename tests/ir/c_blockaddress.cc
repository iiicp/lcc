/***********************************
 * File:     c_blockaddress.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/18
 ***********************************/

#include "llvm/IR/LLVMContext.h"  // 公共的数据结构，用来关联模块
#include "llvm/IR/Module.h" // 一个源文件的抽象
#include "llvm/IR/IRBuilder.h"   // 指令生成器
#include "llvm/IR/Verifier.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/TargetSelect.h"

#include <iostream>
#include <vector>
#include <memory>
#include <string>

namespace llvm {
class BasicBlock;
class ExecutionEngine;
class Function;
class Module;
class PointerType;
class StructType;
class Value;
class LLVMContext;
}

using namespace llvm;
using namespace std;

/* c function:

int jit_func(int index, void** label)
{
entry: {
  goto dispatch;
}

basicblock1: {
  printf("this is basicblock1\n");
  return 1;
}

basicblock2: {
  printf("this is basicblock2\n");
  return 2;
}

dispatch: {
  if (index < 2 && label[index] != NULL) {
    goto *label[index]; // indirectbr
  } else {
    return 0;
  }
}
  return 0;
}

*/
    typedef int (*jit_func)(int index);

#define CONST_int8(v)  ConstantInt::get(IntegerType::get(*llvmContext, 8),  v)
#define CONST_int16(v) ConstantInt::get(IntegerType::get(*llvmContext, 16), v)
#define CONST_int32(v) ConstantInt::get(IntegerType::get(*llvmContext, 32), v)
#define CONST_int64(v) ConstantInt::get(IntegerType::get(*llvmContext, 64), v)
#define getIntegerType(x) (IntegerType::get(*llvmContext, x))




int main(void) {

  // Create LLVM base environment
  LLVMContext *llvmContext = new LLVMContext();
  Module *module = new Module("func-module", *llvmContext);
//  std::unique_ptr<Module> module = std::make_unique<Module>("func-module", *llvmContext);
  IRBuilder <> irBuilder(*llvmContext);

  // Create @func(i32 guest_pc)
  vector <Type *> params;
  params.push_back(irBuilder.getInt32Ty());
  Type *sp = irBuilder.getInt8PtrTy();
  sp->getPointerTo();
  // params.push_back(sp->getPointerTo()); // create i8**
  FunctionType *fType = FunctionType::get(irBuilder.getInt32Ty(), ArrayRef<Type*>(params), false);
  Function *func = Function::Create(fType, GlobalValue::ExternalLinkage, "func", module);

  Function::arg_iterator it = func->arg_begin();
  it->setName("guest_pc");
  Value *arg = it;

  // Create basic block
  BasicBlock *entry_bb = BasicBlock::Create(*llvmContext, "entry_bb", func);
  BasicBlock *dispatch_bb = BasicBlock::Create(*llvmContext, "dispatch_bb", func);
  BasicBlock *dispatch_then_bb = BasicBlock::Create(*llvmContext, "dispatch_then_bb", func);
  BasicBlock *indirectbr_bb = BasicBlock::Create(*llvmContext, "indirectbr_bb", func);
  BasicBlock *ret_bb = BasicBlock::Create(*llvmContext, "ret_bb", func);
  BasicBlock *instr1_bb = BasicBlock::Create(*llvmContext, "instr1_bb", func);
  BasicBlock *instr2_bb = BasicBlock::Create(*llvmContext, "instr2_bb", func);

  // Create global value
  ArrayType *arrayType = ArrayType::get(irBuilder.getInt8PtrTy(), 3);
  module->getOrInsertGlobal("label_array", arrayType);
  GlobalVariable *label_array = module->getNamedGlobal("label_array");

  vector <Constant *> array_elems;
  array_elems.push_back(BlockAddress::get(func, instr1_bb));
  array_elems.push_back(BlockAddress::get(func, instr2_bb));
  array_elems.push_back(BlockAddress::get(func, ret_bb));
  label_array->setInitializer(ConstantArray::get(arrayType, array_elems));


  // Edit entry_bb
  irBuilder.SetInsertPoint(entry_bb);
  Value *ret_val = irBuilder.CreateAlloca(irBuilder.getInt32Ty(), 0, "ret_val");
  Value *index = irBuilder.CreateAlloca(irBuilder.getInt32Ty(), 0, "idx");
  Value *b1 = irBuilder.CreateAlloca(irBuilder.getInt32Ty(), 0, "b1");
  Value *b2 = irBuilder.CreateAlloca(irBuilder.getInt32Ty(), 0, "b2");
  new StoreInst(CONST_int32(404), ret_val, false, entry_bb);
  new StoreInst(arg, index, false, entry_bb);
  irBuilder.CreateBr(dispatch_bb);

  // Edit dispatch_bb
  irBuilder.SetInsertPoint(dispatch_bb);
  Value *idx1 = irBuilder.CreateLoad(irBuilder.getInt32Ty(), index, "idx1");
  Value *cmp_ret = irBuilder.CreateICmpSLT(idx1, CONST_int32(1024), "cmp_ret");
  irBuilder.CreateCondBr(cmp_ret, dispatch_then_bb, ret_bb);

  // Edit dispatch_then_bb
  irBuilder.SetInsertPoint(dispatch_then_bb);
  Value *idx2 = irBuilder.CreateLoad(irBuilder.getInt32Ty(), index, "idx2");
  llvm::SmallVector<llvm::Value *, 4> IdxList;
  IdxList.push_back(
      llvm::ConstantInt::get(irBuilder.getInt32Ty(), 0));
  IdxList.push_back(idx2);
  Value *label_entry = irBuilder.CreateInBoundsGEP(arrayType, label_array, IdxList);
//  Value *label_entry = GetElementPtrInst::CreateInBounds(label_array, idx2, "label_entry", dispatch_then_bb);
  Value *a = new BitCastInst(label_entry, sp->getPointerTo(), "", dispatch_then_bb);
  Value *target = new LoadInst(irBuilder.getInt8PtrTy(), a, "", false, dispatch_then_bb);
  irBuilder.CreateBr(indirectbr_bb);

  // Edit indirectbr_bb
  irBuilder.SetInsertPoint(indirectbr_bb);
  PHINode *phi = irBuilder.CreatePHI(irBuilder.getInt8PtrTy(), 64, "target");
  phi->addIncoming(target, dispatch_then_bb);
  IndirectBrInst *indirect_br = IndirectBrInst::Create(phi, 0, indirectbr_bb);
  indirect_br->addDestination(instr1_bb);
  indirect_br->addDestination(instr2_bb);
  indirect_br->addDestination(ret_bb);

  // Edit instr1_bb
  irBuilder.SetInsertPoint(instr1_bb);
  new StoreInst(CONST_int32(1), b1, false, instr1_bb);
  Value *return_val1 = irBuilder.CreateLoad(irBuilder.getInt32Ty(), b1);
  irBuilder.CreateRet(return_val1);

  // Edit instr2_bb
  irBuilder.SetInsertPoint(instr2_bb);
  new StoreInst(CONST_int32(2), b2, false, instr2_bb);
  Value *return_val2 = irBuilder.CreateLoad(irBuilder.getInt32Ty(), b2);
  irBuilder.CreateRet(return_val2);

  // Edit ret_bb
  irBuilder.SetInsertPoint(ret_bb);
  Value *return_val = irBuilder.CreateLoad(irBuilder.getInt32Ty(), ret_val);
  irBuilder.CreateRet(return_val);

  // Initialize LLVM JIT environment
  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();
  LLVMLinkInMCJIT();

  assert(!verifyModule(*module));
  module->print(llvm::outs(), nullptr);

  // Create LLVM JIT exec_engine
//  string error;
//  EngineBuilder builder(module);
//  builder.setErrorStr(&error);
//  builder.setEngineKind(EngineKind::JIT);
//  builder.setOptLevel(CodeGenOpt::Aggressive);
//  ExecutionEngine *exec_engine = builder.create();
//
//  // Print debug info
//  module->dump();  //print IR
//  cout << error << endl;
//  cout << "verifyModule " << verifyModule(*module) << endl;
//  cout << "verifyFunction " << verifyFunction(*func, PrintMessageAction) << endl;
//
//  // Run jit-func
//  void *func_addr = exec_engine->getPointerToFunction(func);
//  jit_func func_run = (jit_func)func_addr; // (int index);
//  int ret = func_run(0);
//
//  // Print result
//  cout << ret << endl;


  int ret = -1;
  {
    std::unique_ptr<Module> m;
    m.reset(module);
    EngineBuilder builder(std::move(m));
    std::string errorStr;
    auto b = builder.setErrorStr(&errorStr).setEngineKind(EngineKind::JIT).setOptLevel(llvm::CodeGenOpt::Level::None).create();
    if (!errorStr.empty())
      std::cout << "error Str: " << errorStr << std::endl;
    std::unique_ptr<ExecutionEngine> ee(b);
    // 获取函数地址
    void *address = (void *)ee->getFunctionAddress("func");
    ret = ((int(*)(int))address)(1);
  }
  llvm_shutdown();

  std::cout << ret << std::endl;

  return 0;
}
