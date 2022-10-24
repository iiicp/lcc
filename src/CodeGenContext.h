/***********************************
 * File:     CGContext.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/23
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_CODE_GEN_CONTEXT_H
#define LCC_CODE_GEN_CONTEXT_H
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <unordered_map>
namespace lcc::parser {

using LLVMValueSignPair = std::pair<llvm::Value *, bool>;
using LLVMTypePtr = llvm::Type *;

class CodeGenContext {
public:
  llvm::LLVMContext mContext;
  std::unique_ptr<llvm::Module> mModule;
  llvm::IRBuilder<> mIrBuilder{mContext};
};
} // namespace lcc::parser

#endif // LCC_CODE_GEN_CONTEXT_H
