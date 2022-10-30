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
#include "llvm/IR/Verifier.h"
#include <unordered_map>
#include <unordered_map>
namespace lcc {

using LLVMValueSignPair = std::pair<llvm::Value *, bool>;
using LLVMTypePtr = llvm::Type *;

class CodeGenContext {
public:
  llvm::LLVMContext mContext;
  std::unique_ptr<llvm::Module> mModule;
  llvm::IRBuilder<> mIrBuilder{mContext};

  std::vector<llvm::BasicBlock *> mBreaks;
  std::vector<llvm::BasicBlock *> mContinues;

  llvm::Function *mCurrentFunc;
  std::vector<std::list<std::pair<std::string, LLVMValueSignPair>>> mLocalScope;
  std::unordered_map<std::string, LLVMValueSignPair> mGlobalScope;

  void AddLocal(const std::string &name, LLVMValueSignPair valueSignPair) {
    mLocalScope.back().push_back({name, valueSignPair});
  }

  void AddGlobal(const std::string &name, LLVMValueSignPair valueSignPair) {
    mGlobalScope[name] = valueSignPair;
  }

  LLVMValueSignPair FindVar(const std::string& name) {
    for (auto list = mLocalScope.rbegin(); list != mLocalScope.rend(); ++list) {
      for (auto begin = list->rbegin(); begin != list->rend(); ++begin) {
        if (begin->first == name) {
          return begin->second;
        }
      }
    }
    return mGlobalScope[name];
  }

  void PushScope() {
    mLocalScope.emplace_back();
  }

  void PopScope() {
    mLocalScope.pop_back();
  }

  void ClearScope() {
    mLocalScope.clear();
    PushScope();
  }
};
} // namespace lcc::parser

#endif // LCC_CODE_GEN_CONTEXT_H
