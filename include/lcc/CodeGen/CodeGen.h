/***********************************
 * File:     CodeGen.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/4/13
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_CODEGEN_H
#define LCC_CODEGEN_H
#include "lcc/AST/SemaAST.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/TargetMachine.h"
namespace lcc {
class CodeGen {
private:
  llvm::Module &module_;
  const SemaSyntax::TranslationUnit &translationUnit_;

public:
  CodeGen(const SemaSyntax::TranslationUnit &translationUnit,
          llvm::Module &module)
      : module_(module),translationUnit_(translationUnit) {}

  ~CodeGen() {}

  std::unique_ptr<llvm::TargetMachine> Run() {
    llvm::Triple llvmTriple;
    llvmTriple.setArch(llvm::Triple::ArchType::x86_64);
    llvmTriple.setOS(llvm::Triple::OSType::MacOSX);
    llvmTriple.setEnvironment(llvm::Triple::GNU);
    module_.setTargetTriple(llvmTriple.normalize());
    std::string error;
    auto *targetM =
        llvm::TargetRegistry::lookupTarget(module_.getTargetTriple(), error);
    if (!targetM) {
      llvm::errs() << "Target lookup failed with error: " << error;
      return nullptr;
    }
    auto machine =
        std::unique_ptr<llvm::TargetMachine>(targetM->createTargetMachine(
            module_.getTargetTriple(), "generic", "", {}, {}));
    module_.setDataLayout(machine->createDataLayout());

    visit(translationUnit_);
    return machine;
  }
  const llvm::Module &GetModule() const { return module_; }
  llvm::Module &GetModule() { return module_; }

private:
  void visit(const SemaSyntax::TranslationUnit &translationUnit);
  void visit(const SemaSyntax::FunctionDefinition &functionDefinition);
  void visit(const SemaSyntax::Declaration &declaration);
};
} // namespace lcc
#endif // LCC_CODEGEN_H
