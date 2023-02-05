#include <iostream>
#include <fstream>
#include "Version.h"
#include "Lexer.h"
#include "Parser.h"
#include "DumpTool.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"

static const char *Head = "lcc - based llvm c compiler";

static llvm::cl::list<std::string>
    InputFiles(llvm::cl::Positional,
               llvm::cl::desc("<input-files>"));

static llvm::cl::opt<bool>
    DumpTokens("dumpTokens",
            llvm::cl::desc("<Enable Token output on terminals>"));
static llvm::cl::opt<bool>
    DumpAST("dumpAst",
               llvm::cl::desc("<Enable Ast output on terminals>"));

void printVersion(llvm::raw_ostream &OS) {
  OS << Head << " " << lcc::getLccVersion() << "\n";
  OS.flush();
  exit(EXIT_SUCCESS);
}

int main(int argc, char *argv[]) {
    llvm::InitLLVM X(argc, argv);
    llvm::cl::SetVersionPrinter(&printVersion);
    llvm::cl::ParseCommandLineOptions(argc, argv, Head);

    for (const auto &F : InputFiles) {
      llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
          FileOrErr = llvm::MemoryBuffer::getFile(F);
      if (std::error_code BufferError =
              FileOrErr.getError()) {
        llvm::WithColor::error(llvm::errs(), argv[0])
            << "Error reading " << F << ": "
            << BufferError.message() << "\n";
      }
      std::string source((*FileOrErr)->getBuffer());
      lcc::Lexer lexer(source);
      auto tokens = lexer.tokenize();
      if (DumpTokens) {
        lcc::dump::dumpTokens(tokens);
      }
      lcc::Parser parser(std::move(tokens));
      auto translationUnit = parser.ParseTranslationUnit();
      if (DumpAST) {
        lcc::dump::dumpAst(translationUnit);
      }
    }

#if 0
    lcc::CodeGenContext context;
    lcc::CodeGen gen(std::move(program), context);
    assert(!llvm::verifyModule(*context.mModule));

    /// 此处要初始化jit环境
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    LLVMLinkInMCJIT();
    {
      llvm::EngineBuilder builder(std::move(context.mModule));
      std::string error;
      std::unique_ptr<llvm::ExecutionEngine> ee(builder.setErrorStr(&error).setEngineKind(llvm::EngineKind::JIT)
                                                    .setOptLevel(llvm::CodeGenOpt::Level::None).create());
      // 获取函数地址
      void *address = (void *)ee->getFunctionAddress("main");
      std::cout << ((int(*)())address)() << std::endl;
    }
    llvm::llvm_shutdown();
#endif
    return 0;
}
