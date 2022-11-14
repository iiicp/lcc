#include "lcc/CodeGen/CodeGen.h"
#include "lcc/Lexer/Lexer.h"
#include "lcc/Parser/Parser.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/TargetSelect.h"
#include <iostream>

int main(int argc, char *argv[]) {
  std::cout << "Hello, lcc!" << std::endl;

  if (argc < 2) {
    std::cout << "please input file!" << std::endl;
    return 0;
  }

  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
      llvm::MemoryBuffer::getFile(argv[1]);
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::WithColor::error(llvm::errs(), argv[0])
        << "Error reading " << argv[1] << ": " << BufferError.message() << "\n";
    return -1;
  }
  llvm::SourceMgr SrcMgr;
  lcc::DiagnosticsEngine Diags(SrcMgr);

  // Tell SrcMgr about this buffer, which is what the
  // parser will pick up.
  SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());

  lcc::Lexer lex(SrcMgr, Diags);
  auto tokens = lex.Tokenize();
  //    for (auto &tok : tokens) {
  //        std::cout << tok.getLocation();
  //        std::cout << tok.GetTokenSpelling() << std::endl;
  //    }

  lcc::Parser parser(std::move(tokens));
  auto program = parser.ParseProgram();

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
    std::unique_ptr<llvm::ExecutionEngine> ee(
        builder.setErrorStr(&error)
            .setEngineKind(llvm::EngineKind::JIT)
            .setOptLevel(llvm::CodeGenOpt::Level::None)
            .create());
    // 获取函数地址
    void *address = (void *)ee->getFunctionAddress("main");
    std::cout << ((int (*)())address)() << std::endl;
  }
  llvm::llvm_shutdown();
  return 0;
}
