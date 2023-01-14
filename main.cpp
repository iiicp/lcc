#include <iostream>
#include <fstream>
#include "Lexer.h"
#include "Parser.h"
#include "CodeGen.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/ManagedStatic.h"

int main(int argc, char *argv[]) {
    std::cout << "Hello, lcc!" << std::endl;

    if (argc < 2) {
      std::cout << "please input file!" << std::endl;
      return 0;
    }

    std::ifstream file(argv[1]);
    if(!file.is_open())
    {
        std::cerr<<"Could not open source file";
        return -1;
    }
    std::string source;
    file.seekg(0,std::ios_base::end);
    std::size_t pos = file.tellg();
    source.resize(pos);
    file.seekg(0,std::ios_base::beg);
    file.read(source.data(),source.size());

    auto tokens = lcc::tokenize(source);
    for (auto &tok : tokens.data()) {
        std::cout << "(" << tok.getLine(tokens) << "," << tok.getColumn(tokens) << ", " << tok.getValue() << ")" << std::endl;
    }
#if 0
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
