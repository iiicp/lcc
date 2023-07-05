#include "lcc/Basic/Diagnostic.h"
#include "lcc/Basic/Version.h"
#include "lcc/CodeGen/CodeGen.h"
#include "lcc/Lexer/Lexer.h"
#include "lcc/Parser/Parser.h"
#include "lcc/Sema/Sema.h"
#include "lcc/Support/DumpTool.h"
#include "llvm/Bitcode/BitcodeWriterPass.h"
#include "llvm/IR/IRPrintingPasses.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include <filesystem>
#include <llvm/Support/FileSystem.h>
#include <optional>

static const char *Head = "lcc - based llvm c compiler";

static llvm::cl::list<std::string> InputFiles(llvm::cl::Positional,
                                              llvm::cl::desc("<input-files>"),
                                              llvm::cl::OneOrMore);

static llvm::cl::opt<std::string>
    OutputFileName("o", llvm::cl::desc("Write output to <file>"),
                   llvm::cl::value_desc("file"));

static llvm::cl::opt<bool> CompileOnly(
    "c", llvm::cl::desc("Only run preprocess, compile, and assemble steps"));

static llvm::cl::opt<bool>
    AssemblyOnly("S",
                 llvm::cl::desc("Only run preprocess and compilation steps"));

static llvm::cl::opt<bool>
    PreprocessOnly("E", llvm::cl::desc("Only run the preprocessor"));

static llvm::cl::opt<bool>
    EmitLLVM("emit-llvm",
             llvm::cl::desc(
                 "Use the LLVM representation for assembler and object files"));

static llvm::cl::opt<bool>
    EmitTokens("emit-tokens",
               llvm::cl::desc("Emit Tokens files for source inputs"));
static llvm::cl::opt<bool>
    EmitAst("emit-ast", llvm::cl::desc("Emit AST files for source inputs"));

static llvm::cl::opt<bool> TimeOpt("time",
                                   llvm::cl::desc("Time individual commands"));

void printVersion(llvm::raw_ostream &OS) {
  OS << Head << " " << lcc::getLccVersion() << "\n";
  OS.flush();
  exit(EXIT_SUCCESS);
}

enum class Action { Preprocess, Compile, AssemblyOutput, Link };

bool compileCFile(Action action, std::filesystem::path sourceFile) {
  std::optional<llvm::TimerGroup> timer;
  if (TimeOpt) {
    timer.emplace("Compilation", "Time it took for the whole compilation of " +
                                     sourceFile.string());
  }

  /// file read to memory
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
      llvm::MemoryBuffer::getFile(sourceFile.string());
  if (std::error_code BufferError = FileOrErr.getError()) {
    llvm::WithColor::error(llvm::errs(), "lcc")
        << "Error reading " << sourceFile.string() << ": "
        << BufferError.message() << "\n";
    return false;
  }

  /// lexer begin
  std::optional<llvm::Timer> lexerTimer;
  std::optional<llvm::TimeRegion> lexerTimeRegion;
  if (timer) {
    lexerTimer.emplace("lexer", "Time it took to lexer " + sourceFile.string(),
                       *timer);
    lexerTimeRegion.emplace(*lexerTimer);
  }
  llvm::SourceMgr mgr;
  lcc::DiagnosticEngine diag(mgr, llvm::errs());
  std::string sourceCode((*FileOrErr)->getBuffer());
  lcc::Lexer lexer(mgr, diag, std::move(sourceCode), (*FileOrErr)->getBufferIdentifier());
  auto ppTokens = lexer.tokenize();
  if (diag.numErrors())
    return false;
  auto tokens = lexer.toCTokens(std::move(ppTokens));
  if (diag.numErrors())
    return false;
  if (EmitTokens) {
    lcc::dump::dumpTokens(tokens);
  }
  lexerTimeRegion.reset();
  /// lexer end

  /// parser begin
  std::optional<llvm::Timer> parserTimer;
  std::optional<llvm::TimeRegion> parserTimeRegion;
  if (timer) {
    parserTimer.emplace("Parser",
                        "Time it took to parse " + sourceFile.string(), *timer);
    parserTimeRegion.emplace(*parserTimer);
  }
  lcc::Parser parser(tokens, diag);
  auto translationUnit = parser.ParseTranslationUnit();
  if (EmitAst) {
    lcc::dump::dumpAst(translationUnit);
  }
  parserTimeRegion.reset();
  /// parser end

  /// semantics begin
  std::optional<llvm::Timer> semanticsTimer;
  std::optional<llvm::TimeRegion> semanticsTimeRegion;
  if (timer) {
    semanticsTimer.emplace("Semantics",
                           "Time it took to semantics " + sourceFile.string(),
                           *timer);
    semanticsTimeRegion.emplace(*semanticsTimer);
  }
  lcc::Sema semaAnalyse;
  auto semaTranslationUnit = semaAnalyse.Analyse(translationUnit);
  semanticsTimeRegion.reset();
  /// semantics end

  /// codegen begin
  std::optional<llvm::Timer> codeGenTimer;
  std::optional<llvm::TimeRegion> codeGenTimeRegion;
  if (timer) {
    codeGenTimer.emplace(
        "CodeGen", "Time it took to codegen " + sourceFile.string(), *timer);
    codeGenTimeRegion.emplace(*codeGenTimer);
  }
  llvm::LLVMContext context;
  llvm::Module module("", context);
  lcc::CodeGen codeGen(semaTranslationUnit, module);
  auto targetMachine = codeGen.Run();
  if (llvm::verifyModule(module, &llvm::errs())) {
    llvm::errs().flush();
    module.print(llvm::outs(), nullptr);
    std::terminate();
  }
  codeGenTimeRegion.reset();
  /// codegen end

  /// compile to native object code begin
  std::string outputFile;
  if (!OutputFileName.empty()) {
    outputFile = OutputFileName;
  } else {
    auto path = sourceFile;
    if (action == Action::AssemblyOutput) {
      if (EmitLLVM) {
        path.replace_extension("ll");
      } else {
        path.replace_extension("s");
      }
    } else {
      if (EmitLLVM) {
        path.replace_extension("bc");
      } else {
        path.replace_extension("o");
      }
    }
    outputFile = path.string();
  }

  std::error_code ec;
  llvm::raw_fd_ostream os(outputFile, ec, llvm::sys::fs::OpenFlags::OF_None);
  if (ec) {
    llvm::errs() << "failed to open output file";
    return false;
  }

  std::optional<llvm::Timer> compileTimer;
  std::optional<llvm::TimeRegion> compileTimeRegion;
  if (timer) {
    compileTimer.emplace(
        "Compile",
        "Time it took for LLVM to generate native object code " +
            sourceFile.string(),
        *timer);
    compileTimeRegion.emplace(*compileTimer);
  }
  llvm::PassManagerBuilder builder;
//  targetMachine->adjustPassManager(builder);
  llvm::legacy::PassManager pass;
  builder.populateModulePassManager(pass);
  if (EmitLLVM) {
    if (action == Action::AssemblyOutput) {
      pass.add(llvm::createPrintModulePass(os));
    } else {
      pass.add(llvm::createBitcodeWriterPass(os));
    }
  } else {
    if (targetMachine->addPassesToEmitFile(
            pass, os, nullptr,
            action == Action::AssemblyOutput
                ? llvm::CodeGenFileType::CGFT_AssemblyFile
                : llvm::CodeGenFileType::CGFT_ObjectFile)) {
      return false;
    }
  }
  pass.run(module);
  compileTimeRegion.reset();
  os.flush();
  /// compile to native object code end
  return true;
}

int doActionOnAllFiles(Action action) {
  for (const auto &F : InputFiles) {
    auto path = std::filesystem::path(F);
    auto extension = path.extension();
    if (extension == ".c") {
      bool res = compileCFile(action, path);
      if (!res)
        return -1;
    }
  }
  return 0;
}

int main(int argc, char *argv[]) {
  llvm::InitLLVM X(argc, argv);
  llvm::cl::SetVersionPrinter(&printVersion);
  llvm::cl::ParseCommandLineOptions(argc, argv, Head);

  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  if (InputFiles.empty()) {
    llvm::errs() << "no source files specified";
    return -1;
  }

  if (CompileOnly) {
    if (AssemblyOnly) {
      llvm::errs()
          << "cannot compile to object file and assembly at the same time";
      return -1;
    }
    if (PreprocessOnly) {
      llvm::errs()
          << "cannot compile to object file add preprocess at the same time";
      return -1;
    }
    return doActionOnAllFiles(Action::Compile);
  }

  if (AssemblyOnly) {
    if (PreprocessOnly) {
      llvm::errs()
          << "cannot compile to assembly file add preprocess at the same time";
      return -1;
    }
    return doActionOnAllFiles(Action::AssemblyOutput);
  }

  if (PreprocessOnly) {
    return doActionOnAllFiles(Action::Preprocess);
  }

  return doActionOnAllFiles(Action::Compile);
}
