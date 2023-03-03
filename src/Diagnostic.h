/***********************************
 * File:     Diagnostic.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/16
 ***********************************/

#ifndef LCC_DIAGNOSTIC_H
#define LCC_DIAGNOSTIC_H
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/FormatVariadic.h"
#include <string>

namespace lcc {
namespace diag{
enum {
#define DIAG(ID, Level, Msg) ID,
#include "Diagnostic.def"
};
}

#define DiagReport(obj, loc, id, ...) obj.report(__FILE__, __LINE__), obj.report(loc, id, ##__VA_ARGS__)

class DiagnosticEngine {
  static const char *getDiagnosticText(unsigned DiagID);

  static llvm::SourceMgr::DiagKind getDiagnosticKind(unsigned DiagID);

  llvm::SourceMgr &mSrcMgr;
  llvm::raw_ostream &mOstream;
  unsigned NumErrors;
public:
  DiagnosticEngine(llvm::SourceMgr &SrcMgr, llvm::raw_ostream &ostream)
    :mSrcMgr(SrcMgr), mOstream(ostream), NumErrors(0) {}

  unsigned numErrors() { return NumErrors; }

  template <typename... Args>
  void report(llvm::SMLoc Loc, unsigned DiagID, Args &&... arguments) {
    std::string Msg = llvm::formatv(getDiagnosticText(DiagID), std::forward<Args>(arguments)...).str();
    llvm::SourceMgr::DiagKind Kind = getDiagnosticKind(DiagID);
    mSrcMgr.PrintMessage(mOstream, mSrcMgr.GetMessage(Loc, Kind, Msg));
    NumErrors += (Kind == llvm::SourceMgr::DK_Error);
  }

  void report(llvm::StringRef fileName, int line) {
    auto pos = fileName.find_last_of("/");
    if (pos == std::string::npos) {
      pos = fileName.find_last_of("\\");
    }
    if (pos != std::string::npos) {
      auto shortFilename = fileName.substr(pos + 1);
      llvm::errs() << "[" << shortFilename << ":" << line << "]:";
    } else {
      llvm::errs() << "[" << fileName << ":" << line << "]:";
    }
  }
};
}

#endif // LCC_DIAGNOSTIC_H
