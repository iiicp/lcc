/***********************************
 * File:     Diagnostic.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/16
 ***********************************/

#include "Diagnostic.h"

namespace lcc {
namespace {
const char *DiagnosticText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "Diagnostic.def"
};
llvm::SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "Diagnostic.def"
};
}
const char *DiagnosticEngine::getDiagnosticText(unsigned int DiagID) {
  return DiagnosticText[DiagID];
}

llvm::SourceMgr::DiagKind DiagnosticEngine::getDiagnosticKind(unsigned int DiagID) {
  return DiagnosticKind[DiagID];
}
}