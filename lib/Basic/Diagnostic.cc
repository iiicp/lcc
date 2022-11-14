/***********************************
 * File:     Diagnostic.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/13
 *
 * Sign:     enjoy life
 ***********************************/
#include "lcc/Basic/Diagnostic.h"
using namespace lcc;
namespace {
const char *DiagnosticText[] = {
#define DIAG(ID, Level, Msg) Msg,
#include "lcc/Basic/Diagnostic.def"
};
llvm::SourceMgr::DiagKind DiagnosticKind[] = {
#define DIAG(ID, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "lcc/Basic/Diagnostic.def"
};
} // namespace

const char *DiagnosticsEngine::getDiagnosticText(unsigned DiagID) {
  return DiagnosticText[DiagID];
}

llvm::SourceMgr::DiagKind
DiagnosticsEngine::getDiagnosticKind(unsigned DiagID) {
  return DiagnosticKind[DiagID];
}