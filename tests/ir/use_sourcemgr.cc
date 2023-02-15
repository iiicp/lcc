/***********************************
 * File:     use_sourcemgr.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/15
 *
 * Sign:     enjoy life
 ***********************************/
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"
int main() {

  llvm::StringRef ref{"int a = 0, b = 1;"};
  auto mb = llvm::MemoryBuffer::getMemBuffer(ref);
  llvm::outs() << mb->getBuffer() << "\n";

  llvm::SourceMgr mgr;
  mgr.AddNewSourceBuffer(std::move(mb), llvm::SMLoc());
  llvm::outs() << "num buffers: " << mgr.getNumBuffers() << "\n";
  llvm::outs() << "main file id: " << mgr.getMainFileID() << "\n";
  llvm::StringRef srf = mgr.getMemoryBuffer(1)->getBuffer();
  llvm::outs() << "buffer content: " << srf << "\n";

  llvm::SMLoc loc = llvm::SMLoc::getFromPointer(srf.begin() + 9);
  auto locate = mgr.getLineAndColumn(loc, 1);
  llvm::outs() << "[" << locate.first << "," << locate.second << "]" << "\n";

  mgr.PrintMessage(loc, llvm::SourceMgr::DiagKind::DK_Note, "test...");
  return 0;
}