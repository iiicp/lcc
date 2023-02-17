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
#include "llvm/Support/WithColor.h"
bool hasUtf8BOM(llvm::StringRef buf) {
  return (buf.size() >= 3) && (buf[0] == '\xef') &&
         (buf[1] == '\xbb') && (buf[2] == '\xbf');
}

int main() {
  {
    llvm::StringRef ref{"int a = 0, b = 1;"};
    auto mb = llvm::MemoryBuffer::getMemBuffer(ref, "<stdin>");
    llvm::outs() << "buf: " << mb->getBuffer() << "\n";
    llvm::outs() << "name: " << mb->getBufferIdentifier() << "\n";

    llvm::SourceMgr mgr;
    mgr.AddNewSourceBuffer(std::move(mb), llvm::SMLoc());
    llvm::outs() << "num buffers: " << mgr.getNumBuffers() << "\n";
    llvm::outs() << "main file id: " << mgr.getMainFileID() << "\n";
    llvm::StringRef srf = mgr.getMemoryBuffer(1)->getBuffer();
    llvm::outs() << "buffer content: " << srf << "\n";

    llvm::SMLoc loc = llvm::SMLoc::getFromPointer(srf.begin() + 9);
    auto locate = mgr.getLineAndColumn(loc, 1);
    llvm::outs() << "[" << locate.first << "," << locate.second << "]"
                 << "\n";

    mgr.PrintMessage(loc, llvm::SourceMgr::DiagKind::DK_Note, "test...");
  }

  {
    std::string file = "../../tests/ir/use_sourcemgr.cc";
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(file);
    std::error_code BufferError = FileOrErr.getError();
    if (BufferError) {
      llvm::WithColor::error(llvm::errs(), file)
          << "Error reading " << file << ": " << BufferError.message() << "\n";
    }
    auto &mb2 = *FileOrErr;
    llvm::StringRef sr = mb2->getBufferIdentifier();
    llvm::outs() << sr << "\n";
  }

  {
    std::string file = "../../tests/ir/utf8bom.txt";
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(file);
    std::error_code BufferError = FileOrErr.getError();
    if (BufferError) {
      llvm::WithColor::error(llvm::errs(), file)
          << "Error reading " << file << ": "
          << BufferError.message() << "\n";
    }
    auto &mb2 = *FileOrErr;
    llvm::StringRef sr = mb2->getBufferIdentifier();
    llvm::outs() << sr << "\n";

    llvm::outs() << mb2->getBufferSize() << "\n";
    llvm::outs() << mb2->getBuffer() << "\n";
  }

  {
    std::string file = "../../tests/ir/utf8bom.txt";
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr = llvm::MemoryBuffer::getFile(file);
    std::error_code BufferError = FileOrErr.getError();
    llvm::StringRef ref = (*FileOrErr)->getBuffer();

    llvm::SourceMgr mgr;
    if (hasUtf8BOM(ref)) {
        llvm::StringRef newRef((const char *)(ref.bytes_begin())+3);
        std::unique_ptr<llvm::MemoryBuffer> newBuf = llvm::MemoryBuffer::getMemBuffer(newRef, (*FileOrErr)->getBufferIdentifier());
        auto n = std::move(*FileOrErr);
        llvm::outs() << "hello,world\n";
    }else {
//      mgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
      auto n = std::move(*FileOrErr);
    }
  }

  return 0;
}