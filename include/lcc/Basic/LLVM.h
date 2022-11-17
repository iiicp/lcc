/***********************************
 * File:     LLVM.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/17
 ***********************************/

#ifndef LCC_LLVM_H
#define LCC_LLVM_H

namespace llvm {
class SMLoc;
class SourceMgr;
template <typename K, typename V> class StringMap;
class StringRef;
}

namespace lcc {
using llvm::SMLoc;
using llvm::SourceMgr;
using llvm::StringMap;
using llvm::StringRef;
}

#endif // LCC_LLVM_H
