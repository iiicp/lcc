/***********************************
* File:     TokenStreamInterface.h
*
* Author:   蔡鹏
*
* Email:    iiicp@outlook.com
*
* Date:     2023/8/30
***********************************/

#ifndef LCC_TOKENSTREAMINTERFACE_H
#define LCC_TOKENSTREAMINTERFACE_H
#include "lcc/Basic/Diagnostic.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/support/SMLoc.h"
#include <vector>
namespace lcc {
    class TokenStreamInterface {
    public:
        TokenStreamInterface() = default;
        virtual ~TokenStreamInterface() = default;
        TokenStreamInterface(const TokenStreamInterface &) = default;
        TokenStreamInterface &operator=(const TokenStreamInterface &) = default;
        TokenStreamInterface(TokenStreamInterface &&) = default;
        TokenStreamInterface &operator=(TokenStreamInterface &&) = default;

        virtual std::pair<uint64_t, uint64_t> GetLineAndColumn(llvm::SMLoc loc);
        virtual const llvm::SourceMgr &GetSourceMgr();
        virtual const DiagnosticEngine &GetDiagnosticEngine();
    };
}// namespace lcc
#endif//LCC_TOKENSTREAMINTERFACE_H
