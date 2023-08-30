/***********************************
* File:     TokenStream.h
*
* Author:   蔡鹏
*
* Email:    iiicp@outlook.com
*
* Date:     2023/8/30
***********************************/

#ifndef LCC_TOKENSTREAM_H
#define LCC_TOKENSTREAM_H
#include "lcc/Lexer/TokenStreamInterface.h"
namespace lcc {
    namespace Source {
        struct File {
            std::string path;
            std::string sourceContent;
        };
    }// namespace Source
    template<typename T>
    class TokenStream final : virtual public TokenStreamInterface {
        std::vector<T> tokens_;
        std::vector<Source::File> files_;
        const llvm::SourceMgr &sourceMgr_;
        const DiagnosticEngine &diagnosticEngine_;

    public:
        TokenStream() = default;
        TokenStream(std::vector<T> tokens, std::vector<Source::File> &&files, const llvm::SourceMgr &sourceMgr, const DiagnosticEngine &diagnosticEngine)
            : tokens_(std::move(tokens)), files_(std::move(files)), sourceMgr_(sourceMgr), diagnosticEngine_(diagnosticEngine) {}

        std::pair<uint64_t, uint64_t> GetLineAndColumn(llvm::SMLoc loc) override {
            return sourceMgr_.getLineAndColumn(loc);
        }
        const llvm::SourceMgr &GetSourceMgr() override {
            return sourceMgr_;
        }
        const DiagnosticEngine &GetDiagnosticEngine() override {
            return diagnosticEngine_;
        }
        const std::vector<T> &Data() const {
            return tokens_;
        }
        std::vector<T> &Data() {
            return tokens_;
        }
        std::vector<Source::File> &GetFiles() {
            return files_;
        }
    };
}// namespace lcc


#endif//LCC_TOKENSTREAM_H
