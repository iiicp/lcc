/***********************************
 * File:     Scope.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SCOPE_H
#define LCC_SCOPE_H

#include "lcc/AST/SemaAST.h"
#include "lcc/Sema/Type.h"
#include "llvm/ADT/ScopeExit.h"
#include <string_view>
#include <unordered_map>

namespace lcc {

/// 聚合记录所有的tag类型信息
/// 1. 添加debug信息
/// 2.
/// 可以通过id绑定到某一个类型上，这样可以和添加debug信息复用。就是为了添加debug信息
///    导致的复用

class Scope {
public:
  using DeclarationSymbol =
      std::variant<SemaSyntax::Declaration *, SemaSyntax::FunctionDefinition *>;

private:
  struct Env {
    std::vector<std::pair<std::string_view, DeclarationSymbol>>
        declarationSymbols_;
  };
  /// 记录当前的环境id
  size_t currentEnvId_{0};
  /// 默认添加一个全局的环境
  std::vector<Env> scopes_ = {{}};

public:
  auto EnterScope() {
    scopes_.push_back({});
    currentEnvId_++;
    return llvm::make_scope_exit([&] {
      scopes_.pop_back();
      currentEnvId_--;
    });
  }

  const DeclarationSymbol *FindDeclSymbol(std::string_view name) {
    return FindDeclSymbol(name, currentEnvId_);
  }

private:
  const DeclarationSymbol *FindDeclSymbol(std::string_view name, size_t envId);
};
} // namespace lcc
#endif // LCC_SCOPE_H
