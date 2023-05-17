/***********************************
 * File:     Scope.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "lcc/Sema/Scope.h"
#include "lcc/Basic/Match.h"

namespace lcc {

const Scope::DeclarationSymbol* Scope::FindDeclSymbol(std::string_view name, size_t envId) {
  auto curr = envId;
  while (curr >= 0) {
    auto &env = scopes_[envId];
    for (auto &[name_, symbol_] : env.declarationSymbols_)
      if (name_ == name) {
        return &symbol_;
      }
    curr--;
  }
  return nullptr;
}

} // namespace lcc
