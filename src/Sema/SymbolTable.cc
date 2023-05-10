/***********************************
 * File:     SymbolTable.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "SymbolTable.h"
#include "Match.h"

namespace lcc {

const SymbolTable::DeclarationSymbol* SymbolTable::FindDeclSymbol(std::string_view name, size_t envId) {
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

} // namespace lcc::SemaAnalyse
