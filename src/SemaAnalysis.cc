/***********************************
 * File:     SemaAnalysis.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/9
 ***********************************/

#include "SemaAnalysis.h"
namespace lcc {
SemaAnalysis::SemaAnalysis(const std::vector<Token> &tokens, DiagnosticEngine &diag)
    : mTokens(tokens), Diag(diag) {}

Sema::TranslationUnit SemaAnalysis::Analyse(const Syntax::TranslationUnit &node) {
  return visit(node);
}

Sema::TranslationUnit SemaAnalysis::visit(const Syntax::TranslationUnit &node) {
  std::vector<Sema::TranslationUnit::Variant> globals;
  for (auto &iter : node.getGlobals()) {
    auto result = std::visit(overload{[&](const Syntax::FunctionDefinition &functionDefinition)
                     -> std::vector<Sema::TranslationUnit::Variant> {
                    return visit(functionDefinition);
                 },
                 [&](const Syntax::Declaration &declaration)
                     -> std::vector<Sema::TranslationUnit::Variant> {
                    return visit(declaration);
                 }}, iter);
    globals.insert(globals.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
//    globals = std::move(result);
  }
  return Sema::TranslationUnit(std::move(globals));
}

std::vector<Sema::TranslationUnit::Variant> SemaAnalysis::visit(const Syntax::FunctionDefinition &node) {
  std::vector<Sema::TranslationUnit::Variant> result;
  const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
  for (auto &iter : node.getDeclarationSpecifiers().getStorageClassSpecifiers()) {
    if (iter.getSpecifier() != Syntax::StorageClassSpecifier::Extern &&
        iter.getSpecifier() != Syntax::StorageClassSpecifier::Static) {
      DiagReport(Diag, iter.getBegin()->getSMLoc(), diag::err_sema_only_static_or_extern_allowed_in_function_definition);
      continue;
    }
    storageClassSpecifier = &iter;
  }
  return result;
}

std::vector<Sema::TranslationUnit::Variant> SemaAnalysis::visit(const Syntax::Declaration &node) {

}
}