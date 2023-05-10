/***********************************
 * File:     SemaAnalyse.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "SemaAnalyse.h"
#include "Match.h"

namespace lcc {
SemaSyntax::TranslationUnit
SemaAnalyse::Analyse(const Syntax::TranslationUnit &translationUnit) {
  return visitor(translationUnit);
}

SemaSyntax::TranslationUnit
SemaAnalyse::visitor(const Syntax::TranslationUnit &translationUnit) {
  std::vector<SemaSyntax::TranslationUnit::Variant> globals;
  for (const auto &iter : translationUnit.getGlobals()) {
    match(
        iter,
        [this, &globals](const Syntax::Declaration &declaration) {
          globals.emplace_back(visitor(declaration));
        },
        [this, &globals](const Syntax::FunctionDefinition &functionDefinition) {
          globals.emplace_back(visitor(functionDefinition));
        });
  }
  return SemaSyntax::TranslationUnit(std::move(globals));
}

SemaSyntax::FunctionDefinition
SemaAnalyse::visitor(const Syntax::FunctionDefinition &functionDefinition) {
/// 1, 新作用域
/// 2, 确定函数的类型
}

SemaSyntax::Declaration
SemaAnalyse::visitor(const Syntax::Declaration &declaration) {

}
} // namespace lcc
