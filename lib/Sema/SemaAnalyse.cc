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
#include "lcc/Sema/SemaAnalyse.h"
#include "lcc/Basic/Match.h"

namespace lcc {
SemaSyntax::TranslationUnit
SemaAnalyse::Analyse(const Syntax::TranslationUnit &translationUnit) {
  return visit(translationUnit);
}

SemaSyntax::TranslationUnit
SemaAnalyse::visit(const Syntax::TranslationUnit &translationUnit) {
  std::vector<SemaSyntax::TranslationUnit::Variant> globals;
//  for (const auto &iter : translationUnit.getGlobals()) {
//    match(
//        iter,
//        [this, &globals](const Syntax::Declaration &declaration) {
//          //          globals.emplace_back(visit(declaration));
//        },
//        [this, &globals](const Syntax::FunctionDefinition &functionDefinition) {
//          //          globals.emplace_back(visit(functionDefinition));
//        });
//  }
  return SemaSyntax::TranslationUnit(std::move(globals));
}

SemaSyntax::FunctionDefinition
SemaAnalyse::visit(const Syntax::FunctionDefinition &functionDefinition) {
  /// 1, 新作用域
  /// 2, 确定函数的类型
  LCC_UNREACHABLE;
}

SemaSyntax::Declaration
SemaAnalyse::visit(const Syntax::Declaration &declaration) {
    LCC_UNREACHABLE;
}
} // namespace lcc
