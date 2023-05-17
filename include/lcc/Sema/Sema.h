/***********************************
 * File:     Sema.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SEMA_ANALYZER_H
#define LCC_SEMA_ANALYZER_H
#include "lcc/AST/AST.h"
#include "lcc/AST/SemaAST.h"

namespace lcc {
class Sema {
public:
  Sema(){}
  SemaSyntax::TranslationUnit Analyse(const Syntax::TranslationUnit &translationUnit);
private:
  SemaSyntax::TranslationUnit
  visit(const Syntax::TranslationUnit &translationUnit);
  SemaSyntax::FunctionDefinition
  visit(const Syntax::FunctionDefinition &functionDefinition);
  SemaSyntax::Declaration visit(const Syntax::Declaration &declaration);
};
} // namespace lcc

#endif // LCC_SEMA_ANALYZER_H
