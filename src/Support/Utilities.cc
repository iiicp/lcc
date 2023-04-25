/***********************************
 * File:     Utilities.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/1/15
 *
 * Sign:     enjoy life
 ***********************************/

#include "Utilities.h"
#include "llvm/Support/raw_ostream.h"

namespace lcc {

std::string_view getDeclaratorName(const Syntax::Declarator& declarator) {
  auto visitor = overload{
      [](auto &&, const box<Syntax::DirectDeclaratorIdent> &name)
          -> std::string_view { return name->getIdent(); },
      [](auto &&self,
         const box<Syntax::DirectDeclaratorParentheses> &declarator)
          -> std::string_view {
        return std::visit(
            [&self](auto &&value) -> std::string_view { return self(value); },
            declarator->getDeclarator().getDirectDeclarator());
      },
      [](auto &&self,
         const box<Syntax::DirectDeclaratorParamTypeList> &paramTypeList)
          -> std::string_view {
        return std::visit(
            [&self](auto &&value) -> std::string_view { return self(value); },
            paramTypeList->getDirectDeclarator());
      },
      [](auto &&self, const box<Syntax::DirectDeclaratorAssignExpr> &assignExpr)
          -> std::string_view {
        return std::visit(
            [&self](auto &&value) -> std::string_view { return self(value); },
            assignExpr->getDirectDeclarator());
      },
      [](auto &&self, const box<Syntax::DirectDeclaratorAsterisk> &asterisk)
          -> std::string_view {
        return std::visit(
            [&self](auto &&value) -> std::string_view { return self(value); },
            asterisk->getDirectDeclarator());
      }};
  return std::visit(YComb{visitor}, declarator.getDirectDeclarator());
}

const Syntax::DirectDeclaratorParamTypeList *getFuncDeclarator
    (const Syntax::Declarator &declarator) {
 const Syntax::DirectDeclaratorParamTypeList * paramTypeList_ = nullptr;
 auto visitor = overload{
     [](auto &&, const box<Syntax::DirectDeclaratorIdent> &name)
         -> std::string_view { return name->getIdent(); },
     [](auto &&self, const box<Syntax::DirectDeclaratorParentheses> &declarator)
         -> std::string_view {
       return std::visit(
           [&self](auto &&value) -> std::string_view { return self(value); },
           declarator->getDeclarator().getDirectDeclarator());
     },
     [&paramTypeList_](
         auto &&self,
         const box<Syntax::DirectDeclaratorParamTypeList> &paramTypeList)
         -> std::string_view {
       paramTypeList_ = paramTypeList.get();
       return std::visit(
           [&self](auto &&value) -> std::string_view { return self(value); },
           paramTypeList->getDirectDeclarator());
     },
     [](auto &&self, const box<Syntax::DirectDeclaratorAssignExpr> &assignExpr)
         -> std::string_view {
       return std::visit(
           [&self](auto &&value) -> std::string_view { return self(value); },
           assignExpr->getDirectDeclarator());
     },
     [](auto &&self, const box<Syntax::DirectDeclaratorAsterisk> &asterisk)
         -> std::string_view {
       return std::visit(
           [&self](auto &&value) -> std::string_view { return self(value); },
           asterisk->getDirectDeclarator());
     }};
 std::visit(YComb{visitor}, declarator.getDirectDeclarator());
 return paramTypeList_;
}

} // namespace lcc