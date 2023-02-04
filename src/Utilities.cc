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
#include <iostream>

namespace lcc {
void LOGE(uint32_t row, uint32_t col, const std::string &msg) {
  std::cerr << row << ":" << col << ", " << msg << std::endl;
  LCC_ASSERT(0);
}

std::string getDeclaratorName(const Syntax::Declarator& declarator) {
  auto visitor = overload{
    [](auto&&, const Syntax::DirectDeclaratorIdent& name) -> std::string {
        return name.getIdentifierLoc();
      },
    [](auto&& self, const Syntax::DirectDeclaratorParent& declarator) -> std::string {
        return std::visit([&self](auto &&value) -> std::string {
          return self(value);
        }, declarator.getDeclarator()->getDirectDeclarator());
      },
    [](auto&& self, const Syntax::DirectDeclaratorParentParamTypeList& paramTypeList) -> std::string {
      return std::visit([&self](auto &&value) -> std::string {
          return self(value);
        }, *paramTypeList.getDirectDeclarator());
      },
    [](auto&& self, const Syntax::DirectDeclaratorAssignExpr& assignExpr) -> std::string {
      return std::visit([&self](auto &&value) -> std::string { return self(value); },
      *assignExpr.getDirectDeclarator());
    }};
  return std::visit(YComb{visitor}, declarator.getDirectDeclarator());
}

const Syntax::DirectDeclaratorParentParamTypeList *getFuncDeclarator
    (const Syntax::Declarator &declarator) {
 const Syntax::DirectDeclaratorParentParamTypeList * paramTypeList_ = nullptr;
  auto visitor = overload{
      [](auto&&, const Syntax::DirectDeclaratorIdent& name) -> std::string {
        return name.getIdentifierLoc();
      },
      [](auto&& self, const Syntax::DirectDeclaratorParent& declarator) -> std::string {
        return std::visit([&self](auto &&value) -> std::string {
          return self(value);
        }, declarator.getDeclarator()->getDirectDeclarator());
      },
      [&paramTypeList_](auto&& self, const Syntax::DirectDeclaratorParentParamTypeList& paramTypeList) -> std::string {
        paramTypeList_ = &paramTypeList;
        return std::visit([&self](auto &&value) -> std::string {
          return self(value);
        }, *paramTypeList.getDirectDeclarator());
      },
      [](auto&& self, const Syntax::DirectDeclaratorAssignExpr& assignExpr) -> std::string {
        return std::visit([&self](auto &&value) -> std::string { return self(value); },
                          *assignExpr.getDirectDeclarator());
      }};
  std::visit(YComb{visitor}, declarator.getDirectDeclarator());
  return paramTypeList_;
}

} // namespace lcc