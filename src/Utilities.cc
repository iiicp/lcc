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
//void LOGE(uint32_t row, uint32_t col, const std::string &msg) {
//  llvm::errs() << row << ":" << col << ", " << msg << "\n";
//  LCC_ASSERT(0);
//}
//
//void LOGE(const Token& tok, const std::string &msg) {
//  auto pair = tok.getLineAndColumn();
//  llvm::errs() << pair.first << ":" << pair.second << ", " << msg << "\n";
//  LCC_ASSERT(0);
//}


std::string_view getDeclaratorName(const Syntax::Declarator& declarator) {
  auto visitor = overload{
    [](auto&&, const Syntax::DirectDeclaratorIdent& name) -> std::string_view {
        return name.getIdent();
      },
    [](auto&& self, const Syntax::DirectDeclaratorParentheses & declarator) -> std::string_view {
        return std::visit([&self](auto &&value) -> std::string_view {
          return self(value);
        }, declarator.getDeclarator()->getDirectDeclarator());
      },
    [](auto&& self, const Syntax::DirectDeclaratorParamTypeList & paramTypeList) -> std::string_view {
      return std::visit([&self](auto &&value) -> std::string_view {
          return self(value);
        }, *paramTypeList.getDirectDeclarator());
      },
    [](auto&& self, const Syntax::DirectDeclaratorAssignExpr& assignExpr) -> std::string_view {
      return std::visit([&self](auto &&value) -> std::string_view { return self(value); },
      *assignExpr.getDirectDeclarator());
    },
    [](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) -> std::string_view {
      return std::visit([&self](auto &&value) -> std::string_view { return self(value); },
                          *asterisk.getDirectDeclarator());
    }};
  return std::visit(YComb{visitor}, declarator.getDirectDeclarator());
}

const Syntax::DirectDeclaratorParamTypeList *getFuncDeclarator
    (const Syntax::Declarator &declarator) {
 const Syntax::DirectDeclaratorParamTypeList * paramTypeList_ = nullptr;
  auto visitor = overload{
      [](auto&&, const Syntax::DirectDeclaratorIdent& name) -> std::string_view {
        return name.getIdent();
      },
      [](auto&& self, const Syntax::DirectDeclaratorParentheses & declarator) -> std::string_view {
        return std::visit([&self](auto &&value) -> std::string_view {
          return self(value);
        }, declarator.getDeclarator()->getDirectDeclarator());
      },
      [&paramTypeList_](auto&& self, const Syntax::DirectDeclaratorParamTypeList
                                        & paramTypeList) -> std::string_view {
        paramTypeList_ = &paramTypeList;
        return std::visit([&self](auto &&value) -> std::string_view {
          return self(value);
        }, *paramTypeList.getDirectDeclarator());
      },
      [](auto&& self, const Syntax::DirectDeclaratorAssignExpr& assignExpr) -> std::string_view {
        return std::visit([&self](auto &&value) -> std::string_view { return self(value); },
                          *assignExpr.getDirectDeclarator());
      },
     [](auto&& self, const Syntax::DirectDeclaratorAsterisk& asterisk) -> std::string_view {
       return std::visit([&self](auto &&value) -> std::string_view { return self(value); },
                         *asterisk.getDirectDeclarator());
     }};
  std::visit(YComb{visitor}, declarator.getDirectDeclarator());
  return paramTypeList_;
}

} // namespace lcc