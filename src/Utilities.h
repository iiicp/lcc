/***********************************
 * File:     Utilities.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/1/14
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_UTILITIES_H
#define LCC_UTILITIES_H

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <string>
#include "Syntax.h"

#define LCC_ASSERT(...)                                                        \
  do {                                                                         \
    if (!(__VA_ARGS__)) {                                                      \
      fprintf(stderr, __FILE__ ":%d:" #__VA_ARGS__ "\n", __LINE__);            \
      std::abort();                                                            \
    }                                                                          \
  } while (0)

#define LCC_UNREACHABLE                                                        \
  do {                                                                         \
    std::abort();                                                              \
  } while (0)

namespace lcc {
//void LOGE(uint32_t row, uint32_t col, const std::string &msg);
//void LOGE(const Token& tok, const std::string &msg);

template <class... Ts>
struct overload : Ts...
{
  using Ts::operator()...;
};
template <class... Ts>
overload(Ts...) -> overload<Ts...>;

template <typename G>
struct YComb
{
  template <typename... X>
  constexpr decltype(auto) operator()(X&&... x) const
  {
    return g(*this, std::forward<X>(x)...);
  }

  template <typename... X>
  constexpr decltype(auto) operator()(X&&... x)
  {
    return g(*this, std::forward<X>(x)...);
  }

  G g;
};

template <typename G>
YComb(G) -> YComb<G>;

std::string_view getDeclaratorName(const Syntax::Declarator& declarator);
const Syntax::DirectDeclaratorParamTypeList *getFuncDeclarator(const Syntax::Declarator &declarator);
}
#endif // LCC_UTILITIES_H
