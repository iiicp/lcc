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
void LOGE(uint32_t row, uint32_t col, const std::string &msg);

//template <typename T, typename Variant>
//constexpr decltype(auto) get(Variant&& variant) noexcept
//{
//  LCC_ASSERT(!variant.valueless_by_exception() && std::holds_alternative<T>(variant));
//  auto* value = std::get_if<T>(&variant);
//  LCC_ASSERT(value);
//  if constexpr (std::is_lvalue_reference_v<Variant>)
//  {
//    return *value;
//  }
//  else
//  {
//    return std::move(*value);
//  }
//}

//template <std::size_t i, typename Variant>
//constexpr decltype(auto) get(Variant&& variant) noexcept
//{
//  LCC_ASSERT(!variant.valueless_by_exception() && variant.index() == i);
//  auto* value = std::get_if<i>(&variant);
//  LCC_ASSERT(value);
//  if constexpr (std::is_lvalue_reference_v<Variant>)
//  {
//    return *value;
//  }
//  else
//  {
//    return std::move(*value);
//  }
//}

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

//template <class T>
//struct type_identity
//{
//  using type = T;
//};
//
//namespace detail
//{
//template <class... Ts>
//struct overload : Ts...
//{
//  using Ts::operator()...;
//};
//template <class... Ts>
//overload(Ts...) -> overload<Ts...>;
//
//template <typename G, class Ret>
//struct YWithRet
//{
//  template <typename... X>
//  Ret operator()(X&&... x) const&
//  {
//    if constexpr (std::is_void_v<std::decay_t<Ret>>)
//    {
//      g(*this, std::forward<X>(x)...);
//    }
//    else
//    {
//      return g(*this, std::forward<X>(x)...);
//    }
//  }
//
//  G g;
//  type_identity<Ret> id;
//};

//template <typename G, class T>
//YWithRet(G, type_identity<T>) -> YWithRet<G, T>;
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant,
//                                   std::enable_if_t<(i == 0 || i > 12)>* = nullptr)
//{
//  if (variant.index() == i)
//  {
//    return std::forward<Callable>(callable)(lcc::get<i>(std::forward<Variant>(variant)));
//  }
//  if constexpr (i != 0)
//  {
//    return visitImpl<i - 1>(std::forward<Callable>(callable), std::forward<Variant>(variant));
//  }
//  else
//  {
//    LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 1)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 2)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 3)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 4)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 5)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 6)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 7)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 8)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  case 8: return std::forward<Callable>(callable)(lcc::get<8>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 9)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  case 8: return std::forward<Callable>(callable)(lcc::get<8>(std::forward<Variant>(variant)));
//  case 9: return std::forward<Callable>(callable)(lcc::get<9>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 10)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  case 8: return std::forward<Callable>(callable)(lcc::get<8>(std::forward<Variant>(variant)));
//  case 9: return std::forward<Callable>(callable)(lcc::get<9>(std::forward<Variant>(variant)));
//  case 10: return std::forward<Callable>(callable)(lcc::get<10>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 11)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  case 8: return std::forward<Callable>(callable)(lcc::get<8>(std::forward<Variant>(variant)));
//  case 9: return std::forward<Callable>(callable)(lcc::get<9>(std::forward<Variant>(variant)));
//  case 10: return std::forward<Callable>(callable)(lcc::get<10>(std::forward<Variant>(variant)));
//  case 11: return std::forward<Callable>(callable)(lcc::get<11>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <std::size_t i, class Callable, class Variant>
//constexpr decltype(auto) visitImpl(Callable&& callable, Variant&& variant, std::enable_if_t<(i == 12)>* = nullptr)
//{
//  switch (variant.index())
//  {
//  case 0: return std::forward<Callable>(callable)(lcc::get<0>(std::forward<Variant>(variant)));
//  case 1: return std::forward<Callable>(callable)(lcc::get<1>(std::forward<Variant>(variant)));
//  case 2: return std::forward<Callable>(callable)(lcc::get<2>(std::forward<Variant>(variant)));
//  case 3: return std::forward<Callable>(callable)(lcc::get<3>(std::forward<Variant>(variant)));
//  case 4: return std::forward<Callable>(callable)(lcc::get<4>(std::forward<Variant>(variant)));
//  case 5: return std::forward<Callable>(callable)(lcc::get<5>(std::forward<Variant>(variant)));
//  case 6: return std::forward<Callable>(callable)(lcc::get<6>(std::forward<Variant>(variant)));
//  case 7: return std::forward<Callable>(callable)(lcc::get<7>(std::forward<Variant>(variant)));
//  case 8: return std::forward<Callable>(callable)(lcc::get<8>(std::forward<Variant>(variant)));
//  case 9: return std::forward<Callable>(callable)(lcc::get<9>(std::forward<Variant>(variant)));
//  case 10: return std::forward<Callable>(callable)(lcc::get<10>(std::forward<Variant>(variant)));
//  case 11: return std::forward<Callable>(callable)(lcc::get<11>(std::forward<Variant>(variant)));
//  case 12: return std::forward<Callable>(callable)(lcc::get<12>(std::forward<Variant>(variant)));
//  default: LCC_UNREACHABLE;
//  }
//}
//
//template <class Callable, class Variant>
//constexpr decltype(auto) visit(Callable&& callable, Variant&& variant)
//{
//  LCC_ASSERT(!variant.valueless_by_exception());
//  return visitImpl<std::variant_size_v<std::decay_t<Variant>> - 1>(std::forward<Callable>(callable),
//                                                                   std::forward<Variant>(variant));
//}
//} // namespace detail
//
//template <typename Variant, typename... Matchers>
//constexpr decltype(auto) match(Variant&& variant, Matchers&&... matchers)
//{
//  LCC_ASSERT(!variant.valueless_by_exception());
//  return detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
//}
//
//template <class Ret, typename Variant, typename... Matchers>
//constexpr Ret match(Variant&& variant, Matchers&&... matchers)
//{
//  static_assert(std::is_void_v<Ret>, "Only explicit return type of void allowed");
//  detail::visit(detail::overload{std::forward<Matchers>(matchers)...}, std::forward<Variant>(variant));
//}
//
//template <typename Variant, typename... Matchers>
//constexpr decltype(auto) matchWithSelf(Variant&& variant, Matchers&&... matchers)
//{
//  return detail::visit(YComb{detail::overload{std::forward<Matchers>(matchers)...}}, std::forward<Variant>(variant));
//}
//
//template <class Ret, typename Variant, typename... Matchers>
//constexpr Ret matchWithSelf(Variant&& variant, Matchers&&... matchers)
//{
//  static_assert(std::is_void_v<Ret>, "Only explicit return type of void allowed");
//  detail::visit(detail::YWithRet{detail::overload{std::forward<Matchers>(matchers)...}, type_identity<Ret>{}},
//                std::forward<Variant>(variant));
//}

std::string getDeclaratorName(const Syntax::Declarator& declarator);

}
#endif // LCC_UTILITIES_H
