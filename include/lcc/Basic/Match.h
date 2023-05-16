/***********************************
 * File:     Match.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/4/26
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_MATCH_H
#define LCC_MATCH_H
#include "lcc/Basic/Util.h"
#include <cassert>
#include <variant>
namespace lcc {
template <class... Ts> struct overload : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overload(Ts...) -> overload<Ts...>;

template <typename G> struct YComb {
  template <typename... X> constexpr decltype(auto) operator()(X &&...x) const {
    return g(*this, std::forward<X>(x)...);
  }

  template <typename... X> constexpr decltype(auto) operator()(X &&...x) {
    return g(*this, std::forward<X>(x)...);
  }

  G g;
};

template <typename G> YComb(G) -> YComb<G>;

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto)
visit_imp(Callable &&callable, Variant &&variant,
          std::enable_if_t<i == 0 || (i > 8)> * = nullptr) {
  if (variant.index() == i) {
    return std::forward<Callable>(callable)(
        std::get<i>(std::forward<Variant>(variant)));
  }
  if constexpr (i > 0) {
    return visit_imp<i - 1>(std::forward<Callable>(callable),
                            std::forward<Variant>(variant));
  } else {
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 1> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 2> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 3> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 4> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  case 4:
    return std::forward<Callable>(callable)(
        std::get<4>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 5> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  case 4:
    return std::forward<Callable>(callable)(
        std::get<4>(std::forward<Variant>(variant)));
  case 5:
    return std::forward<Callable>(callable)(
        std::get<5>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 6> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  case 4:
    return std::forward<Callable>(callable)(
        std::get<4>(std::forward<Variant>(variant)));
  case 5:
    return std::forward<Callable>(callable)(
        std::get<5>(std::forward<Variant>(variant)));
  case 6:
    return std::forward<Callable>(callable)(
        std::get<6>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 7> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  case 4:
    return std::forward<Callable>(callable)(
        std::get<4>(std::forward<Variant>(variant)));
  case 5:
    return std::forward<Callable>(callable)(
        std::get<5>(std::forward<Variant>(variant)));
  case 6:
    return std::forward<Callable>(callable)(
        std::get<6>(std::forward<Variant>(variant)));
  case 7:
    return std::forward<Callable>(callable)(
        std::get<7>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <size_t i, typename Callable, typename Variant>
constexpr decltype(auto) visit_imp(Callable &&callable, Variant &&variant,
                                   std::enable_if_t<i == 8> * = nullptr) {
  switch (variant.index()) {
  case 0:
    return std::forward<Callable>(callable)(
        std::get<0>(std::forward<Variant>(variant)));
  case 1:
    return std::forward<Callable>(callable)(
        std::get<1>(std::forward<Variant>(variant)));
  case 2:
    return std::forward<Callable>(callable)(
        std::get<2>(std::forward<Variant>(variant)));
  case 3:
    return std::forward<Callable>(callable)(
        std::get<3>(std::forward<Variant>(variant)));
  case 4:
    return std::forward<Callable>(callable)(
        std::get<4>(std::forward<Variant>(variant)));
  case 5:
    return std::forward<Callable>(callable)(
        std::get<5>(std::forward<Variant>(variant)));
  case 6:
    return std::forward<Callable>(callable)(
        std::get<6>(std::forward<Variant>(variant)));
  case 7:
    return std::forward<Callable>(callable)(
        std::get<7>(std::forward<Variant>(variant)));
  case 8:
    return std::forward<Callable>(callable)(
        std::get<8>(std::forward<Variant>(variant)));
  default:
    LCC_UNREACHABLE;
  }
}

template <typename Callable, typename Variant>
constexpr decltype(auto) visit(Callable &&callable, Variant &&variant) {
  return visit_imp<std::variant_size_v<std::decay_t<Variant>> - 1>(
      std::forward<Callable>(callable), std::forward<Variant>(variant));
}

template <typename Variant, typename... Matchers>
constexpr decltype(auto) match(Variant &&variant, Matchers &&...matchers) {
  return visit(overload{std::forward<Matchers>(matchers)...},
               std::forward<Variant>(variant));
}

template <typename Variant, typename... Matchers>
constexpr decltype(auto) match_with_self(Variant &&variant,
                                         Matchers &&...matchers) {
  return visit(YComb{overload{std::forward<Matchers>(matchers)...}},
               std::forward<Variant>(variant));
}
} // namespace lcc

#endif // LCC_MATCH_H
