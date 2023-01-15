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

#include <cstdio>
#include <cassert>
#include <cstdlib>
#include <string>

#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/APFloat.h"

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
template <class T, class = void>
struct ToString : std::false_type
{
};

template <class T>
struct ToString<T, std::void_t<decltype(std::to_string(std::declval<T>()))>> : std::true_type
{
};

template <class T, class = std::enable_if_t<ToString<T>{}>>
std::string to_string(T value)
{
  return std::to_string(value);
}

std::string to_string(const llvm::APSInt& apsInt);

std::string to_string(const llvm::APFloat& apFloat);

inline std::string to_string(std::string_view stringView)
{
  return std::string(stringView.begin(), stringView.end());
}
}

#endif // LCC_UTILITIES_H
