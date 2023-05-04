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
#ifndef LCC_UTIL_H
#define LCC_UTIL_H

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <string>

#define MV_(obj) std::move(obj)

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

#ifdef __clang__
#define LCC_NON_NULL _Nonnull
#define LCC_NULLABLE _Nullable
#else
#define LCC_NON_NULL
#define LCC_NULLABLE
#endif

#endif // LCC_UTIL_H
