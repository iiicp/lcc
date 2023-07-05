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

// Uses compiler specific extensions if possible.
#ifdef __GNUC__ // GCC, Clang, ICC

#define unreachable() (__builtin_unreachable())

#elif defined(_MSC_VER) // MSVC

#define unreachable() (__assume(false))

#else
// Even if no extension is used, undefined behavior is still raised by
// the empty function body and the noreturn attribute.
// The external definition of unreachable_impl must be emitted in a separated TU
// due to the rule for inline functions in C.

[[noreturn]] inline void unreachable_impl() {}
#define unreachable() (unreachable_impl())
#endif

#define LCC_UNREACHABLE                                                        \
  do {                                                                         \
    unreachable();                                                             \
  } while (0)

#ifdef __clang__
#define LCC_NON_NULL _Nonnull
#define LCC_NULLABLE _Nullable
#else
#define LCC_NON_NULL
#define LCC_NULLABLE
#endif

#define DECL_GETTER(type, name)                                                \
  type name() const { return this->name##_; }

#endif // LCC_UTIL_H
