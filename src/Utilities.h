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
void LOGE(uint32_t row, uint32_t col, const std::string &msg);
}
#endif // LCC_UTILITIES_H
