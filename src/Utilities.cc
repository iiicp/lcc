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
#include "llvm/ADT/SmallString.h"
#include <iostream>

namespace lcc {
void LOGE(uint32_t row, uint32_t col, const std::string &msg) {
  std::cerr << row << ":" << col << ", " << msg << std::endl;
  LCC_ASSERT(0);
}
} // namespace lcc