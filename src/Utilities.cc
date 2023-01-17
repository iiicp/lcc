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
std::string to_string(const llvm::APSInt& apsInt)
{
  llvm::SmallString<25> number;
  apsInt.toString(number);
  return std::string(number.begin(), number.end());
}

std::string to_string(const llvm::APFloat& apFloat)
{
  llvm::SmallString<25> number;
  apFloat.toString(number, 0, 3, false);
  return std::string(number.begin(), number.end());
}

void logErr(int32_t row, int32_t col, std::string msg) {
  std::cerr << row << ":" << col << ", " << msg << std::endl;
  LCC_ASSERT(0);
}
}