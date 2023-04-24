/***********************************
 * File:     ValueReset.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/4/24
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_VALUERESET_H
#define LCC_VALUERESET_H

#include <utility>
template <typename T> class ValueReset {
private:
  T &restoreValue_;
  T updateValue_;

public:
  ValueReset(T &restoreValue, T updateValue)
      : restoreValue_(restoreValue), updateValue_(updateValue) {
    std::swap(restoreValue_, updateValue_);
  }
  ~ValueReset() { std::swap(restoreValue_, updateValue_); }
};

#endif // LCC_VALUERESET_H
