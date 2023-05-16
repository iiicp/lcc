/***********************************
 * File:     NodeIterator.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_ITERATOR_H
#define LCC_ITERATOR_H
#include "lcc/Basic/Util.h"
#include <iterator>
namespace lcc {
template <typename Element, typename Callable> class Iterator {
  const Element *curr_;
  Callable &func_;

public:
  using value_type = Element;
  using reference = Element &;
  using const_reference = const Element &;
  using pointer = Element *;
  using const_pointer = const Element *;
  using iterator_category = std::forward_iterator_tag;
  using difference_type = void;

  Iterator() = default;

  Iterator(const Element *curr, const Callable *func)
      : curr_(curr), func_(func) {}

  bool operator==(const Iterator &rhs) const { return curr_ == rhs.curr_; }

  bool operator!=(const Iterator &rhs) const { return !(*this == rhs); }

  const_reference operator*() const {
    LCC_ASSERT(curr_);
    return *curr_;
  }

  const_pointer operator->() const { return curr_; }

  // ++i;
  Iterator &operator++() {
    LCC_ASSERT(func_ && curr_);
    curr_ = (*func_)(*curr_);
    return *this;
  }

  /// i++;
  Iterator operator++(int) {
    auto before = *this;
    LCC_ASSERT(func_ && curr_);
    curr_ = (*func_)(*curr_);
    return before;
  }
};
} // namespace lcc

#endif // LCC_ITERATOR_H
