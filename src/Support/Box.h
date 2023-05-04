/***********************************
 * File:     Box.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/4/22
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_BOX_H
#define LCC_BOX_H

#include <memory>
namespace lcc {

template <typename T> class box {
  std::unique_ptr<T> impl_;

public:
  // Automatic construction from a `T`, not a `T*`
  box(T &&obj) : impl_(new T(std::move(obj))) {}

  box(box &&other) : impl_(std::move(other.impl_)) {}

  box &operator=(box &&other) {
    if (this != &other)
      impl_ = std::move(other.impl_);
    return *this;
  }

  // unique_ptr destroys `T` for us.
  ~box() = default;

  // Access propagates const ness.
  T &operator*() { return *impl_; }
  const T &operator*() const { return *impl_; }

  T *operator->() { return impl_.get(); }
  const T *operator->() const { return impl_.get(); }

  T *get() { return impl_.get(); }
  const T *get() const { return impl_.get(); }
};
} // namespace lcc

#endif // LCC_BOX_H
