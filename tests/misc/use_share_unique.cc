/***********************************
 * File:     use_share_unique.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/11/6
 *
 * Sign:     enjoy life
 ***********************************/
#include <memory>
#include <iostream>
class A{
public:
  int a;
  A() {a = 10;}
  ~A() {}
};
int main() {
  std::unique_ptr<A> a = std::make_unique<A>();
  std::shared_ptr<A> b = std::make_shared<A>();
//  std::unique_ptr<A> c = std::move(a);
  std::cout << b->a << std::endl;
  return 0;
}