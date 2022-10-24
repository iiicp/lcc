/***********************************
 * File:     use_transform.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/23
 *
 * Sign:     enjoy life
 ***********************************/
#include <vector>
#include <iostream>
#include <algorithm>
int main() {
  std::vector<std::pair<int, int>> vec;
  vec.emplace_back(std::pair<int,int>(1,1001));
  vec.emplace_back(std::pair<int,int>(2,1002));
  vec.emplace_back(std::pair<int,int>(3,1003));
  vec.emplace_back(std::pair<int,int>(4,1004));
  for (auto &p : vec) {
    std::cout << p.first << ", " << p.second << std::endl;
  }

  std::vector<int> pp;
  std::transform(vec.begin(), vec.end(), std::back_inserter(pp), [](const auto& pair) {
    return pair.first;
  });

  std::cout << "new line..." << std::endl;
  for (auto &p : pp) {
    std::cout << p << std::endl;
  }
  return 0;
}