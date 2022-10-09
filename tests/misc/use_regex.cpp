/***********************************
* File:     use_regex.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/10/2
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include <regex>
#include <string>
#include <iostream>
int main() {
    std::string str = "int main(){int a; int b; float c; c = 1.4; b = 2; return 0;}";
    std::regex pattern("[a-zA-Z_]\\w*");
    std::smatch match;

    std::cout << "---------------regex_search---------------" << std::endl;
    /// (单次匹配) 可以匹配某个子串
    bool res = std::regex_search(str, match, pattern);
    std::cout << "regex_search find : " << res << std::endl;
    std::cout << "regex_search smatch size: " << match.size() << std::endl;
    for (auto &r : match) {
        std::cout << r.length() << ", " << r.str() << std::endl;
    }
    std::cout << "---------------regex_match---------------" << std::endl;

    // (单次匹配) 匹配整个串，若整串不满足，则匹配失败 （并非匹配子串）
    std::smatch st;
    bool res2 = std::regex_match(str, st, pattern);
    std::cout << res2 << std::endl;
    for (auto &r : st) {
        std::cout << r << std::endl;
    }

    std::cout << "------------------------------" << std::endl;
    return 0;
}