/***********************************
* File:     use_char.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/10/3
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include <iostream>
int main() {
    /// "
    char ch = '\"';
    std::cout << ch << std::endl;
    ch = '\\"';
    std::cout << ch << std::endl;
    ch = '\\\"';
    std::cout << ch << std::endl;
    ch = '\\\\"';
    std::cout << ch << std::endl;

    /// '
    ch = '\'';
    std::cout << ch << std::endl;
    ch = '\\\'';
    std::cout << ch << std::endl;
    ch = '\\\\\'';
    std::cout << ch << std::endl;

    /// \

    ch = '\\';
    std::cout << ch << std::endl;
    ch = '\\\\';
    std::cout << ch << std::endl;
    ch = '\\\\\\';
    std::cout << ch << std::endl;

    ch = '\\?';
    std::cout << ch << std::endl;
    return 0;
}