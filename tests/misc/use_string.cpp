/***********************************
* File:     use_string.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/10/3
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include <string>
#include <iostream>

int main() {
    const char *p = "hello,lcc";
    /// [first, last)
    std::string sub{p+1, p+2};
    std::cout << sub << std::endl;

    int index = 0;
    const char *q = p;
    while (*q) {
        std::cout << index++ << "," << *q++ << std::endl;
    }
    assert(*q == 0);
    if (*q == 0) {
        std::cout << "last..." << std::endl;
    }
    std::cout <<  index++ << "," << *q << std::endl;

    std::string tmp;
    tmp.reserve(512);
    std::cout << tmp.capacity() << std::endl;
    std::cout << tmp.size() << std::endl;
    tmp.push_back('a');
    tmp.push_back('b');
    std::cout << tmp.capacity() << std::endl;
    std::cout << tmp.size() << std::endl;
    std::cout << tmp << std::endl;
    tmp.shrink_to_fit();
    std::cout << tmp.capacity() << std::endl;
    std::cout << tmp.size() << std::endl;
    std::cout << tmp << std::endl;

    std::string t;
    /// default capacity is 22
    std::cout << t.capacity() << std::endl;
    std::string w("1e+10");
    std::cout << w.size() << std::endl;
    std::cout << w.length() << std::endl;
    return 0;
}