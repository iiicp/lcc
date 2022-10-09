/***********************************
* File:     use_variant.cpp
*
* Author:   caipeng
*
* Email:    iiicp@outlook.com
*
* Date:     2022/10/2
*
* Sign:     不管几岁，快乐万岁
***********************************/
#include <iostream>
#include <variant>
#include <string>
#include <sstream>

struct MyVisit {
    void operator()(int16_t val) {
        std::cout << val << std::endl;
    }
    void operator()(int32_t val) {
        std::cout << val << std::endl;
    }
    void operator()(const std::string &s) {
        std::cout << s << std::endl;
    }
    void operator()(float val) {
        std::cout << val << std::endl;
    }
};

int main() {
    std::variant<int16_t, int32_t, float, std::string> variant;
    variant = 123;
    /// 采用通用类型参数 auto &
    std::visit([](auto &val){
        std::cout << val << std::endl;
    }, variant);

    /// 万能引用类型
    std::visit([](auto &&val) {
        std::cout << val << std::endl;
    }, variant);

    /// 必须要所有类型匹配
    std::visit(MyVisit(), variant);

    /// visitor也可以带有返回值
    variant = "lcc";
    auto res = std::visit([](auto &&val)->std::string {
        using T = std::decay_t<decltype(val)>;
        std::cout << typeid(decltype(val)).name() << std::endl;
        std::cout << typeid(T).name() << std::endl;
        /// 加上 constexpr 后, 编译器会在编译期只检测这个分支，此分支的返回值显然能赋值给std::string
        /// 如果不加上constexpr, 则编译器会检测variant的所有值的是否都能匹配std::string
        /// constexpr 编译器编译期检测，避免了其余分支的判断. c++ 特性
        if constexpr (!std::is_same_v<T, std::string>) {
            std::ostringstream ost;
            ost << val;
            return ost.str();
        }else {
            return val;
        }
    }, variant);
    std::cout << res << std::endl;

    std::variant<std::monostate, int, float> v2;
    v2 = 3.4f;
    v2 = std::monostate();
    const auto &ret = std::visit([](auto &&val)->std::string {
        using T = std::decay_t<decltype(val)>;
        if constexpr (std::is_same_v<T, std::string>) {
            return val;
        }else if constexpr (!std::is_same_v<T, std::monostate>) {
            std::ostringstream otr;
            otr << val;
            return otr.str();
        }else {
            return "monoState";
        }
    }, v2);
    std::cout << ret << std::endl;
    return 0;
}