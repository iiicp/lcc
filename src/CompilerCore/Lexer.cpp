/***********************************
* File:     Lexer.cpp
*
* Author:   蔡鹏
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/30
*
* Sign:     不管几岁，快乐万岁
***********************************/

#include "Lexer.h"
#include <sstream>

namespace lcc
{
    std::string Lexer::Token::getTokenSimpleSpelling(Lexer::TokenType tokenType) {
        switch (tokenType) {
#define PUNCTUATOR(X, Y) case X: return Y;
#define KEYWORD(X, Y) case kw_ ## X: return Y;
#include "TokenKinds.def"
            case identifier: return std::get<std::string>(getValue());
            case literal:
                return std::visit([](auto && value) -> std::string
                {
                    using T = std::decay_t<decltype(value)>;
                    if constexpr (std::is_same_v<std::string, T>)
                    {
                        /// 加上 constexpr 后, 编译器会在编译期只检测这个分支，此分支的返回值显然能赋值给std::string
                        /// 如果不加上constexpr, 则编译器会检测variant的所有值的是否都能匹配std::string
                        /// constexpr 编译器编译期检测，避免了其余分支的判断. c++ 特性
                        return value;
                    }
                    else if constexpr (!std::is_same_v<std::monostate, T>)
                    {
                        std::ostringstream ss;
                        ss << value;
                        return ss.str();
                    }
                    else {
                        return "";
                    }
                }, getValue());
            default: break;
        }
        return "";
    }

    std::vector<Lexer::Token> Lexer::tokenize(const std::string &source) {
        return std::vector<Token>();
    }
}