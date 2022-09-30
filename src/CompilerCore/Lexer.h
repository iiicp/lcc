/***********************************
* File:     Lexer.h
*
* Author:   蔡鹏
*
* Email:    iiicp@outlook.com
*
* Date:     2022/9/30
*
* Sign:     不管几岁，快乐万岁
***********************************/

#ifndef LCC_LEXER_H
#define LCC_LEXER_H
#include <cstdint>
#include <variant>
#include <string>
#include <vector>

namespace lcc {
    namespace Lexer {
        enum TokenType
        {
#define TOK(X) X,
#include "TokenKinds.def"
        };

        class Token {
            using variant = std::variant<std::monostate, int32_t, uint32_t, int64_t, uint64_t, float, double, std::string>;
            uint64_t m_line;
            uint64_t m_column;
            TokenType m_tokenType;
            variant m_value;
            friend std::vector<Token> tokenize(const std::string &source);
        public:
            explicit Token(uint64_t line, uint64_t column, TokenType tokenType) noexcept
                    : m_line(line), m_column(column), m_tokenType(tokenType) {}

            template<class T>
            Token(uint64_t line, uint64_t column, TokenType tokenType, T &&value)
                    : m_line(line), m_column(column), m_tokenType(tokenType), m_value(std::forward<T>(value)) {}

            TokenType getTokenType() const {
                return m_tokenType;
            }

            const variant &getValue() const {
                return m_value;
            }

            uint64_t getLine() const {
                return m_line;
            }

            uint64_t getColumn() const {
                return m_column;
            }

            std::string getTokenSimpleSpelling(TokenType tokenType);
        };
        std::vector<Token> tokenize(const std::string &source);
    };
}


#endif //LCC_LEXER_H
