#include <iostream>
#include "Lexer.h"

int main() {
    std::cout << "Hello, lcc!" << std::endl;
    std::string str;
    lcc::Lexer::tokenize(str);
    return 0;
}
