#include <iostream>
#include <fstream>
#include "Lex/Lexer.h"

using namespace lcc;

int main() {
    std::cout << "Hello, lcc!" << std::endl;

    std::ifstream file("../tests/c/fib.c");
    if(!file.is_open())
    {
        std::cerr<<"Could not open source file";
        return -1;
    }
    std::string source;
    file.seekg(0,std::ios_base::end);
    std::size_t pos = file.tellg();
    source.resize(pos);
    file.seekg(0,std::ios_base::beg);
    file.read(source.data(),source.size());

    auto tokens = lcc::lexer::Tokenize(source);
    for (auto &tok : tokens) {
        std::cout << tok.GetLine() << ":" << tok.GetColumn() << " ";
        std::cout << tok.GetTokenSimpleSpelling(tok.GetTokenType()) << std::endl;
    }

    return 0;
}
