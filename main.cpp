#include <iostream>
#include <fstream>
#include "Lexer.h"
#include "Parser.h"
#include "CodeGen.h"

int main() {
    std::cout << "Hello, lcc!" << std::endl;

    std::ifstream file("../tests/c/test.c");
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

    lcc::lexer::Lexer lex(reinterpret_cast<uint8_t *>(source.data()), source.size());
    auto tokens = lex.Tokenize();
    for (auto &tok : tokens) {
        std::cout << tok.GetLine() << ":" << tok.GetColumn() << " ";
        std::cout << tok.GetTokenSpelling() << std::endl;
    }

    lcc::parser::Parser parser(std::move(tokens));
    auto program = parser.ParseProgram();

    lcc::CodeGenContext context;
    lcc::codegen::CodeGen gen(std::move(program), context);
    return 0;
}
