set(LLVM_LINK_COMPONENTS support)

add_lcc_library(lccLexer
        Lexer.cc

        LINK_LIBS
        lccBasic)