set(LLVM_LINK_COMPONENTS support)

add_lcc_library(LccLexer
        Lexer.cc
        Token.cc

        LINK_LIBS
        LccBasic
        )
