set(LLVM_LINK_COMPONENTS support)

add_lcc_library(LccParser
        Parser.cc

        LINK_LIBS
        LccBasic
        LccLexer
        LccAST
        )
