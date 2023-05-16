set(LLVM_LINK_COMPONENTS support)

add_lcc_library(lccParser
        Parser.cc

        LINK_LIBS
        lccBasic
        lccLexer)