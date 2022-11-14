set(LLVM_LINK_COMPONENTS support)

add_lcc_library(LccAST
        Syntax.cc

        LINK_LIBS
        LccBasic
        )
