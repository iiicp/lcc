set(LLVM_LINK_COMPONENTS support)

add_lcc_library(LccCodeGen
        CodeGen.cc

        LINK_LIBS
        LccParser
        )
