set(LLVM_LINK_COMPONENTS support)

add_lcc_library(lccCodeGen
        CodeGen.cc

        LINK_LIBS
        lccSema)