set(LLVM_LINK_COMPONENTS support)

add_lcc_library(lccSupport
        DumpTool.cc

        LINK_LIBS
        lccParser)