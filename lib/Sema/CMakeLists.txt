set(LLVM_LINK_COMPONENTS support)

add_lcc_library(lccSema
        Sema.cc
        Scope.cc
        Type.cc

        LINK_LIBS
        lccBasic)