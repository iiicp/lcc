set(LLVM_LINK_COMPONENTS
        ${LLVM_TARGETS_TO_BUILD}
        Analysis
        CodeGen
        Core
        IPO
        AggressiveInstCombine
        InstCombine
        Instrumentation
        MC
        MCParser
        ObjCARCOpts
        Option
        ScalarOpts
        Support
        TransformUtils
        Vectorize)

add_lcc_tool(lcc main.cpp)

target_link_libraries(lcc
        PRIVATE
        lccBasic
        lccCodeGen
        lccLexer
        lccParser
        lccSema
        lccSupport)