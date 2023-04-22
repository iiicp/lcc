/***********************************
 * File:     SemaSyntax.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/6
 *
 * Sign:     enjoy life
 ***********************************/
#include "SemaNode.h"
#include "Utilities.h"

namespace lcc::Sema {
BlockStmt::BlockStmt(
    std::vector<std::variant<Statement, Declaration>> blockItems)
    : blockItems_(std::move(blockItems)) {}

Declaration::Declaration(Type type, Linkage linkage,
                     Lifetime lifetime, std::string name)
    : type_(std::move(type)), linkage_(linkage), lifetime_(lifetime), name_(std::move(name)){}

FunctionDefinition::FunctionDefinition(
    FunctionType type, std::string name,Linkage linkage,
    BlockStmt &&blockStmt)
    : type_(std::move(type)), name_(std::move(name)),
      linkage_(linkage), blockStmt_(std::move(blockStmt)) {}

TranslationUnit::TranslationUnit(std::vector<Variant> globals)
    : globals_(std::move(globals)) {}
}
