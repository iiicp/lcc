/***********************************
 * File:     SemaSyntax.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/6
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SEMANODE_H
#define LCC_SEMANODE_H
#include <cstdint>
#include <memory>
#include <optional>
#include <variant>
#include <string>
#include "Type.h"
#include "Token.h"
namespace lcc::Sema {

class ReturnStmt;
class ExprStmt;
class IfStmt;
class WhileStmt;
class DoWhileStmt;
class ForStmt;
class BreakStmt;
class ContinueStmt;
class SwitchStmt;
class DefaultStmt;
class CaseStmt;
class GotoStmt;
class LabelStmt;
class BlockStmt;
class Declaration;

using Statement = std::variant<ReturnStmt, ExprStmt, IfStmt,
                               WhileStmt, DoWhileStmt, ForStmt, BreakStmt,
                               ContinueStmt, SwitchStmt, DefaultStmt,
                               CaseStmt, GotoStmt, LabelStmt, BlockStmt>;

class ReturnStmt final {

};

class ExprStmt final {

};

class IfStmt final {

};

class WhileStmt final {

};

class DoWhileStmt final {

};

class ForStmt final {

};

class BreakStmt final {

};

class ContinueStmt final {

};

class SwitchStmt final {

};

class DefaultStmt final {

};

class CaseStmt final {

};

class GotoStmt final {

};

class LabelStmt final {

};

class BlockStmt final {
private:
  std::vector<std::variant<Statement, Declaration>> blockItems_;
public:
  explicit BlockStmt(std::vector<std::variant<Statement, Declaration>> blockItems);
  [[nodiscard]] const std::vector<std::variant<Statement, Declaration>> &GetBlockItems() const {
    return blockItems_;
  }
};

enum class Linkage : uint8_t {
  Internal,
  External,
  None
};

enum class Lifetime : uint8_t {
  Automatic,
  Static,
  Register
};

class Declaration final {
private:
  Type type_;
  Linkage linkage_;
  Lifetime lifetime_;
  std::string name_;
  //std::optional<Initializer> initializer_;
public:
  explicit Declaration(Type type, Linkage linkage,
                       Lifetime lifetime, std::string name);

  [[nodiscard]] const Type &GetType() const {
    return type_;
  }
  [[nodiscard]] Linkage GetLinkage() const {
    return linkage_;
  }
  [[nodiscard]] Lifetime GetLifeTime() const {
    return lifetime_;
  }
  [[nodiscard]] const std::string& GetName() const {
    return name_;
  }
};

class FunctionDefinition final {
private:
  FunctionType type_;
  std::string name_;
  Linkage linkage_;
  BlockStmt blockStmt_;
public:
  explicit FunctionDefinition(FunctionType type, std::string name,
                              Linkage linkage, BlockStmt&& blockStmt);

  [[nodiscard]] const std::string& GetName() const {
    return name_;
  }
  [[nodiscard]] const FunctionType& GetType() const {
    return type_;
  }
  [[nodiscard]] Linkage GetLinkage() const {
    return linkage_;
  }
};

class TranslationUnit final {
public:
  using Variant = std::variant<FunctionDefinition,Declaration>;

private:
  std::vector<Variant> globals_;

public:
  explicit TranslationUnit(std::vector<Variant> globals);
  [[nodiscard]] const std::vector<Variant> &GetGlobals() const {
    return globals_;
  }
};
}

#endif // LCC_SEMANODE_H
