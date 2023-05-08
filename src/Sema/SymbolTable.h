/***********************************
 * File:     SymbolTable.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SYMBOLTABLE_H
#define LCC_SYMBOLTABLE_H

#include "SemaNode.h"
#include "Type.h"
#include <unordered_map>

namespace lcc::Sema {
class SymbolTable {
public:
  enum class StructDefTag : size_t {

  };

  enum class UnionDefTag : size_t {

  };

  enum class EnumDefTag : size_t {

  };

  struct DeclarationInScope {
    std::string_view name;
    using Variant = std::variant<Declaration *, FunctionDefinition *>;
    Variant declared;
  };

  struct TagTypeInScope {
    struct StructDecl {};
    struct UnionDecl {};
    using Variant = std::variant<StructDecl, UnionDecl, StructDefTag,
                                 UnionDefTag, EnumDefTag>;
    Variant tagType;
  };

  struct Scope {
    int64_t previousScope;
    std::vector<DeclarationInScope> declarations;
    std::unordered_map<std::string_view, TagTypeInScope> tagTypes;
  };

  std::vector<Scope> scopes_ = {Scope{-1, {}, {}}};
  std::vector<StructDefinition> structDefinitions_;
  std::vector<UnionDefinition> unionDefinitions_;
  std::vector<EnumDefinition> enumDefinitions_;

  bool isCompleteType(const Type &type);

  constexpr static std::uint64_t IS_SCOPE = 1ull << 63;
  constexpr static std::uint64_t SCOPE_OR_ID_MASK = ~(1ull << 63);

  template <class T>
  const T *lookupType(std::string_view name, std::int64_t scope) const {
    auto curr = scope;
    while (curr >= 0) {
      auto result = scopes_[curr].tagTypes.find(name);
      if (result != scopes_[curr].tagTypes.end()) {
        if (auto *ptr = std::get_if<T>(&result->second.tagType)) {
          return ptr;
        }
      }
      curr = scopes_[curr].previousScope;
    }
    return nullptr;
  }

  StructDefinition *getStructDefinition(std::string_view name,
                                        std::uint64_t scopeOrId,
                                        std::uint64_t *idOut = nullptr);
  const StructDefinition *
  getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                      std::uint64_t *idOut = nullptr) const;

  EnumDefinition *getEnumDefinition(std::string_view name,
                                    std::uint64_t scopeOrId,
                                    std::uint64_t *idOut = nullptr);
  const EnumDefinition *getEnumDefinition(std::string_view name,
                                          std::uint64_t scopeOrId,
                                          std::uint64_t *idOut = nullptr) const;

  UnionDefinition *getUnionDefinition(std::string_view name,
                                      std::uint64_t scopeOrId,
                                      std::uint64_t *idOut = nullptr);
  const UnionDefinition *
  getUnionDefinition(std::string_view name, std::uint64_t scopeOrId,
                     std::uint64_t *idOut = nullptr) const;

  const std::vector<Scope> &getScopes() const { return scopes_; }

  bool isVoid(const Type &type);

  bool isArray(const Type &type);

  const Type &getArrayElementType(const Type &type);

  Type adjustParameterType(const Type &type);

  bool isInteger(const Type &type);

  bool isArithmetic(const Type &type);

  bool isScalar(const Type &type);

  bool isRecord(const Type &type);

  bool isBool(const Type &type);

  bool isCharType(const Type &type);

  bool isAggregate(const Type &type);
};
} // namespace lcc::Sema
#endif // LCC_SYMBOLTABLE_H
