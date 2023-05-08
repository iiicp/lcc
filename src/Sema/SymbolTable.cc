/***********************************
 * File:     SymbolTable.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "SymbolTable.h"
#include "Match.h"

namespace lcc::Sema {
bool SymbolTable::isCompleteType(const Type &type) {
  if (isVoid(type)) {
    return false;
  }
  if (std::holds_alternative<AbstractArrayType>(type.type())) {
    return false;
  }
  if (std::holds_alternative<EnumType>(type.type())) {
    auto &enumType = std::get<EnumType>(type.type());
    return getEnumDefinition(enumType.name(), enumType.index());
  }
  if (std::holds_alternative<StructType>(type.type())) {
    auto &structType = std::get<StructType>(type.type());
    return getStructDefinition(structType.name(), structType.index());
  }
  if (std::holds_alternative<UnionType>(type.type())) {
    auto &unionType = std::get<UnionType>(type.type());
    return getUnionDefinition(unionType.name(), unionType.index());
  }
  return true;
}

StructDefinition *SymbolTable::getStructDefinition(std::string_view name,
                                                   std::uint64_t scopeOrId,
                                                   std::uint64_t *idOut) {
  return const_cast<StructDefinition *>(
      const_cast<const SymbolTable *>(this)->getStructDefinition(
          name, scopeOrId, idOut));
}

const StructDefinition *
SymbolTable::getStructDefinition(std::string_view name, std::uint64_t scopeOrId,
                                 std::uint64_t *idOut) const {
  if (!(scopeOrId & IS_SCOPE)) {
    if (idOut) {
      *idOut = scopeOrId;
    }
    return &structDefinitions_[scopeOrId];
  }
  auto *type = lookupType<StructDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
  if (!type) {
    return nullptr;
  }
  if (idOut) {
    *idOut = static_cast<std::uint64_t>(*type);
  }
  return &structDefinitions_[static_cast<std::uint64_t>(*type)];
}

EnumDefinition *SymbolTable::getEnumDefinition(std::string_view name,
                                               std::uint64_t scopeOrId,
                                               std::uint64_t *idOut) {
  return const_cast<EnumDefinition *>(
      const_cast<const SymbolTable *>(this)->getEnumDefinition(name, scopeOrId,
                                                               idOut));
}

const EnumDefinition *
SymbolTable::getEnumDefinition(std::string_view name, std::uint64_t scopeOrId,
                               std::uint64_t *idOut) const {
  if (!(scopeOrId & IS_SCOPE)) {
    if (idOut) {
      *idOut = scopeOrId;
    }
    return &enumDefinitions_[scopeOrId];
  }
  auto *type = lookupType<EnumDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
  if (!type) {
    return nullptr;
  }
  if (idOut) {
    *idOut = static_cast<std::uint64_t>(*type);
  }
  return &enumDefinitions_[static_cast<std::uint64_t>(*type)];
}

UnionDefinition *SymbolTable::getUnionDefinition(std::string_view name,
                                                 std::uint64_t scopeOrId,
                                                 std::uint64_t *idOut) {
  return const_cast<UnionDefinition *>(
      const_cast<const SymbolTable *>(this)->getUnionDefinition(name, scopeOrId,
                                                                idOut));
}

const UnionDefinition *
SymbolTable::getUnionDefinition(std::string_view name, std::uint64_t scopeOrId,
                                std::uint64_t *idOut) const {
  if (!(scopeOrId & IS_SCOPE)) {
    if (idOut) {
      *idOut = scopeOrId;
    }
    return &unionDefinitions_[scopeOrId];
  }
  auto *type = lookupType<UnionDefTag>(name, scopeOrId & SCOPE_OR_ID_MASK);
  if (!type) {
    return nullptr;
  }
  if (idOut) {
    *idOut = static_cast<std::uint64_t>(*type);
  }
  return &unionDefinitions_[static_cast<std::uint64_t>(*type)];
}

bool SymbolTable::isVoid(const Type &type) {
  auto *primitive = std::get_if<PrimitiveType>(&type.type());
  if (!primitive) {
    return false;
  }
  return primitive->kind() == PrimitiveType::Kind::Void;
}

bool SymbolTable::isArray(const Type &type) {
  return std::holds_alternative<ArrayType>(type.type()) ||
         std::holds_alternative<AbstractArrayType>(type.type());
}

const Type &SymbolTable::getArrayElementType(const Type &type) {
  return match(
      type.type(), [](const auto &) -> const Type & { LCC_UNREACHABLE; },
      [](const ArrayType &arrayType) -> const Type & {
        return *arrayType.type();
      },
      [](const AbstractArrayType &abstractArrayType) -> const Type & {
        return *abstractArrayType.type();
      });
}

bool SymbolTable::isInteger(const Type &type) {
  return std::holds_alternative<PrimitiveType>(type.type()) &&
         !std::get<PrimitiveType>(type.type()).isFloatingPoint() &&
         std::get<PrimitiveType>(type.type()).sizeOf() != 0;
}

bool SymbolTable::isArithmetic(const Type &type) {
  return std::holds_alternative<PrimitiveType>(type.type()) &&
         std::get<PrimitiveType>(type.type()).sizeOf() != 0;
}

bool SymbolTable::isScalar(const Type &type) {
  return isArithmetic(type) || std::holds_alternative<PointerType>(type.type());
}

bool SymbolTable::isRecord(const Type &type) {
  return std::holds_alternative<StructType>(type.type()) ||
         std::holds_alternative<UnionType>(type.type()) ||
         std::holds_alternative<AnonymousStructType>(type.type()) ||
         std::holds_alternative<AnonymousUnionType>(type.type());
}

bool SymbolTable::isBool(const Type &type) {
  auto *primitive = std::get_if<PrimitiveType>(&type.type());
  if (!primitive) {
    return false;
  }
  return primitive->kind() == PrimitiveType::Kind::Bool;
}

bool SymbolTable::isCharType(const Type &type) {
  auto *primitive = std::get_if<PrimitiveType>(&type.type());
  if (!primitive) {
    return false;
  }
  return primitive->kind() == PrimitiveType::Kind::Char ||
         primitive->kind() == PrimitiveType::Kind::UnSignedChar;
}

bool SymbolTable::isAggregate(const Type &type) {
  return isRecord(type) || isArray(type);
}

} // namespace lcc::Sema
