/***********************************
 * File:     Type.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_TYPE_H
#define LCC_TYPE_H
#include "Util.h"
#include <cstdint>
#include <memory>
#include <string_view>
#include <variant>
#include <vector>

namespace lcc {

class Type;

class PrimitiveType final {
private:
  std::uint8_t sizeOf_;
  std::uint8_t alignOf_;
  bool isFloatingPoint_;
  bool isSigned_;

public:
  enum Kind : std::uint8_t {
    Char,
    UnSignedChar,
    Bool,
    Short,
    UnSignedShort,
    Int,
    UnSignedInt,
    Long,
    UnSignedLong,
    LongLong,
    UnSignedLongLong,
    Float,
    Double,
    LongDouble,
    Void
  };

private:
  Kind kind_;

  PrimitiveType(Kind kind);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile, Kind kind);
  DECL_GETTER(Kind, kind);
  DECL_GETTER(uint64_t, sizeOf);
  DECL_GETTER(uint64_t, alignOf);
  DECL_GETTER(bool, isFloatingPoint);
  DECL_GETTER(bool, isSigned);

  bool operator==(const PrimitiveType &rhs) const;
  bool operator!=(const PrimitiveType &rhs) const;
};

class ArrayType final {
private:
  std::shared_ptr<Type> type_;
  size_t size_;
  bool isRestricted_;
  bool isStatic_;
  ArrayType(bool isRestricted, bool isStatic, std::shared_ptr<Type> type,
            size_t size);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      bool isRestricted, bool isStatic,
                                      std::shared_ptr<Type> elemType,
                                      size_t size);

  DECL_GETTER(std::shared_ptr<Type>, type);
  DECL_GETTER(size_t, size);
  DECL_GETTER(bool, isRestricted);
  DECL_GETTER(bool, isStatic);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const ArrayType &rhs) const;
  bool operator!=(const ArrayType &rhs) const;
};

class AbstractArrayType final {
private:
  std::shared_ptr<Type> type_;
  bool isRestricted_;
  AbstractArrayType(bool isRestricted, std::shared_ptr<Type> type);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      bool isRestricted,
                                      std::shared_ptr<Type> elemType);
  DECL_GETTER(std::shared_ptr<Type>, type);
  DECL_GETTER(bool, isRestricted);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const AbstractArrayType &rhs) const;
  bool operator!=(const AbstractArrayType &rhs) const;
};

class PointerType final {
private:
  std::shared_ptr<Type> elementType_;
  bool restricted_{false};
  PointerType(bool isRestricted, std::shared_ptr<Type> elementType);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      bool restricted,
                                      std::shared_ptr<Type> elementType);
  DECL_GETTER(std::shared_ptr<Type>, elementType);
  DECL_GETTER(bool, restricted);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const PointerType &rhs) const;
  bool operator!=(const PointerType &rhs) const;
};

class FunctionType final {
private:
  std::shared_ptr<Type> returnType_;
  std::vector<std::pair<std::shared_ptr<Type>, std::string_view>> arguments_;
  bool lastIsVararg_;

public:
  using Argument = std::pair<std::shared_ptr<Type>, std::string_view>;

private:
  FunctionType(std::shared_ptr<Type> returnType,
               std::vector<Argument> arguments, bool lastIsVararg);

public:
  static std::shared_ptr<Type> create(std::shared_ptr<Type> returnType,
                                      std::vector<Argument> &&arguments,
                                      bool lastIsVararg);

  DECL_GETTER(std::shared_ptr<Type>, returnType);
  DECL_GETTER(bool, lastIsVararg);
  DECL_GETTER(const std::vector<Argument> &, arguments);

  uint64_t sizeOf() const { LCC_UNREACHABLE; }
  uint64_t alignOf() const { LCC_UNREACHABLE; }

  [[nodiscard]] bool operator==(const FunctionType &rhs) const;
  [[nodiscard]] bool operator!=(const FunctionType &rhs) const;
};

class StructType final {
private:
  std::string_view name_;
  uint64_t index_;
  StructType(std::string_view name, uint64_t index);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::string_view name, uint64_t index);

  DECL_GETTER(std::string_view, name);
  DECL_GETTER(uint64_t, index);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const StructType &rhs) const;
  bool operator!=(const StructType &rhs) const;
};

class UnionType final {
private:
  std::string_view name_;
  uint64_t index_;
  UnionType(std::string_view name, uint64_t index);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::string_view name, uint64_t index);

  DECL_GETTER(std::string_view, name);
  DECL_GETTER(uint64_t, index);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const UnionType &rhs) const;
  bool operator!=(const UnionType &rhs) const;
};

class EnumType final {
private:
  std::string_view name_;
  uint64_t index_;
  EnumType(std::string_view name, uint64_t index);

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::string_view name, uint64_t index);

  DECL_GETTER(std::string_view, name);
  DECL_GETTER(uint64_t, index);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const EnumType &rhs) const;
  bool operator!=(const EnumType &rhs) const;
};

struct Field {
  std::shared_ptr<Type> type;
  std::string_view name;
  std::optional<std::pair<uint32_t, uint32_t>> bitFieldBounds;
  size_t layoutIndex;

  bool operator==(const Field &rhs) const {
    return std::tie(type, name, bitFieldBounds) ==
           std::tie(rhs.type, rhs.name, rhs.bitFieldBounds);
  }
  bool operator!=(const Field &rhs) const { return !(rhs == *this); }
};

class AnonymousStructType final {
private:
  uint32_t id_;
  std::vector<Field> fields_;
  uint64_t sizeOf_;
  uint64_t alignOf_;
  AnonymousStructType(std::uint64_t id, std::vector<Field> &&fields,
                      std::uint32_t sizeOf, std::uint32_t alignOf)
      : id_(id), fields_(std::move(fields)), sizeOf_(sizeOf),
        alignOf_(alignOf) {}

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::uint64_t id,
                                      std::vector<Field> fields,
                                      std::uint32_t sizeOf,
                                      std::uint32_t alignOf);

  DECL_GETTER(uint32_t, id);
  DECL_GETTER(std::vector<Field>, fields);
  DECL_GETTER(uint64_t, sizeOf);
  DECL_GETTER(uint64_t, alignOf);

  bool operator==(const AnonymousStructType &rhs) const {
    return id_ == rhs.id_;
  }
  bool operator!=(const AnonymousStructType &rhs) const {
    return !(rhs == *this);
  }
};

class AnonymousUnionType final {
private:
  uint32_t id_;
  std::vector<Field> fields_;
  uint64_t sizeOf_;
  uint64_t alignOf_;
  AnonymousUnionType(std::uint64_t id, std::vector<Field> &&fields,
                     std::uint32_t sizeOf, std::uint32_t alignOf)
      : id_(id), fields_(std::move(fields)), sizeOf_(sizeOf),
        alignOf_(alignOf) {}

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::uint64_t id,
                                      std::vector<Field> fields,
                                      std::uint32_t sizeOf,
                                      std::uint32_t alignOf);

  DECL_GETTER(uint32_t, id);
  DECL_GETTER(std::vector<Field>, fields);
  DECL_GETTER(uint64_t, sizeOf);
  DECL_GETTER(uint64_t, alignOf);

  bool operator==(const AnonymousUnionType &rhs) const {
    return id_ == rhs.id_;
  }
  bool operator!=(const AnonymousUnionType &rhs) const {
    return !(rhs == *this);
  }
};

class AnonymousEnumType final {
private:
  std::shared_ptr<Type> type_;
  uint64_t id_;
  AnonymousEnumType(uint64_t id, std::shared_ptr<Type> type)
      : id_(id), type_(type) {}

public:
  static std::shared_ptr<Type> create(bool isConst, bool isVolatile,
                                      std::uint64_t id,
                                      std::shared_ptr<Type> type);

  DECL_GETTER(uint64_t, id);
  DECL_GETTER(std::shared_ptr<Type>, type);

  uint64_t sizeOf() const;
  uint64_t alignOf() const;

  bool operator==(const AnonymousEnumType &rhs) const { return id_ == rhs.id_; }
  bool operator!=(const AnonymousEnumType &rhs) const {
    return !(rhs == *this);
  }
};

class Type final {
public:
  using Variant =
      std::variant<std::monostate, PrimitiveType, ArrayType, AbstractArrayType,
                   StructType, UnionType, EnumType, AnonymousStructType,
                   AnonymousUnionType, AnonymousEnumType, PointerType,
                   FunctionType>;

private:
  Variant type_;
  std::string_view name_;
  bool isConst_;
  bool isVolatile_;

public:
  explicit Type(bool isConst = false, bool isVolatile = false,
                Variant type = std::monostate{})
      : type_(std::move(type)), isConst_(isConst), isVolatile_(isVolatile) {}

  [[nodiscard]] bool isUndefined() const {
    return std::holds_alternative<std::monostate>(type_);
  }

  DECL_GETTER(Variant, type);
  DECL_GETTER(std::string_view, name);
  DECL_GETTER(bool, isConst);
  DECL_GETTER(bool, isVolatile);

  void setName(std::string_view name) { name_ = name; }

  bool isTypedef() const { return !name_.empty(); }

  [[nodiscard]] std::uint64_t sizeOf() const;
  [[nodiscard]] std::uint64_t alignOf() const;

  [[nodiscard]] bool operator==(const Type &rhs) const;
  [[nodiscard]] bool operator!=(const Type &rhs) const;
};
} // namespace lcc

#endif // LCC_TYPE_H
