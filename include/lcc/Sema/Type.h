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
#include "lcc/Basic/Util.h"
#include <cstdint>
#include <memory>
#include <string_view>
#include <variant>
#include <vector>
#include <optional>

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

class Type final {
public:
  using Variant =
      std::variant<std::monostate, PrimitiveType, PointerType, FunctionType>;

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

  DECL_GETTER(const Variant &, type);
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
