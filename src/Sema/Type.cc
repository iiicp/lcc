/***********************************
 * File:     Type.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#include "Type.h"
#include "Match.h"
#include <tuple>

namespace lcc {
PrimitiveType::PrimitiveType(Kind kind) {
  switch (kind) {
  case Char:
  case UnSignedChar: {
    sizeOf_ = 1;
    alignOf_ = 1;
    isFloatingPoint_ = false;
    if (kind == Char) {
      isSigned_ = true;
    } else {
      isSigned_ = false;
    }
    break;
  }
  case Bool: {
    sizeOf_ = 1;
    alignOf_ = 1;
    isFloatingPoint_ = false;
    isSigned_ = true;
    break;
  }
  case Short:
  case UnSignedShort: {
    sizeOf_ = 2;
    alignOf_ = 2;
    isFloatingPoint_ = false;
    if (kind == Short) {
      isSigned_ = true;
    } else {
      isSigned_ = false;
    }
    break;
  }
  case Int:
  case UnSignedInt: {
    sizeOf_ = 4;
    alignOf_ = 4;
    isFloatingPoint_ = false;
    if (kind == Int) {
      isSigned_ = true;
    } else {
      isSigned_ = false;
    }
    break;
  }
  case Long:
  case UnSignedLong: {
    sizeOf_ = 8;
    alignOf_ = 8;
    isFloatingPoint_ = false;
    if (kind == Long) {
      isSigned_ = true;
    } else {
      isSigned_ = false;
    }
    break;
  }
  case LongLong:
  case UnSignedLongLong: {
    sizeOf_ = 8;
    alignOf_ = 8;
    isFloatingPoint_ = false;
    if (kind == LongLong) {
      isSigned_ = true;
    } else {
      isSigned_ = false;
    }
    break;
  }
  case Float: {
    sizeOf_ = 4;
    alignOf_ = 4;
    isFloatingPoint_ = true;
    isSigned_ = true;
    break;
  }
  case Double:
  case LongDouble: {
    sizeOf_ = 8;
    alignOf_ = 8;
    isFloatingPoint_ = true;
    isSigned_ = true;
    break;
  }
  case Void: {
    sizeOf_ = 0;
    alignOf_ = 0;
    isFloatingPoint_ = false;
    isSigned_ = true;
    break;
  }
  }
}

std::shared_ptr<Type> PrimitiveType::create(bool isConst, bool isVolatile,
                                            PrimitiveType::Kind kind) {
  return std::make_shared<Type>(isConst, isVolatile, PrimitiveType(kind));
}

bool PrimitiveType::operator==(const PrimitiveType &rhs) const {
  return kind_ == rhs.kind_;
}

bool PrimitiveType::operator!=(const PrimitiveType &rhs) const {
  return !(rhs == *this);
}

PointerType::PointerType(bool isRestricted, std::shared_ptr<Type> elementType)
    : restricted_(isRestricted), elementType_(elementType) {}

std::shared_ptr<Type> PointerType::create(bool isConst, bool isVolatile,
                                          bool restricted,
                                          std::shared_ptr<Type> elementType) {
  return std::make_shared<Type>(isConst, isVolatile,
                                PointerType(restricted, elementType));
}

bool PointerType::operator==(const PointerType &rhs) const {
  return std::tie(restricted_, *elementType_) ==
         std::tie(rhs.restricted_, *rhs.elementType_);
}

bool PointerType::operator!=(const PointerType &rhs) const {
  return !(rhs == *this);
}

uint64_t PointerType::sizeOf() const { return 8; }

uint64_t PointerType::alignOf() const { return 8; }

ArrayType::ArrayType(bool isRestricted, bool isStatic,
                     std::shared_ptr<Type> type, size_t size)
    : isRestricted_(isRestricted), isStatic_(isStatic), type_(type),
      size_(size) {}

std::shared_ptr<Type> ArrayType::create(bool isConst, bool isVolatile,
                                        bool isRestricted, bool isStatic,
                                        std::shared_ptr<Type> elemType,
                                        size_t size) {
  return std::make_shared<Type>(
      isConst, isVolatile, ArrayType(isRestricted, isStatic, elemType, size));
}

uint64_t ArrayType::sizeOf() const { return size_ * type_->sizeOf(); }

uint64_t ArrayType::alignOf() const { return type_->alignOf(); }

bool ArrayType::operator==(const ArrayType &rhs) const {
  return std::tie(isRestricted_, *type_, size_) ==
         std::tie(rhs.isRestricted_, *rhs.type_, rhs.size_);
}

bool ArrayType::operator!=(const ArrayType &rhs) const {
  return !(rhs == *this);
}

AbstractArrayType::AbstractArrayType(bool isRestricted,
                                     std::shared_ptr<Type> type)
    : isRestricted_(isRestricted), type_(type) {}

std::shared_ptr<Type>
AbstractArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                          std::shared_ptr<Type> elemType) {
  return std::make_shared<Type>(isConst, isVolatile,
                                AbstractArrayType(isRestricted, elemType));
}

uint64_t AbstractArrayType::sizeOf() const { LCC_UNREACHABLE; }

uint64_t AbstractArrayType::alignOf() const { return type_->alignOf(); }

bool AbstractArrayType::operator==(const lcc::AbstractArrayType &rhs) const {
  return std::tie(isRestricted_, *type_) ==
         std::tie(rhs.isRestricted_, *rhs.type_);
}

bool AbstractArrayType::operator!=(const lcc::AbstractArrayType &rhs) const {
  return !(rhs == *this);
}

FunctionType::FunctionType(std::shared_ptr<Type> returnType,
                           std::vector<Argument> arguments, bool lastIsVararg)
    : returnType_(returnType), arguments_(std::move(arguments)),
      lastIsVararg_(lastIsVararg) {}

std::shared_ptr<Type> FunctionType::create(std::shared_ptr<Type> returnType,
                                           std::vector<Argument> &&arguments,
                                           bool lastIsVararg) {
  return std::make_shared<Type>(
      false, false,
      FunctionType(returnType, std::move(arguments), lastIsVararg));
}

bool FunctionType::operator==(const FunctionType &rhs) const {
  if (lastIsVararg_ != rhs.lastIsVararg_) {
    return false;
  }
  if (*returnType_ != *rhs.returnType_) {
    return false;
  }
  return std::equal(arguments_.begin(), arguments_.end(),
                    rhs.arguments_.begin(), rhs.arguments_.end(),
                    [](const auto &lhs, const auto &rhs) {
                      return *lhs.first == *rhs.first;
                    });
}

bool FunctionType::operator!=(const FunctionType &rhs) const {
  return !(rhs == *this);
}

StructType::StructType(std::string_view name, uint64_t index)
    : name_(name), index_(index) {}

std::shared_ptr<Type> StructType::create(bool isConst, bool isVolatile,
                                         std::string_view name,
                                         uint64_t index) {
  return std::make_shared<Type>(isConst, isVolatile, StructType(name, index));
}

uint64_t StructType::sizeOf() const {}

uint64_t StructType::alignOf() const {}

bool StructType::operator==(const lcc::StructType &rhs) const { return false; }

bool StructType::operator!=(const lcc::StructType &rhs) const {
  return !(rhs == *this);
}

UnionType::UnionType(std::string_view name, uint64_t index)
    : name_(name), index_(index) {}

std::shared_ptr<Type> UnionType::create(bool isConst, bool isVolatile,
                                        std::string_view name, uint64_t index) {
  return std::make_shared<Type>(isConst, isVolatile, UnionType(name, index));
}

uint64_t UnionType::sizeOf() const {}

uint64_t UnionType::alignOf() const {}

bool UnionType::operator==(const lcc::UnionType &rhs) const { return false; }

bool UnionType::operator!=(const lcc::UnionType &rhs) const {
  return !(rhs == *this);
}

EnumType::EnumType(std::string_view name, uint64_t index)
    : name_(name), index_(index) {}

std::shared_ptr<Type> EnumType::create(bool isConst, bool isVolatile,
                                       std::string_view name, uint64_t index) {
  return std::make_shared<Type>(isConst, isVolatile, EnumType(name, index));
}

uint64_t EnumType::sizeOf() const {}

uint64_t EnumType::alignOf() const {}

bool EnumType::operator==(const lcc::EnumType &rhs) const { return false; }

bool EnumType::operator!=(const lcc::EnumType &rhs) const {
  return !(rhs == *this);
}

std::shared_ptr<Type> AnonymousStructType::create(bool isConst, bool isVolatile,
                                                  std::uint64_t id,
                                                  std::vector<Field> fields,
                                                  std::uint32_t sizeOf,
                                                  std::uint32_t alignOf) {
  return std::make_shared<Type>(
      isConst, isVolatile,
      AnonymousStructType(id, std::move(fields), sizeOf, alignOf));
}

std::shared_ptr<Type> AnonymousUnionType::create(bool isConst, bool isVolatile,
                                                 std::uint64_t id,
                                                 std::vector<Field> fields,
                                                 std::uint32_t sizeOf,
                                                 std::uint32_t alignOf) {
  return std::make_shared<Type>(
      isConst, isVolatile,
      AnonymousUnionType(id, std::move(fields), sizeOf, alignOf));
}

std::shared_ptr<Type> AnonymousEnumType::create(bool isConst, bool isVolatile,
                                                std::uint64_t id,
                                                std::shared_ptr<Type> type) {
  return std::make_shared<Type>(isConst, isVolatile,
                                AnonymousEnumType(id, type));
}

uint64_t AnonymousEnumType::sizeOf() const { return type_->sizeOf(); }

uint64_t AnonymousEnumType::alignOf() const { return type_->alignOf(); }

std::uint64_t Type::alignOf() const {
  return match(type_, [&](auto &&value) -> uint64_t {
    if constexpr (std::is_same_v<std::monostate,
                                 std::decay_t<decltype(value)>>) {
      LCC_UNREACHABLE;
    } else {
      return value.alignOf();
    }
  });
}

std::uint64_t Type::sizeOf() const {
  return match(type_, [&](auto &&value) -> uint64_t {
    if constexpr (std::is_same_v<std::monostate,
                                 std::decay_t<decltype(value)>>) {
      LCC_UNREACHABLE;
    } else {
      return value.sizeOf();
    }
  });
}

bool Type::operator==(const Type &rhs) const {
  if (std::tie(isConst_, isVolatile_) !=
      std::tie(rhs.isConst_, rhs.isVolatile_)) {
    return false;
  }
  if (type_.index() != rhs.type_.index()) {
    return false;
  }
  return match(type_, [&](auto &&value) -> bool {
    using T = std::decay_t<decltype(value)>;
    return value == std::get<T>(rhs.type_);
  });
}

bool Type::operator!=(const Type &rhs) const { return !(rhs == *this); }
} // namespace lcc