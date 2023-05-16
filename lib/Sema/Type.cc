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
#include "lcc/Sema/Type.h"
#include "lcc/Basic/Match.h"
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
    : elementType_(elementType), restricted_(isRestricted) {}

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