/***********************************
 * File:     Type.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/7
 ***********************************/

#include "Type.h"
#include "Utilities.h"
#include "RecursiveVisitor.h"
namespace lcc::Sema {

template <class T1, class T2>
constexpr T1 roundUpTo(T1 number, T2 multiple)
{
  static_assert(std::is_integral_v<T1> && std::is_integral_v<T2>);
  if (multiple == 0)
  {
    return number;
  }

  auto remainder = number % multiple;
  if (remainder == 0)
  {
    return number;
  }

  return number + multiple - remainder;
}

/// PrimaryType start
PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned, std::uint8_t bitCount,
                             std::uint8_t alignOf, Kind kind)
    : m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned),
      m_bitCount(bitCount), m_alignOf(alignOf), m_kind(kind) {}

Type PrimitiveType::create(bool isConst, bool isVolatile, bool isFloatingPoint,
                           bool isSigned, std::uint8_t bitCount, std::uint8_t alignOf,
                           Kind kind) {
  return Type(isConst, isVolatile,
              PrimitiveType(isFloatingPoint, isSigned, bitCount, alignOf, kind));
}

std::uint8_t PrimitiveType::getByteCount() const {
  return roundUpTo(m_bitCount, m_alignOf * 8) / 8;
}

bool PrimitiveType::operator==(const PrimitiveType &rhs) const {
  if (m_bitCount == 0 && rhs.m_bitCount == 0) {
    return true;
  }
  return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount, m_kind) ==
         std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount,
                  rhs.m_kind);
}

bool PrimitiveType::operator!=(const PrimitiveType &rhs) const {
  return !(rhs == *this);
}

Type PrimitiveType::createChar(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 8, 1, Kind::Char);
}

Type PrimitiveType::createUnsignedChar(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 8, 1,
                Kind::UnsignedChar);
}

Type PrimitiveType::createUnderlineBool(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 1, 1, Kind::Bool);
}

Type PrimitiveType::createShort(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 2 * 8, 2, Kind::Short);
}

Type PrimitiveType::createUnsignedShort(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 2 * 8, 2,
                Kind::UnsignedShort);
}

Type PrimitiveType::createInt(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 4 * 8, 4, Kind::Int);
}

Type PrimitiveType::createUnsignedInt(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 4 * 8, 4,
                Kind::UnsignedInt);
}

Type PrimitiveType::createLong(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 8 * 8, 8, Kind::Long);
}

Type PrimitiveType::createUnsignedLong(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 8 * 8, 8,
                Kind::UnsignedLong);
}

Type PrimitiveType::createLongLong(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 64, 8,
                Kind::LongLong);
}

Type PrimitiveType::createUnsignedLongLong(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, false, 64, 8,
                Kind::UnsignedLongLong);
}

Type PrimitiveType::createFloat(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, true, true, 32, 4, Kind::Float);
}

Type PrimitiveType::createDouble(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, true, true, 64, 8, Kind::Double);
}

Type PrimitiveType::createLongDouble(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, true, true, 64, 8,
                Kind::LongDouble);
}

Type PrimitiveType::createVoid(bool isConst, bool isVolatile) {
  return create(isConst, isVolatile, false, true, 0, 0, Kind::Void);
}
/// PrimaryType end

/// ArrayType start
ArrayType::ArrayType(bool isRestricted, bool isStatic,
                     std::shared_ptr<Type> type,
                     std::size_t size)
    : m_restricted(isRestricted), m_static(isStatic),
      m_type(std::move(type)), m_size(size) {}

Type ArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                       bool isStatic, Type type, std::size_t size) {
  return Type(
      isConst, isVolatile,
      ArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type)), size));
}

std::uint64_t ArrayType::getSizeOf() const {
  return m_type->getSizeOf() * m_size;
}

std::uint64_t ArrayType::getAlignOf() const {
  return m_type->getAlignOf();
}

bool ArrayType::operator==(const ArrayType &rhs) const {
  return std::tie(m_restricted, m_static, *m_type, m_size) ==
         std::tie(rhs.m_restricted, rhs.m_static, *rhs.m_type, rhs.m_size);
}

bool ArrayType::operator!=(const ArrayType &rhs) const {
  return !(rhs == *this);
}
/// ArrayType end

/// ValArrayType start
ValArrayType::ValArrayType(bool isRestricted, bool isStatic, std::shared_ptr<Type> &&type)
    : m_restricted(isRestricted), m_static(isStatic), m_type(std::move(type)) {}

Type ValArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                          bool isStatic, Type type) {
  return Type(
      isConst, isVolatile,
      ValArrayType(isRestricted, isStatic, std::make_shared<Type>(std::move(type))));
}

std::uint64_t ValArrayType::getAlignOf() const {
  return m_type->getAlignOf();
}

bool ValArrayType::operator==(const ValArrayType &rhs) const {
  return std::tie(m_restricted, m_static, *m_type) ==
         std::tie(rhs.m_restricted, rhs.m_static, *rhs.m_type);
}

bool ValArrayType::operator!=(const ValArrayType &rhs) const {
  return !(rhs == *this);
}
/// ValArrayType end

/// abstractArrayType Start
AbstractArrayType::AbstractArrayType(bool isRestricted,
                                     std::shared_ptr<Type> &&type)
    : m_restricted(isRestricted), m_type(std::move(type)) {}

Type AbstractArrayType::create(bool isConst, bool isVolatile, bool isRestricted,
                               Type type) {
  return Type(
      isConst, isVolatile,
      AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

std::uint64_t AbstractArrayType::getAlignOf() const {
  return m_type->getAlignOf();
}

bool AbstractArrayType::operator==(const AbstractArrayType &rhs) const {
  return std::tie(m_restricted, *m_type) ==
         std::tie(rhs.m_restricted, *rhs.m_type);
}

bool AbstractArrayType::operator!=(const AbstractArrayType &rhs) const {
  return !(rhs == *this);
}

/// abstractArrayType end

/// pointerType start
PointerType::PointerType(bool isRestricted, std::shared_ptr<Type> &&elementType)
    : m_restricted(isRestricted), m_elementType(std::move(elementType)) {}

Type PointerType::create(bool isConst, bool isVolatile, bool isRestricted,
                         Type elementType) {
  return Type(isConst, isVolatile,
              PointerType(isRestricted,
                          std::make_shared<Type>(std::move(elementType))));
}

[[nodiscard]] uint64_t PointerType::getSizeOf() const {
  return 8;
}

[[nodiscard]] uint64_t PointerType::getAlignOf() const {
  return 8;
}

bool PointerType::operator==(const PointerType &rhs) const {
  return std::tie(m_restricted, *m_elementType) ==
         std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool PointerType::operator!=(const PointerType &rhs) const {
  return !(rhs == *this);
}
/// PointerType end

/// FunctionType start
FunctionType::FunctionType(std::shared_ptr<Type> &&returnType,
                           std::vector<std::pair<Type, std::string_view>> arguments,
                           bool lastIsVararg)
    : m_returnType(std::move(returnType)), m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg) {}

Type FunctionType::create(Type returnType,
                          std::vector<std::pair<Type, std::string_view>> &&arguments,
                          bool lastIsVararg) {
  return Type(false, false,
              FunctionType(std::make_shared<Type>(std::move(returnType)),
                           std::move(arguments), lastIsVararg));
}


bool FunctionType::operator==(const FunctionType &rhs) const {
  if (m_lastIsVararg != rhs.m_lastIsVararg)
  {
    return false;
  }
  if (*m_returnType != *rhs.m_returnType)
  {
    return false;
  }
  return std::equal(m_arguments.begin(), m_arguments.end(), rhs.m_arguments.begin(), rhs.m_arguments.end(),
                    [](const auto& lhs, const auto& rhs) { return lhs.first == rhs.first; });
}

bool FunctionType::operator!=(const FunctionType &rhs) const {
  return !(rhs == *this);
}
/// FunctionType end

/// RecordType start
StructType::StructType(
    std::string_view name, size_t id)
    : m_name(name), m_id(id) {
}

Type StructType::create(
    bool isConst, bool isVolatile, std::string_view name, size_t id) {
  return Type(isConst, isVolatile, StructType(name, id));
}

uint64_t StructType::getSizeOf() const {

}

uint64_t StructType::getAlignOf() const {

}

/// RecordType end

/// UnionType start
UnionType::UnionType(
    std::string_view name, size_t id)
    : m_name(name), m_id(id) {
}

Type UnionType::create(
    bool isConst, bool isVolatile, std::string_view name, size_t id) {
  return Type(isConst, isVolatile, UnionType(name, id));
}

uint64_t UnionType::getSizeOf() const {

}

uint64_t UnionType::getAlignOf() const {

}

/// UnionType end

/// enumType start
EnumType::EnumType(std::string_view name, size_t id)
    : m_name(name), m_id(id) {}

Type EnumType::create(
    bool isConst, bool isVolatile, std::string_view name, size_t id) {
  return Type(isConst, isVolatile,EnumType(name, id));
}
uint64_t EnumType::getSizeOf() const {

}

uint64_t EnumType::getAlignOf() const {

}
/// enumType end

std::uint64_t Type::getSizeOf() const {
  return std::visit([&](auto && value)->size_t {
      if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>) {
        LCC_UNREACHABLE;
      }else {
        return value.getSizeOf();
      }
    }, m_type);
}

std::uint64_t Type::getAlignOf() const {
  return std::visit([&](auto && value)->size_t {
    if constexpr (std::is_same_v<std::monostate, std::decay_t<decltype(value)>>) {
      LCC_UNREACHABLE;
    }else {
      return value.getAlignOf();
    }
  }, m_type);
}

/// Type start
bool Type::operator==(const Type &rhs) const {
  if (std::tie(m_isConst, m_isVolatile) != std::tie(rhs.m_isConst, rhs.m_isVolatile)) {
    return false;
  }
  if (m_type.index() != rhs.m_type.index()) {
    return false;
  }
  return std::visit([&](auto&& value) -> bool {
    using T = std::decay_t<decltype(value)>;
    return value == std::get<T>(rhs.m_type);
  },m_type);
}

bool Type::operator!=(const Type &rhs) const { return !(rhs == *this); }

bool Type::isVoid() const{
  auto *primitive = std::get_if<PrimitiveType>(&m_type);
  if (!primitive) {
    return false;
  }
  return primitive->getKind() == PrimitiveType::Kind::Void;
}

bool Type::isArray() const{
  return std::holds_alternative<ArrayType>(m_type) ||
      std::holds_alternative<ValArrayType>(m_type) ||
          std::holds_alternative<AbstractArrayType>(m_type);
}

bool Type::isCharArray() const{
  if (!isArray()) return false;
  auto type = getArrayElementType();
  auto *primitive = std::get_if<PrimitiveType>(&type.getVariant());
  if (!primitive) return false;
  return primitive->getKind() == PrimitiveType::Kind::Char ||
         primitive->getKind() == PrimitiveType::Kind::UnsignedChar;
}

bool Type::isInteger() const{
  return std::holds_alternative<PrimitiveType>(m_type) &&
      !std::get<PrimitiveType>(m_type).isFloatingPoint() &&
      std::get<PrimitiveType>(m_type).getBitCount() != 0;
}

bool Type::isArithmetic() const{
  return (std::holds_alternative<PrimitiveType>(m_type)
      && std::get<PrimitiveType>(m_type).getBitCount() != 0) ||
  std::holds_alternative<EnumType>(m_type);
}

bool Type::isScalar() const{
  return isArithmetic() || std::holds_alternative<PointerType>(m_type);
}

bool Type::isRecord() const{
  return isUnion() || isStruct();
}

bool Type::isStruct() const{
  return std::holds_alternative<StructType>(m_type);
}

bool Type::isUnion() const{
  return std::holds_alternative<UnionType>(m_type);
}

bool Type::isAnonymous() const{
  return
      (std::holds_alternative<EnumType>(m_type) && std::get<EnumType>(m_type).isAnonymous()) ||
      (std::holds_alternative<StructType>(m_type) && std::get<StructType>(m_type).isAnonymous()) ||
      (std::holds_alternative<UnionType>(m_type) && std::get<UnionType>(m_type).isAnonymous());
}

bool Type::isEnum() const{
  return std::holds_alternative<EnumType>(m_type);
}

bool Type::isBool() const{
  auto *primitive = std::get_if<PrimitiveType>(&m_type);
  if (!primitive) return false;
  return primitive->getKind() == PrimitiveType::Kind::Bool;
}

bool Type::isCharType() const{
  auto *primitive = std::get_if<PrimitiveType>(&m_type);
  if (!primitive) return false;
  return primitive->getKind() == PrimitiveType::Kind::Char ||
  primitive->getKind() == PrimitiveType::Kind::UnsignedChar;
}

bool Type::isAggregate() const{
  return isRecord() || isArray();
}

bool Type::isVariablyModified() const{
  auto typeVisitor = RecursiveVisitor(*this, TYPE_NEXT_FN);
  return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                     [](const Type &type) {return std::holds_alternative<ValArrayType>(type.getVariant());});
}

bool Type::isVariableLengthArray() const{
  auto typeVisitor = RecursiveVisitor(*this, ARRAY_TYPE_NEXT_FN);
  return std::any_of(typeVisitor.begin(), typeVisitor.end(),
                     [](const Type &type) {return std::holds_alternative<ValArrayType>(type.getVariant());});
}

const Type& Type::getArrayElementType() const{
  return std::visit(
      overload{
          [](const auto &)->const Type& {LCC_UNREACHABLE;},
          [](const ArrayType &arrayType)->const Type&{
                return arrayType.getType();
              },
          [](const AbstractArrayType &abstractArrayType)->const Type&{
            return abstractArrayType.getType();
          },
          [](const ValArrayType &valArrayType)->const Type&{
            return valArrayType.getType();
          },
    }, m_type);
}
/// Type end

lcc::Sema::Type adjustParameterType(lcc::Sema::Type type){
  if (type.isArray())
  {
    auto elementType = std::visit([](auto&& value) -> Type {
      using T = std::decay_t<decltype(value)>;
      if constexpr (std::is_same_v<ArrayType,
                                   T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
      {
        return value.getType();
      }
      LCC_UNREACHABLE;
    }, type.getVariant());
    bool restrict = std::visit([](auto&& value) -> bool {
      using T = std::decay_t<decltype(value)>;
      if constexpr (std::is_same_v<ArrayType,
                                   T> || std::is_same_v<AbstractArrayType, T> || std::is_same_v<ValArrayType, T>)
      {
        return value.isRestricted();
      }
      LCC_UNREACHABLE;
    },type.getVariant());
    return PointerType::create(type.isConst(), type.isVolatile(), restrict, std::move(elementType));
  }
  return type;
}
lcc::Sema::Type removeQualifiers(lcc::Sema::Type type) {
  if (type.isConst() || type.isVolatile()
      || (std::holds_alternative<PointerType>(type.getVariant())
          && std::get<PointerType>(type.getVariant()).isRestricted()))
  {
    if (!std::holds_alternative<PointerType>(type.getVariant())
        || !std::get<PointerType>(type.getVariant()).isRestricted())
    {
      return Type(false, false, std::move(type).getVariant());
    }
    return PointerType::create(false, false, false,
                               std::get<lcc::Sema::PointerType>(type.getVariant()).getElementType());
  }
  return type;
}
} // namespace lcc::Sema