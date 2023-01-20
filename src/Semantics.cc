/***********************************
 * File:     Semantics.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/1/20
 ***********************************/

#include "Semantics.h"
#include "ConstantEvaluator.h"
#include <algorithm>
#include <cassert>
#include <sstream>
#include <utility>
namespace lcc::Semantics {
const Semantics::Type &Semantics::ArrayType::getType() const { return *m_type; }

std::size_t Semantics::ArrayType::getSize() const { return m_size; }

Semantics::ArrayType::ArrayType(bool isRestricted,
                                std::shared_ptr<Semantics::Type> &&type,
                                std::size_t size)
    : m_restricted(isRestricted), m_type(std::move(type)), m_size(size) {}

bool Semantics::ArrayType::isRestricted() const { return m_restricted; }

Semantics::Type Semantics::ArrayType::create(bool isConst, bool isVolatile,
                                             bool isRestricted,
                                             Semantics::Type &&type,
                                             std::size_t size) {
  std::ostringstream ss;
  ss << size;
  auto name = type.getName() + "[" + ss.str() + "]";
  return Semantics::Type(
      isConst, isVolatile, name,
      ArrayType(isRestricted, std::make_shared<Type>(std::move(type)), size));
}

bool Semantics::ArrayType::operator==(const Semantics::ArrayType &rhs) const {
  return std::tie(m_restricted, *m_type, m_size) ==
         std::tie(rhs.m_restricted, *rhs.m_type, rhs.m_size);
}

bool Semantics::ArrayType::operator!=(const Semantics::ArrayType &rhs) const {
  return !(rhs == *this);
}

bool Semantics::Type::isConst() const { return m_isConst; }

bool Semantics::Type::isVolatile() const { return m_isVolatile; }

const std::string &Semantics::Type::getName() const { return m_name; }

void Semantics::Type::setName(const std::string &name) { m_name = name; }

const Semantics::Type::variant &Semantics::Type::get() const { return m_type; }

Semantics::Type::Type(bool isConst, bool isVolatile, std::string name,
                      Semantics::Type::variant &&type)
    : m_isConst(isConst), m_isVolatile(isVolatile), m_name(std::move(name)),
      m_type(std::move(type)) {}

bool Semantics::Type::operator==(const Semantics::Type &rhs) const {
  return std::tie(m_isConst, m_isVolatile, m_type) ==
         std::tie(rhs.m_isConst, rhs.m_isVolatile, rhs.m_type);
}

bool Semantics::Type::operator!=(const Semantics::Type &rhs) const {
  return !(rhs == *this);
}

bool Semantics::Type::isCompatibleWith(const Semantics::Type &rhs) const {
  return m_type == rhs.m_type;
}

Semantics::PointerType::PointerType(
    bool isRestricted, std::shared_ptr<Semantics::Type> &&elementType)
    : m_restricted(isRestricted), m_elementType(std::move(elementType)) {}

const Semantics::Type &Semantics::PointerType::getElementType() const {
  return *m_elementType;
}

bool Semantics::PointerType::isRestricted() const { return m_restricted; }

Semantics::Type Semantics::PointerType::create(bool isConst, bool isVolatile,
                                               bool isRestricted,
                                               Semantics::Type &&elementType) {
  std::string name;
  if (std::holds_alternative<FunctionType>(elementType.get())) {
    auto openParenthese = elementType.getName().find('(');
    name = elementType.getName().substr(0, openParenthese) + "(*)" +
           elementType.getName().substr(openParenthese);
  } else if (std::holds_alternative<ArrayType>(elementType.get()) ||
             std::holds_alternative<ValArrayType>(elementType.get()) ||
                 std::holds_alternative<AbstractArrayType>(elementType.get())) {
    auto openBracket = elementType.getName().find('[');
    name = elementType.getName().substr(0, openBracket) + "(*)" +
           elementType.getName().substr(openBracket);
  } else {
    name = elementType.getName() + "*";
  }
  return Semantics::Type(
      isConst, isVolatile, name,
      PointerType(isRestricted,
                  std::make_shared<Type>(std::move(elementType))));
}

bool Semantics::PointerType::operator==(
    const Semantics::PointerType &rhs) const {
  return std::tie(m_restricted, *m_elementType) ==
         std::tie(rhs.m_restricted, *rhs.m_elementType);
}

bool Semantics::PointerType::operator!=(
    const Semantics::PointerType &rhs) const {
  return !(rhs == *this);
}

Semantics::EnumType::EnumType(
    std::string name, std::vector<std::pair<std::string, std::int32_t>> values)
    : m_name(std::move(name)), m_values(std::move(values)) {}

const std::vector<std::pair<std::string, int32_t>> &
Semantics::EnumType::getValues() const {
  return m_values;
}

bool Semantics::EnumType::isDefinition() const { return !m_values.empty(); }

bool Semantics::EnumType::isAnonymous() const { return m_name.empty(); }

Semantics::Type Semantics::EnumType::create(
    bool isConst, bool isVolatile, const std::string &name,
    std::vector<std::pair<std::string, std::int32_t>> values) {
  return Semantics::Type(isConst, isVolatile, "enum " + name,
                         EnumType(name, std::move(values)));
}

bool Semantics::EnumType::operator==(const Semantics::EnumType &rhs) const {
  return m_name == rhs.m_name;
}

bool Semantics::EnumType::operator!=(const Semantics::EnumType &rhs) const {
  return !(rhs == *this);
}

const std::string &Semantics::EnumType::getName() const { return m_name; }

Semantics::PrimitiveType::PrimitiveType(bool isFloatingPoint, bool isSigned,
                                        std::uint8_t bitCount)
    : m_isFloatingPoint(isFloatingPoint), m_isSigned(isSigned),
      m_bitCount(bitCount) {}

bool Semantics::PrimitiveType::isFloatingPoint() const {
  return m_isFloatingPoint;
}

bool Semantics::PrimitiveType::isSigned() const { return m_isSigned; }

std::uint8_t Semantics::PrimitiveType::getBitCount() const {
  return m_bitCount;
}

Semantics::Type Semantics::PrimitiveType::create(bool isConst, bool isVolatile,
                                                 bool isFloatingPoint,
                                                 bool isSigned,
                                                 std::uint8_t bitCount) {
  return Semantics::Type(
      isConst, isVolatile,
      [=]() -> const char * {
        if (isFloatingPoint) {
          if (bitCount == 32) {
            return "float";
          } else if (bitCount == 64) {
            return "double";
          }
          return "";
        }
        switch (bitCount) {
        case 8:
          return isSigned ? "char" : "unsigned char";
        case 16:
          return isSigned ? "short" : "unsigned short";
        case 32:
          return isSigned ? "int" : "unsigned int";
        case 64:
          return isSigned ? "long long" : "unsigned long long";
        default:
          return "void";
        }
        return "";
      }(),
      PrimitiveType(isFloatingPoint, isSigned, bitCount));
}

bool Semantics::PrimitiveType::operator==(
    const Semantics::PrimitiveType &rhs) const {
  if (m_bitCount == 0 && rhs.m_bitCount == 0) {
    return true;
  }
  return std::tie(m_isFloatingPoint, m_isSigned, m_bitCount) ==
         std::tie(rhs.m_isFloatingPoint, rhs.m_isSigned, rhs.m_bitCount);
}

bool Semantics::PrimitiveType::operator!=(
    const Semantics::PrimitiveType &rhs) const {
  return !(rhs == *this);
}

Semantics::Type Semantics::PrimitiveType::createChar(bool isConst,
                                                     bool isVolatile) {
  return create(isConst, isVolatile, false, true, 8);
}

Semantics::Type Semantics::PrimitiveType::createUnsignedChar(bool isConst,
                                                             bool isVolatile) {
  return create(isConst, isVolatile, false, false, 8);
}

Semantics::Type Semantics::PrimitiveType::createShort(bool isConst,
                                                      bool isVolatile) {
  return create(isConst, isVolatile, false, true, 16);
}

Semantics::Type Semantics::PrimitiveType::createUnsignedShort(bool isConst,
                                                              bool isVolatile) {
  return create(isConst, isVolatile, false, false, 16);
}

Semantics::Type Semantics::PrimitiveType::createInt(bool isConst,
                                                    bool isVolatile) {
  return create(isConst, isVolatile, false, true, 32);
}

Semantics::Type Semantics::PrimitiveType::createUnsignedInt(bool isConst,
                                                            bool isVolatile) {
  return create(isConst, isVolatile, false, false, 32);
}

Semantics::Type Semantics::PrimitiveType::createLongLong(bool isConst,
                                                         bool isVolatile) {
  return create(isConst, isVolatile, false, true, 64);
}

Semantics::Type
Semantics::PrimitiveType::createUnsignedLongLong(bool isConst,
                                                 bool isVolatile) {
  return create(isConst, isVolatile, false, false, 64);
}

Semantics::Type Semantics::PrimitiveType::createFloat(bool isConst,
                                                      bool isVolatile) {
  return create(isConst, isVolatile, true, true, 32);
}

Semantics::Type Semantics::PrimitiveType::createDouble(bool isConst,
                                                       bool isVolatile) {
  return create(isConst, isVolatile, true, true, 64);
}

Semantics::Type Semantics::PrimitiveType::createVoid(bool isConst,
                                                     bool isVolatile) {
  return create(isConst, isVolatile, false, true, 0);
}

Semantics::ValArrayType::ValArrayType(bool isRestricted,
                                              std::shared_ptr<Semantics::Type>&& type)
    : m_restricted(isRestricted), m_type(std::move(type))
{
}

const Semantics::Type& Semantics::ValArrayType::getType() const
{
  return *m_type;
}

bool Semantics::ValArrayType::isRestricted() const
{
  return m_restricted;
}

Semantics::Type Semantics::ValArrayType::create(bool isConst, bool isVolatile,
                                                                bool isRestricted,
                                                                Semantics::Type&& type)
{
  auto name = type.getName() + "[*]";
  return Semantics::Type(isConst, isVolatile, name,
                                 ValArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool Semantics::ValArrayType::operator==(const Semantics::ValArrayType& rhs) const
{
  return std::tie(m_restricted, *m_type) == std::tie(rhs.m_restricted, *rhs.m_type);
}

bool Semantics::ValArrayType::operator!=(const Semantics::ValArrayType& rhs) const
{
  return !(rhs == *this);
}

Semantics::FunctionType::FunctionType(
    std::shared_ptr<Type> &&returnType,
    std::vector<std::pair<Type, std::string>> arguments, bool lastIsVararg,
    bool hasPrototype)
    : m_returnType(std::move(returnType)), m_arguments(std::move(arguments)),
      m_lastIsVararg(lastIsVararg), m_hasPrototype(hasPrototype) {}

const Semantics::Type &Semantics::FunctionType::getReturnType() const {
  return *m_returnType;
}

const std::vector<std::pair<Semantics::Type, std::string>> &
Semantics::FunctionType::getArguments() const {
  return m_arguments;
}

bool Semantics::FunctionType::isLastVararg() const { return m_lastIsVararg; }

Semantics::Type Semantics::FunctionType::create(
    Semantics::Type &&returnType,
    std::vector<std::pair<Type, std::string>> &&arguments, bool lastIsVararg,
    bool hasPrototype) {
  std::string argumentsNames;
  for (std::size_t i = 0; i < arguments.size(); i++) {
    argumentsNames += arguments[i].first.getName();
    if (i + 1 < arguments.size()) {
      argumentsNames += ", ";
    }
  }
  argumentsNames = returnType.getName() + "(" + argumentsNames + ")";
  return Semantics::Type(
      false, false, argumentsNames,
      FunctionType(std::make_shared<Type>(std::move(returnType)),
                   std::move(arguments), lastIsVararg, hasPrototype));
}

bool Semantics::FunctionType::operator==(
    const Semantics::FunctionType &rhs) const {
  std::vector<Type> thisTypes, rhsTypes;
  auto pairFirst = [](const auto &pair) { return pair.first; };
  std::transform(m_arguments.begin(), m_arguments.end(),
                 std::back_inserter(thisTypes), pairFirst);
  std::transform(rhs.m_arguments.begin(), rhs.m_arguments.end(),
                 std::back_inserter(rhsTypes), pairFirst);
  return std::tie(*m_returnType, thisTypes, m_lastIsVararg) ==
         std::tie(*rhs.m_returnType, rhsTypes, rhs.m_lastIsVararg);
}

bool Semantics::FunctionType::operator!=(
    const Semantics::FunctionType &rhs) const {
  return !(rhs == *this);
}

bool Semantics::FunctionType::hasPrototype() const { return m_hasPrototype; }

Semantics::AbstractArrayType::AbstractArrayType(
    bool isRestricted, std::shared_ptr<Semantics::Type> &&type)
    : m_restricted(isRestricted), m_type(std::move(type)) {}

const Semantics::Type &Semantics::AbstractArrayType::getType() const {
  return *m_type;
}

bool Semantics::AbstractArrayType::isRestricted() const { return m_restricted; }

Semantics::Type Semantics::AbstractArrayType::create(bool isConst,
                                                     bool isVolatile,
                                                     bool isRestricted,
                                                     Semantics::Type &&type) {
  auto name = type.getName() + "[]";
  return Semantics::Type(
      isConst, isVolatile, name,
      AbstractArrayType(isRestricted, std::make_shared<Type>(std::move(type))));
}

bool Semantics::AbstractArrayType::operator==(
    const Semantics::AbstractArrayType &rhs) const {
  return std::tie(m_restricted, *m_type) ==
         std::tie(rhs.m_restricted, *rhs.m_type);
}

bool Semantics::AbstractArrayType::operator!=(
    const Semantics::AbstractArrayType &rhs) const {
  return !(rhs == *this);
}

namespace {
template <typename G> struct Y {
  template <typename... X> decltype(auto) operator()(X &&...x) const & {
    return g(*this, std::forward<X>(x)...);
  }

  G g;
};

template <typename G> Y(G) -> Y<G>;

template <class... Ts> struct overload : Ts... { using Ts::operator()...; };
template <class... Ts> overload(Ts...) -> overload<Ts...>;

Expected<std::vector<std::pair<Semantics::Type, std::string>>, FailureReason>
parameterListToArguments(
    const std::vector<Syntax::ParameterDeclaration> &parameterDeclarations,
    const std::map<std::string, std::reference_wrapper<const Semantics::Type>>
        &typedefs,
    const std::vector<Syntax::Declaration> &declarations,
    const std::map<std::string, Semantics::RecordType> &structOrUnions) {
  std::vector<std::pair<Semantics::Type, std::string>> arguments;
  for (auto &pair : parameterDeclarations) {
    std::vector<Semantics::SpecifierQualifierRef> specifierQualifiers;
    for (auto &iter : pair.first) {
      auto optional = std::visit(
          overload{[&specifierQualifiers](const Syntax::TypeSpecifier &typeSpecifier)
                       -> std::optional<FailureReason> {
                     specifierQualifiers.emplace_back(typeSpecifier);
                     return {};
                   },
                   [&specifierQualifiers](const Syntax::TypeQualifier &typeQualifier)
                       -> std::optional<FailureReason> {
                     specifierQualifiers.emplace_back(typeQualifier);
                     return {};
                   },
                   [](const Syntax::StorageClassSpecifier &storageClassSpecifier)
                       -> std::optional<FailureReason> {
                     if (storageClassSpecifier !=
                         Syntax::StorageClassSpecifier::Register) {
                       return FailureReason(
                           "No storage class specifiers except register "
                           "allowed for function argument");
                     }
                     return {};
                   },
                   [](Syntax::FunctionSpecifier) -> std::optional<FailureReason> {
                     return FailureReason(
                         "Inline not allowed in paramter type list");
                   }},
          iter);
      if (optional) {
        return *optional;
      }
    }
    auto result = declaratorsToType(
        specifierQualifiers,
        std::visit(
            overload{[](const std::unique_ptr<Syntax::Declarator> &directDeclarator)
                         -> Semantics::PossiblyAbstractQualifierRef {
                       return std::cref(*directDeclarator);
                     },
                     [](const std::unique_ptr<Syntax::AbstractDeclarator>
                            &abstractDeclarator)
                         -> Semantics::PossiblyAbstractQualifierRef {
                       return abstractDeclarator.get();
                     }},
            pair.second),
        typedefs, declarations, structOrUnions);
    if (!result) {
      return result.error();
    }
    if (parameterDeclarations.size() == 1 &&
        *result == Semantics::PrimitiveType::createVoid(false, false) &&
        !std::visit(
            [](const auto &uniquePtr) -> bool { return uniquePtr.get(); },
            pair.second)) {
      break;
    }
    if (isVoid(*result)) {
      return FailureReason("Parameter is not allowed to have void datatype");
    }
    arguments.emplace_back(
        std::move(*result),
        std::holds_alternative<std::unique_ptr<Syntax::Declarator>>(pair.second)
            ? Semantics::declaratorToName(
                  *std::get<std::unique_ptr<Syntax::Declarator>>(pair.second))
            : "");
  }
  return arguments;
}

Expected<Semantics::Type, FailureReason>
primitivesToType(std::vector<Syntax::TypeSpecifier::PrimitiveTypeSpecifier> primitives,
                 bool isConst, bool isVolatile) {
  enum {
    Void =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void),
    Char =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Char),
    Short =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Short),
    Int = static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Int),
    Long =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Long),
    Float =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Float),
    Double =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Double),
    Signed =
        static_cast<std::size_t>(Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Signed),
    Unsigned = static_cast<std::size_t>(
        Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Unsigned),
  };
  std::array<std::size_t, 9> primitivesCount = {0};
  for (auto &iter : primitives) {
    primitivesCount[static_cast<std::size_t>(iter)]++;
  }
  if (primitivesCount[Void] > 1) {
    return FailureReason("void appearing more than once");
  }
  if (primitivesCount[Char] > 1) {
    return FailureReason("char appearing more than once");
  }
  if (primitivesCount[Short] > 1) {
    return FailureReason("short appearing more than once");
  }
  if (primitivesCount[Int] > 1) {
    return FailureReason("int appearing more than once");
  }
  if (primitivesCount[Float] > 1) {
    return FailureReason("float appearing more than once");
  }
  if (primitivesCount[Double] > 1) {
    return FailureReason("double appearing more than once");
  }
  if (primitivesCount[Signed] > 1) {
    return FailureReason("signed appearing more than once");
  }
  if (primitivesCount[Unsigned] > 1) {
    return FailureReason("unsigned appearing more than once");
  }
  if (primitivesCount[Long] > 2) {
    return FailureReason("long appearing more than twice");
  }
  bool hasSigned = primitivesCount[Signed];
  bool hasUnsigned = primitivesCount[Unsigned];
  if (hasSigned && hasUnsigned) {
    return FailureReason("Can't have both signed and unsigned");
  }
  if (primitivesCount[Void]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i++ == Void) {
                        return false;
                      }
                      return count;
                    })) {
      return FailureReason("Can't combine void with any other primitives");
    }
    return Semantics::PrimitiveType::createVoid(isConst, isVolatile);
  }
  if (primitivesCount[Float]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i++ == Float) {
                        return false;
                      }
                      return count;
                    })) {
      return FailureReason("Can't combine float with any other primitives");
    }
    return Semantics::PrimitiveType::createFloat(isConst, isVolatile);
  }
  if (primitivesCount[Double]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i++ == Double) {
                        return false;
                      }
                      return count;
                    })) {
      return FailureReason("Can't combine double with any other primitives");
    }
    return Semantics::PrimitiveType::createDouble(isConst, isVolatile);
  }
  if (primitivesCount[Char]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i == Char || i == Signed || i == Unsigned) {
                        i++;
                        return false;
                      }
                      i++;
                      return count;
                    })) {
      return FailureReason("Can only combine char with signed or unsigned");
    }
    if (hasUnsigned) {
      return Semantics::PrimitiveType::createUnsignedChar(isConst, isVolatile);
    } else {
      return Semantics::PrimitiveType::createChar(isConst, isVolatile);
    }
  }
  if (primitivesCount[Short]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i == Short || i == Signed || i == Unsigned ||
                          i == Int) {
                        i++;
                        return false;
                      }
                      i++;
                      return count;
                    })) {
      return FailureReason(
          "Can only combine short with signed,unsigned or int");
    }
    if (hasUnsigned) {
      return Semantics::PrimitiveType::createUnsignedShort(isConst, isVolatile);
    } else {
      return Semantics::PrimitiveType::createShort(isConst, isVolatile);
    }
  }
  if (primitivesCount[Long] == 1) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i == Signed || i == Unsigned || i == Int ||
                          i == Long) {
                        i++;
                        return false;
                      }
                      i++;
                      return count;
                    })) {
      return FailureReason(
          "Can only combine long with long, signed, unsigned and int");
    }
    if (hasUnsigned) {
      return Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
    } else {
      return Semantics::PrimitiveType::createInt(isConst, isVolatile);
    }
  }
  if (primitivesCount[Long] == 2) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i == Signed || i == Unsigned || i == Int ||
                          i == Long) {
                        i++;
                        return false;
                      }
                      i++;
                      return count;
                    })) {
      return FailureReason(
          "Can only combine long with long, signed, unsigned and int");
    }
    if (hasUnsigned) {
      return Semantics::PrimitiveType::createUnsignedLongLong(isConst,
                                                              isVolatile);
    } else {
      return Semantics::PrimitiveType::createLongLong(isConst, isVolatile);
    }
  }
  if (primitivesCount[Int]) {
    std::size_t i = 0;
    if (std::any_of(primitivesCount.begin(), primitivesCount.end(),
                    [&i](std::size_t count) -> bool {
                      if (i == Signed || i == Unsigned || i == Int) {
                        i++;
                        return false;
                      }
                      i++;
                      return count;
                    })) {
      return FailureReason("Can only combine int with signed or unsigned");
    }
    if (hasUnsigned) {
      return Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
    } else {
      return Semantics::PrimitiveType::createInt(isConst, isVolatile);
    }
  }
  if (hasSigned) {
    return Semantics::PrimitiveType::createInt(isConst, isVolatile);
  } else if (hasUnsigned) {
    return Semantics::PrimitiveType::createUnsignedInt(isConst, isVolatile);
  }
  return FailureReason("Internal compiler error");
}
} // namespace

Expected<Semantics::Type, FailureReason> Semantics::declaratorsToType(
    std::vector<SpecifierQualifierRef> specifierQualifiers,
    PossiblyAbstractQualifierRef declarator,
    const std::map<std::string, std::reference_wrapper<const Type>> &typedefs,
    const std::vector<Syntax::Declaration> &declarations,
    const std::map<std::string, Semantics::RecordType> &structOrUnions) {
  using ThisReturnType = Expected<Type, FailureReason>;

  bool isConst = false;
  bool isVolatile = false;
  bool isRestricted = false;
  for (auto &iter : specifierQualifiers) {
    if (auto *typeQualifier =
            std::get_if<std::reference_wrapper<const Syntax::TypeQualifier>>(
                &iter)) {
      switch (*typeQualifier) {
      case Syntax::TypeQualifier::Const: {
        isConst = true;
        break;
      }
      case Syntax::TypeQualifier::Restrict: {
        isRestricted = true;
        break;
      }
      case Syntax::TypeQualifier::Volatile: {
        isVolatile = true;
        break;
      }
      }
    }
  }

  std::vector<const Syntax::TypeSpecifier *> typeSpecifiers;
  std::transform(
      specifierQualifiers.begin(), specifierQualifiers.end(),
      std::back_inserter(typeSpecifiers),
      [](const auto &value) -> const Syntax::TypeSpecifier * {
        auto *refPointer =
            std::get_if<std::reference_wrapper<const Syntax::TypeSpecifier>>(
                &value);
        if (!refPointer) {
          return nullptr;
        } else {
          return &refPointer->get();
        }
      });
  typeSpecifiers.erase(
      std::remove(typeSpecifiers.begin(), typeSpecifiers.end(), nullptr),
      typeSpecifiers.end());

  if (typeSpecifiers.empty()) {
    return FailureReason("At least one type specifier must be present");
  }
  auto primitiveResult = std::visit(
      overload{
          [&typeSpecifiers, isConst, isVolatile](
              Syntax::TypeSpecifier::PrimitiveTypeSpecifier) -> ThisReturnType {
            if (!std::all_of(
                    typeSpecifiers.begin(), typeSpecifiers.end(),
                    [](const auto pointer) {
                      return std::holds_alternative<
                          Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                          pointer->getVariant());
                    })) {
              return FailureReason("Primitive type specifiers mixed with "
                                   "struct, union, enum and typedef names");
            }
            std::vector<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>
                primitiveTypeSpecifier;
            std::transform(typeSpecifiers.begin(), typeSpecifiers.end(),
                           std::back_inserter(primitiveTypeSpecifier),
                           [](const auto pointer) {
                             return std::get<
                                 Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
                                 pointer->getVariant());
                           });

            return primitivesToType(std::move(primitiveTypeSpecifier), isConst,
                                    isVolatile);
          },
          [&typeSpecifiers, isConst, isVolatile, &typedefs,
           &declarations](const std::unique_ptr<Syntax::StructOrUnionSpecifier>
                              &structOrUnion) -> ThisReturnType {
            if (typeSpecifiers.size() != 1) {
              return FailureReason(
                  std::string("Expected no further type specifiers after ") +
                  (structOrUnion->isUnion() ? "union" : "struct") +
                  " specifier");
            }
            if (structOrUnion->getStructDeclarations().empty()) {
              return RecordType::create(isConst, isVolatile,
                                        structOrUnion->isUnion(),
                                        structOrUnion->getIdentifier());
            }
            std::vector<std::tuple<Type, std::string, std::int64_t>> members;
            for (auto &[structSpecifiers, structDecls] :
                 structOrUnion->getStructDeclarations()) {
              std::vector<SpecifierQualifierRef> refs;
              for (auto &iter : structSpecifiers) {
                std::visit([&refs](auto &&value) { refs.push_back(value); },
                           iter);
              }
              for (auto &iter : structDecls) {
                auto type = declaratorsToType(
                    refs,
                    [&]() -> PossiblyAbstractQualifierRef {
                      if (iter.first) {
                        return *iter.first;
                      } else {
                        return nullptr;
                      }
                    }(),
                    typedefs, declarations);
                if (!type) {
                  return type;
                }
                std::string name =
                    iter.first ? declaratorToName(*iter.first) : "";
                members.emplace_back(std::move(*type), std::move(name),
                                     iter.second);
              }
            }
            return RecordType::create(
                isConst, isVolatile, structOrUnion->isUnion(),
                structOrUnion->getIdentifier(), std::move(members));
          },
          [&typeSpecifiers, isConst, isVolatile](
              const std::unique_ptr<Syntax::EnumSpecifier> &enumSpecifier)
              -> ThisReturnType {
            if (typeSpecifiers.size() != 1) {
              return FailureReason(
                  "Expected no further type specifiers after enum");
            }
            std::vector<std::pair<std::string, std::int32_t>> values;
            auto name = std::visit(
                overload{[](const std::string &name) { return name; },
                         [&values](const Syntax::EnumDeclaration &declaration) {
                           values = declaration.getValues();
                           return declaration.getName();
                         }},
                enumSpecifier->getVariant());
            return EnumType::create(isConst, isVolatile, name,
                                    std::move(values));
          },
          [&](const std::string &typedefName) -> ThisReturnType {
            auto result = typedefs.find(typedefName);
            if (result == typedefs.end()) {
              return FailureReason("Could not find typedef of name " +
                                   typedefName);
            }
            Type copy = result->second;
            copy.setName(typedefName);
            return copy;
          }},
      typeSpecifiers[0]->getVariant());
  if (!primitiveResult) {
    return primitiveResult;
  }
  auto baseType = *primitiveResult;
  if (isRestricted) {
    return FailureReason("Only pointers can be restricted");
  }

  auto getQualifiers =
      [](const std::vector<Syntax::TypeQualifier> &typeQualifiers)
      -> std::tuple<bool, bool, bool> {
    bool isConst = false;
    bool isVolatile = false;
    bool isRestricted = false;
    for (auto &typeQual : typeQualifiers) {
      switch (typeQual) {
      case Syntax::TypeQualifier::Const:
        isConst = true;
        break;
      case Syntax::TypeQualifier::Restrict:
        isRestricted = true;
        break;
      case Syntax::TypeQualifier::Volatile:
        isVolatile = true;
        break;
      default:
        break;
      }
    }
    return std::tie(isConst, isVolatile, isRestricted);
  };

  auto result = std::visit(
      Y{overload{
          [&](auto &&self, const Syntax::AbstractDeclarator *abstractDeclarator)
              -> std::optional<FailureReason> {
            if (!abstractDeclarator) {
              return {};
            }
            for (auto &iter : abstractDeclarator->getPointers()) {
              auto [isConst, isVolatile, isRestricted] =
                  getQualifiers(iter.getTypeQualifiers());
              baseType = PointerType::create(isConst, isVolatile, isRestricted,
                                             std::move(baseType));
            }
            return std::visit(
                Y{overload{
                    [&](auto &&,
                        const std::unique_ptr<Syntax::AbstractDeclarator>
                            &abstractDeclarator)
                        -> std::optional<FailureReason> {
                      // Might need to watch out for 6.7.5.3.11 not sure yet
                      return self(abstractDeclarator.get());
                    },
                    [&](auto &&directSelf,
                        const Syntax::
                            DirectAbstractDeclaratorAssignmentExpression
                                &directAbstractDeclaratorAssignmentExpression)
                        -> std::optional<FailureReason> {
                      if (!directAbstractDeclaratorAssignmentExpression
                               .getAssignmentExpression()) {
                        baseType = AbstractArrayType::create(
                            false, false, false, std::move(baseType));
                      } else {
                        Semantics::ConstantEvaluator evaluator(structOrUnions,
                                                               typedefs);
                        auto result = evaluator.visit(
                            *directAbstractDeclaratorAssignmentExpression
                                 .getAssignmentExpression());
                        if (!result) {
                          baseType = ValArrayType::create(false, false, false,
                                                          std::move(baseType));
                        } else {
                          auto size = std::visit(
                              [](auto &&value) -> std::optional<std::size_t> {
                                using T = std::decay_t<decltype(value)>;
                                if constexpr (std::is_convertible_v<
                                                  T, std::size_t>) {
                                  return value;
                                }
                                return {};
                              },
                              *result);
                          if (!size) {
                            return FailureReason("Invalid type in result of "
                                                 "constant expression");
                          }
                          baseType = ArrayType::create(
                              false, false, false, std::move(baseType), *size);
                        }
                      }
                      if (directAbstractDeclaratorAssignmentExpression
                              .getDirectAbstractDeclarator()) {
                        return std::visit(
                            [&directSelf](
                                auto &&value) -> std::optional<FailureReason> {
                              return directSelf(value);
                            },
                            directAbstractDeclaratorAssignmentExpression
                                .getDirectAbstractDeclarator()
                                ->getVariant());
                      }
                      return {};
                    },
                    [&](auto &&directSelf,
                        const std::unique_ptr<Syntax::DirectAbstractDeclarator>
                            &directAbstractDeclarator)
                        -> std::optional<FailureReason> {
                      baseType = ValArrayType::create(false, false, false,
                                                      std::move(baseType));
                      if (directAbstractDeclarator) {
                        return std::visit(
                            [&directSelf](
                                auto &&value) -> std::optional<FailureReason> {
                              return directSelf(value);
                            },
                            directAbstractDeclarator->getVariant());
                      }
                      return {};
                    },
                    [&](auto &&directSelf,
                        const Syntax::DirectAbstractDeclaratorParameterTypeList
                            &parameterTypeList)
                        -> std::optional<FailureReason> {
                      std::vector<std::pair<Type, std::string>> arguments;
                      if (parameterTypeList.getParameterTypeList()) {
                        auto argumentsResult = parameterListToArguments(
                            parameterTypeList.getParameterTypeList()
                                ->getParameterList()
                                .getParameterDeclarations(),
                            typedefs, declarations, structOrUnions);
                        if (!argumentsResult) {
                          return argumentsResult.error();
                        }
                        arguments = std::move(*argumentsResult);
                      }
                      baseType = FunctionType::create(
                          std::move(baseType), std::move(arguments),
                          !parameterTypeList.getParameterTypeList() ||
                              parameterTypeList.getParameterTypeList()
                                  ->hasEllipse(),
                          true);
                      if (parameterTypeList.getDirectAbstractDeclarator()) {
                        return std::visit(
                            [&directSelf](
                                auto &&value) -> std::optional<FailureReason> {
                              return directSelf(value);
                            },
                            parameterTypeList.getDirectAbstractDeclarator()
                                ->getVariant());
                      }
                      return {};
                    }}},
                abstractDeclarator->getDirectAbstractDeclarator().getVariant());
          },
          [&](auto &&self,
              std::reference_wrapper<const Syntax::Declarator> declarator)
              -> std::optional<FailureReason> {
            for (auto &iter : declarator.get().getPointers()) {
              auto [isConst, isVolatile, isRestricted] =
                  getQualifiers(iter.getTypeQualifiers());
              baseType = PointerType::create(isConst, isVolatile, isRestricted,
                                             std::move(baseType));
            }
            return std::visit(
                Y{overload{
                    [&](auto &&, const std::string &)
                        -> std::optional<FailureReason> { return {}; },
                    [&](auto &&,
                        const std::unique_ptr<Syntax::Declarator> &declarator)
                        -> std::optional<FailureReason> {
                      // Might need to watch out
                      // for 6.7.5.3.11 not sure yet
                      return self(std::cref(*declarator));
                    },
                    [&](auto &&directSelf,
                        const Syntax::DirectDeclaratorNoStaticOrAsterisk
                            &dirWithoutStaticOrAsterisk)
                        -> std::optional<FailureReason> {
                      auto [isConst, isVolatile, isRestricted] = getQualifiers(
                          dirWithoutStaticOrAsterisk.getTypeQualifiers());
                      if (!dirWithoutStaticOrAsterisk
                               .getAssignmentExpression()) {
                        baseType = AbstractArrayType::create(
                            isConst, isVolatile, isRestricted,
                            std::move(baseType));
                      } else {
                        Semantics::ConstantEvaluator evaluator(structOrUnions,
                                                               typedefs);
                        auto result =
                            evaluator.visit(*dirWithoutStaticOrAsterisk
                                                 .getAssignmentExpression());
                        if (!result) {
                          baseType = ValArrayType::create(isConst, isVolatile,
                                                          isRestricted,
                                                          std::move(baseType));
                        } else {
                          auto size = std::visit(
                              [](auto &&value) -> std::optional<std::size_t> {
                                using T = std::decay_t<decltype(value)>;
                                if constexpr (std::is_convertible_v<
                                                  T, std::size_t>) {
                                  return value;
                                }
                                return {};
                              },
                              *result);
                          if (!size) {
                            return FailureReason("Invalid type in result of "
                                                 "constant expression");
                          }
                          baseType = ArrayType::create(
                              isConst, isVolatile, isRestricted,
                              std::move(baseType), *size);
                        }
                      }
                      return std::visit(
                          [&directSelf](
                              auto &&value) -> std::optional<FailureReason> {
                            return directSelf(value);
                          },
                          dirWithoutStaticOrAsterisk.getDirectDeclarator()
                              .getVariant());
                    },
                    [&](auto &&directSelf, const Syntax::DirectDeclaratorStatic
                                               &directDeclaratorStatic)
                        -> std::optional<FailureReason> {
                      auto [isConst, isVolatile, isRestricted] = getQualifiers(
                          directDeclaratorStatic.getTypeQualifiers());
                      Semantics::ConstantEvaluator evaluator(structOrUnions,
                                                             typedefs);
                      auto result = evaluator.visit(
                          directDeclaratorStatic.getAssignmentExpression());
                      if (!result) {
                        baseType = ValArrayType::create(isConst, isVolatile,
                                                        isRestricted,
                                                        std::move(baseType));
                      } else {
                        auto size = std::visit(
                            [](auto &&value) -> std::optional<std::size_t> {
                              using T = std::decay_t<decltype(value)>;
                              if constexpr (std::is_convertible_v<
                                                T, std::size_t>) {
                                return value;
                              }
                              return {};
                            },
                            *result);
                        if (!size) {
                          return FailureReason(
                              "Invalid type in result of constant expression");
                        }
                        baseType =
                            ArrayType::create(isConst, isVolatile, isRestricted,
                                              std::move(baseType), *size);
                      }
                      return std::visit(
                          [&directSelf](
                              auto &&value) -> std::optional<FailureReason> {
                            return directSelf(value);
                          },
                          directDeclaratorStatic.getDirectDeclarator()
                              .getVariant());
                    },
                    [&](auto &&directSelf,
                        const Syntax::DirectDeclaratorAsterisk
                            &directDeclaratorAsterisk)
                        -> std::optional<FailureReason> {
                      auto [isConst, isVolatile, isRestricted] = getQualifiers(
                          directDeclaratorAsterisk.getTypeQualifiers());
                      baseType = ValArrayType::create(isConst, isVolatile,
                                                      isRestricted,
                                                      std::move(baseType));
                      return std::visit(
                          [&directSelf](
                              auto &&value) -> std::optional<FailureReason> {
                            return directSelf(value);
                          },
                          directDeclaratorAsterisk.getDirectDeclarator()
                              .getVariant());
                    },
                    [&](auto &&directSelf,
                        const Syntax::DirectDeclaratorParentParameters
                            &parentheseParameters)
                        -> std::optional<FailureReason> {
                      std::vector<std::pair<Type, std::string>> arguments;
                      auto &parameterDeclarations =
                          parentheseParameters.getParameterTypeList()
                              .getParameterList()
                              .getParameterDeclarations();
                      auto argumentResult = parameterListToArguments(
                          parameterDeclarations, typedefs, declarations,
                          structOrUnions);
                      if (!argumentResult) {
                        return argumentResult.error();
                      }
                      arguments = std::move(*argumentResult);
                      baseType = FunctionType::create(
                          std::move(baseType), std::move(arguments),
                          parentheseParameters.getParameterTypeList()
                              .hasEllipse(),
                          true);
                      return std::visit(
                          [&directSelf](
                              auto &&value) -> std::optional<FailureReason> {
                            return directSelf(value);
                          },
                          parentheseParameters.getDirectDeclarator()
                              .getVariant());
                    },
                    [&](auto &&directSelf,
                        const Syntax::DirectDeclaratorParentIdentifiers
                            &identifiers) -> std::optional<FailureReason> {
                      std::vector<std::pair<Type, std::string>> arguments(
                          identifiers.getIdentifiers().size(),
                          {PrimitiveType::create(false, false, false, true, 32),
                           ""});
                      std::map<std::string, Type> declarationMap;
                      for (auto &iter : declarations) {
                        std::vector<SpecifierQualifierRef> refs;
                        for (auto &specifiers :
                             iter.getDeclarationSpecifiers()) {
                          auto result = std::visit(
                              overload{[](Syntax::StorageClassSpecifier
                                              storageClassSpecifier)
                                           -> std::optional<FailureReason> {
                                         if (storageClassSpecifier !=
                                             Syntax::StorageClassSpecifier::
                                                 Register) {
                                           return FailureReason(
                                               "No storage class specifiers "
                                               "except register allowed for "
                                               "function argument");
                                         }
                                         return {};
                                       },
                                       [&refs](const Syntax::TypeSpecifier
                                                   &typeSpecifier)
                                           -> std::optional<FailureReason> {
                                         refs.emplace_back(typeSpecifier);
                                         return {};
                                       },
                                       [&refs](const Syntax::TypeQualifier
                                                   &typeQualifier)
                                           -> std::optional<FailureReason> {
                                         refs.emplace_back(typeQualifier);
                                         return {};
                                       },
                                       [](Syntax::FunctionSpecifier)
                                           -> std::optional<FailureReason> {
                                         return FailureReason(
                                             "inline keyword not allowed in "
                                             "this context");
                                       }},
                              specifiers);
                          if (result) {
                            return result;
                          }
                        }
                        for (auto &pair : iter.getInitDeclarators()) {
                          if (pair.second) {
                            return FailureReason(
                                "Declarations in function definitions are not "
                                "allowed to have initializers");
                          }
                          auto name = Semantics::declaratorToName(*pair.first);
                          auto result = Semantics::declaratorsToType(
                              refs, *pair.first, typedefs, {}, structOrUnions);
                          if (!result) {
                            return result.error();
                          }
                          declarationMap.emplace(name, *result);
                        }
                      }

                      for (std::size_t i = 0;
                           i < identifiers.getIdentifiers().size(); i++) {
                        auto result = declarationMap.find(
                            identifiers.getIdentifiers()[i]);
                        if (result == declarationMap.end()) {
                          continue;
                        }
                        if (auto *primitive = std::get_if<PrimitiveType>(
                                &result->second.get())) {
                          if (primitive->isFloatingPoint()) {
                            arguments[i] = {PrimitiveType::createDouble(
                                                result->second.isConst(),
                                                result->second.isVolatile()),
                                            result->first};
                          } else if (primitive->getBitCount() == 0) {
                            return FailureReason("Declaration can't have void");
                          } else if (primitive->getBitCount() < 32) {
                            arguments[i] = {PrimitiveType::createInt(
                                                result->second.isConst(),
                                                result->second.isVolatile()),
                                            result->first};
                          } else {
                            arguments[i] = {result->second, result->first};
                          }
                        } else {
                          arguments[i] = {result->second, result->first};
                        }
                        declarationMap.erase(result);
                      }

                      std::string error;
                      for (auto &iter : declarationMap) {
                        error += iter.first;
                      }
                      if (!error.empty()) {
                        return FailureReason(error +
                                             " named in declaration but not as "
                                             "parameter in identifier list");
                      }

                      baseType = FunctionType::create(std::move(baseType),
                                                      std::move(arguments),
                                                      false, false);
                      return std::visit(
                          directSelf,
                          identifiers.getDirectDeclarator().getVariant());
                    }}},
                declarator.get().getDirectDeclarator().getVariant());
          }}},
      declarator);
  if (result) {
    return *result;
  }
  return baseType;
}

std::string Semantics::declaratorToName(const Syntax::Declarator &declarator) {
  return std::visit(
      Y{overload{
          [](auto &&, const std::string &name) -> std::string { return name; },
          [](auto &&self,
             const std::unique_ptr<Syntax::Declarator> &declarator) -> std::string {
            return std::visit(
                [&self](auto &&value) -> std::string { return self(value); },
                declarator->getDirectDeclarator().getVariant());
          },
          [](auto &&self, auto &&value) -> std::string {
            return std::visit(
                [&self](auto &&value) -> std::string { return self(value); },
                value.getDirectDeclarator().getVariant());
          }}},
      declarator.getDirectDeclarator().getVariant());
}

Semantics::RecordType::RecordType(
    std::string name, bool isUnion,
    std::vector<std::tuple<Type, std::string, std::int64_t>> &&names)
    : m_name(std::move(name)), m_isUnion(isUnion), m_members(std::move(names)) {
}

bool Semantics::RecordType::isUnion() const { return m_isUnion; }

const std::vector<std::tuple<Semantics::Type, std::string, std::int64_t>> &
Semantics::RecordType::getMembers() const {
  return m_members;
}

bool Semantics::RecordType::isDefinition() const { return !m_members.empty(); }

Semantics::Type Semantics::RecordType::create(
    bool isConst, bool isVolatile, bool isUnion, const std::string &name,
    std::vector<std::tuple<Semantics::Type, std::string, int64_t>> &&members) {
  return Semantics::Type(isConst, isVolatile,
                         (isUnion ? "union " : "struct ") + name,
                         RecordType(name, isUnion, std::move(members)));
}

bool Semantics::RecordType::operator==(const Semantics::RecordType &rhs) const {
  return std::tie(m_isUnion, m_name) == std::tie(rhs.m_isUnion, rhs.m_name);
}

bool Semantics::RecordType::operator!=(const Semantics::RecordType &rhs) const {
  return !(rhs == *this);
}

const std::string &Semantics::RecordType::getName() const { return m_name; }

Expected<std::size_t, FailureReason>
Semantics::alignmentOf(const Semantics::Type &type) {
  return std::visit(
      overload{
          [](const PrimitiveType &primitiveType)
              -> Expected<std::size_t, FailureReason> {
            return (std::size_t)(primitiveType.getBitCount() / 8);
          },
          [](const ArrayType &arrayType)
              -> Expected<std::size_t, FailureReason> {
            return alignmentOf(arrayType.getType());
          },
          [](const AbstractArrayType &)
              -> Expected<std::size_t, FailureReason> {
            return FailureReason("Incomplete type in sizeof");
          },
          [](const ValArrayType &valArrayType)
              -> Expected<std::size_t, FailureReason> {
            return alignmentOf(valArrayType.getType());
          },
          [](const FunctionType &) -> Expected<std::size_t, FailureReason> {
            return FailureReason(
                "Function type not allowed in sizeof operator");
          },
          [](const RecordType &recordType)
              -> Expected<std::size_t, FailureReason> {
            if (recordType.getMembers().empty()) {
              return FailureReason("Incomplete type in sizeof");
            }
            if (!recordType.isUnion()) {
              std::size_t currentAlignment = 0;
              for (auto &[type, name, bits] : recordType.getMembers()) {
                (void)name;
                (void)bits;
                auto result = alignmentOf(type);
                if (!result) {
                  return result;
                }
                currentAlignment = std::max(currentAlignment, *result);
              }
              return currentAlignment;
            } else {
              std::optional<FailureReason> failure;
              auto result = std::max_element(
                  recordType.getMembers().begin(),
                  recordType.getMembers().end(),
                  [&failure](const auto &lhs, const auto &rhs) {
                    auto lhsSize = sizeOf(std::get<0>(lhs));
                    if (!lhsSize) {
                      failure = lhsSize.error();
                    }
                    auto rhsSize = sizeOf(std::get<0>(rhs));
                    if (!rhsSize) {
                      failure = rhsSize.error();
                    }
                    return (lhsSize ? *lhsSize : 0) < (rhsSize ? *rhsSize : 0);
                  });
              if (failure) {
                return *failure;
              }
              return alignmentOf(std::get<0>(*result));
            }
          },
          [](const EnumType &) -> Expected<std::size_t, FailureReason> {
            return (std::size_t)4ul;
          },
          [](const PointerType &) -> Expected<std::size_t, FailureReason> {
            return (std::size_t)8ul;
          }},
      type.get());
}

bool Semantics::isVoid(const Semantics::Type &type) {
  auto *primitive = std::get_if<PrimitiveType>(&type.get());
  if (!primitive) {
    return false;
  }
  return primitive->getBitCount() == 0;
}

Expected<std::size_t, FailureReason>
Semantics::sizeOf(const Semantics::Type &type) {
  return std::visit(
      overload{
          [](const PrimitiveType &primitiveType)
              -> Expected<std::size_t, FailureReason> {
            return primitiveType.getBitCount() / 8ul;
          },
          [](const ArrayType &arrayType)
              -> Expected<std::size_t, FailureReason> {
            auto result = sizeOf(arrayType.getType());
            if (!result) {
              return result;
            }
            return *result * arrayType.getSize();
          },
          [](const AbstractArrayType &)
              -> Expected<std::size_t, FailureReason> {
            return FailureReason("Incomplete type in sizeof");
          },
          [](const ValArrayType &) -> Expected<std::size_t, FailureReason> {
            return FailureReason(
                "sizeof Val array cannot be determined in constant expression");
          },
          [](const FunctionType &) -> Expected<std::size_t, FailureReason> {
            return FailureReason(
                "Function type not allowed in sizeof operator");
          },
          [](const RecordType &recordType)
              -> Expected<std::size_t, FailureReason> {
            std::size_t currentSize = 0;
            if (recordType.getMembers().empty()) {
              return FailureReason("Incomplete type in sizeof");
            }
            for (auto &[type, name, bits] : recordType.getMembers()) {
              (void)name;
              (void)bits;
              auto alignment = alignmentOf(type);
              if (!alignment) {
                return alignment;
              }
              auto rest = currentSize % *alignment;
              if (rest != 0) {
                currentSize += *alignment - rest;
              }
              auto subSize = sizeOf(type);
              if (!subSize) {
                return subSize;
              }
              currentSize += *subSize;
            }
            return currentSize;
          },
          [](const EnumType &) -> Expected<std::size_t, FailureReason> {
            return 4ul;
          },
          [](const PointerType &) -> Expected<std::size_t, FailureReason> {
            return 8ul;
          }},
      type.get());
}

Semantics::FunctionDefinition::FunctionDefinition(
    FunctionType type, std::string name,
    std::vector<Declaration> parameterDeclarations, Linkage linkage)
    : m_type(std::move(type)), m_name(std::move(name)),
      m_parameterDeclarations(std::move(parameterDeclarations)),
      m_linkage(linkage) {}

const Semantics::FunctionType &Semantics::FunctionDefinition::getType() const {
  return m_type;
}

bool Semantics::FunctionDefinition::hasPrototype() const {
  return m_type.hasPrototype();
}

const std::string &Semantics::FunctionDefinition::getName() const {
  return m_name;
}

Semantics::Linkage Semantics::FunctionDefinition::getLinkage() const {
  return m_linkage;
}

const std::vector<Semantics::Declaration> &
Semantics::FunctionDefinition::getParameterDeclarations() const {
  return m_parameterDeclarations;
}

Semantics::TranslationUnit::TranslationUnit(
    std::vector<TranslationUnit::variant> globals)
    : m_globals(std::move(globals)) {}

const std::vector<Semantics::TranslationUnit::variant> &
Semantics::TranslationUnit::getGlobals() const {
  return m_globals;
}

Semantics::Declaration::Declaration(Semantics::Type type,
                                    Semantics::Linkage linkage,
                                    Semantics::Lifetime lifetime,
                                    std::string name)
    : m_type(std::move(type)), m_linkage(linkage), m_lifetime(lifetime),
      m_name(std::move(name)) {}

const Semantics::Type &Semantics::Declaration::getType() const {
  return m_type;
}

Semantics::Linkage Semantics::Declaration::getLinkage() const {
  return m_linkage;
}

Semantics::Lifetime Semantics::Declaration::getLifetime() const {
  return m_lifetime;
}

const std::string &Semantics::Declaration::getName() const { return m_name; }
} // namespace lcc::Semantics