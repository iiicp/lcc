#include "ConstantEvaluator.h"

#include <algorithm>

namespace {
template <class, class, class = void> struct hasMultiply : std::false_type {};

template <class T1, class T2>
struct hasMultiply<
    T1, T2, std::void_t<decltype(std::declval<T1>() * std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasDivide : std::false_type {};

template <class T1, class T2>
struct hasDivide<T1, T2,
                 std::void_t<decltype(std::declval<T1>() / std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasModulo : std::false_type {};

template <class T1, class T2>
struct hasModulo<T1, T2,
                 std::void_t<decltype(std::declval<T1>() % std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasPlus : std::false_type {};

template <class T1, class T2>
struct hasPlus<T1, T2,
               std::void_t<decltype(std::declval<T1>() + std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasMinus : std::false_type {};

template <class T1, class T2>
struct hasMinus<T1, T2,
                std::void_t<decltype(std::declval<T1>() - std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasRShift : std::false_type {};

template <class T1, class T2>
struct hasRShift<
    T1, T2, std::void_t<decltype(std::declval<T1>() >> std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasLShift : std::false_type {};

template <class T1, class T2>
struct hasLShift<
    T1, T2, std::void_t<decltype(std::declval<T1>() << std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasLT : std::false_type {};

template <class T1, class T2>
struct hasLT<T1, T2,
             std::void_t<decltype(std::declval<T1>() < std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasLE : std::false_type {};

template <class T1, class T2>
struct hasLE<T1, T2,
             std::void_t<decltype(std::declval<T1>() <= std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasGT : std::false_type {};

template <class T1, class T2>
struct hasGT<T1, T2,
             std::void_t<decltype(std::declval<T1>() > std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasGE : std::false_type {};

template <class T1, class T2>
struct hasGE<T1, T2,
             std::void_t<decltype(std::declval<T1>() >= std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasEQ : std::false_type {};

template <class T1, class T2>
struct hasEQ<T1, T2,
             std::void_t<decltype(std::declval<T1>() == std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasNE : std::false_type {};

template <class T1, class T2>
struct hasNE<T1, T2,
             std::void_t<decltype(std::declval<T1>() != std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasBitAnd : std::false_type {};

template <class T1, class T2>
struct hasBitAnd<T1, T2,
                 std::void_t<decltype(std::declval<T1>() & std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasBitXor : std::false_type {};

template <class T1, class T2>
struct hasBitXor<T1, T2,
                 std::void_t<decltype(std::declval<T1>() ^ std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasBitOr : std::false_type {};

template <class T1, class T2>
struct hasBitOr<T1, T2,
                std::void_t<decltype(std::declval<T1>() | std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasLogicAnd : std::false_type {};

template <class T1, class T2>
struct hasLogicAnd<
    T1, T2, std::void_t<decltype(std::declval<T1>() && std::declval<T2>())>>
    : std::true_type {};

template <class, class, class = void> struct hasLogicOr : std::false_type {};

template <class T1, class T2>
struct hasLogicOr<
    T1, T2, std::void_t<decltype(std::declval<T1>() || std::declval<T2>())>>
    : std::true_type {};

template <class, class = void> struct hasLogicNegate : std::false_type {};

template <class T>
struct hasLogicNegate<T, std::void_t<decltype(!std::declval<T>())>>
    : std::true_type {};

template <class, class = void> struct hasBitNegate : std::false_type {};

template <class T>
struct hasBitNegate<T, std::void_t<decltype(~std::declval<T>())>>
    : std::true_type {};

template <class, class = void> struct hasNegate : std::false_type {};

template <class T>
struct hasNegate<T, std::void_t<decltype(-std::declval<T>())>>
    : std::true_type {};
} // namespace

namespace lcc::Semantics {
Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PrimaryExprConstant &node) {
  return std::visit(
      [](auto &&value) -> Semantics::ConstRetType {
        using T = std::decay_t<decltype(value)>;
        if constexpr (!std::is_same_v<T, std::string>) {
          return Semantics::ConstRetType::ValueType(value);
        } else {
          return FailureReason(
              "Can't use string literal in constant expression");
        }
      },
      node.getValue());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PrimaryExprParent &node) {
  return visit(node.getExpr());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PostFixExprPrimary &node) {
  return visit(node.getPrimaryExpr());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::UnaryExprPostFixExpr &node) {
  return visit(node.getPostExpr());
}

Semantics::ConstRetType Semantics::ConstantEvaluator::visit(
    const Syntax::UnaryExprUnaryOperator &node) {
  auto value = visit(node.getUnaryExpr());
  if (!value) {
    return value;
  }
  switch (node.getOperator()) {
  case tok::plus_plus:
  case tok::minus_minus:
  case tok::amp:
  case tok::star:
    return FailureReason("Unary Operator not allowed in constant expression");
  case tok::plus:
    return value;
  case tok::minus: {
    return std::visit(
        [](auto &&value) -> ConstRetType {
          using T = std::decay_t<decltype(value)>;
          if constexpr (hasNegate<T>{}) {
            return Semantics::ConstRetType::ValueType(-value);
          } else {
            return FailureReason("Can't apply - to constant operator");
          }
        },
        *value);
  }
  case tok::tilde: {
    return std::visit(
        [](auto &&value) -> ConstRetType {
          using T = std::decay_t<decltype(value)>;
          if constexpr (hasBitNegate<T>{}) {
            return Semantics::ConstRetType::ValueType(~value);
          } else {
            return FailureReason("Can't apply - to constant operator");
          }
        },
        *value);
  }
  case tok::exclaim: {
    return std::visit(
        [](auto &&value) -> ConstRetType {
          using T = std::decay_t<decltype(value)>;
          if constexpr (hasLogicNegate<T>{}) {
            return Semantics::ConstRetType::ValueType(!value);
          } else {
            return FailureReason("Can't apply - to constant operator");
          }
        },
        *value);
  }
  default:
    return FailureReason("Can't apply - to constant operator");
  }
  return value;
}

namespace {
template <typename G> struct Y {
  template <typename... X> decltype(auto) operator()(X &&...x) const & {
    return g(*this, std::forward<X>(x)...);
  }

  G g;
};

template <typename G> Y(G) -> Y<G>;

template <class... Ts> struct overload : Ts... {
  using Ts::operator()...;
};
template <class... Ts> overload(Ts...) -> overload<Ts...>;
} // namespace

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::UnaryExprSizeOf &node) {
  return std::visit(
      overload{[this](const std::unique_ptr<Syntax::TypeName> &typeName)
                   -> Semantics::ConstRetType {
                 std::vector<Semantics::SpecifierQualifierRef> refs;
                 for (auto &iter : typeName->getSpecifierQualifiers()) {
                   std::visit(
                       [&refs](auto &&value) {
                         refs.emplace_back(std::cref(value));
                       },
                       iter);
                 }
                 auto type = Semantics::declaratorsToType(
                     refs, typeName->getAbstractDeclarator(), m_typedefs, {},
                     m_structOrUnions);
                 if (!type) {
                   return type;
                 }
                 auto result = Semantics::sizeOf(*type);
                 if (!result) {
                   return result;
                 }
                 return *result;
               },
               [](auto &&) -> Semantics::ConstRetType {
                 throw std::runtime_error("Not implemented yet");
               }},
      node.getVariant());
}

namespace {
template <class T>
Semantics::ConstRetType castVariant(const Semantics::ConstRetType &variant) {
  return std::visit(
      [](auto &&value) -> Semantics::ConstRetType {
        using U = std::decay_t<decltype(value)>;
        if constexpr (std::is_convertible_v<U, T>) {
          return static_cast<T>(value);
        } else {
          return FailureReason("Invalid constant cast");
        }
      },
      variant);
}
} // namespace

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::CastExpr &node) {
  return std::visit(
      [this](auto &&value) -> Semantics::ConstRetType {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, Syntax::UnaryExpr>) {
          return visit(value);
        } else {
          //                                  value.second->accept(*this);
          //                                  auto old =
          //                                  std::get<ConstRetType>(m_return);
          //                                  if
          //                                  (std::shared_ptr<Syntax::PrimitiveType>
          //                                          primitive =
          //                                          std::dynamic_pointer_cast<Syntax::PrimitiveType>(value
          //                                                                                                           .first);primitive)
          //                                  {
          //                                      if
          //                                      (primitive->isFloatingPoint())
          //                                      {
          //                                          if
          //                                          (primitive->getBitCount()
          //                                          == 32)
          //                                          {
          //                                              return
          //                                              castVariant<float>(old);
          //                                          }
          //                                          else if
          //                                          (primitive->getBitCount()
          //                                          == 64)
          //                                          {
          //                                              return
          //                                              castVariant<double>(old);
          //                                          }
          //                                          else
          //                                          {
          //                                              throw
          //                                              std::runtime_error("Invalid
          //                                              bitcount for floating
          //                                              point type");
          //                                          }
          //                                      }
          //                                      else
          //                                      {
          //                                          switch
          //                                          (primitive->getBitCount())
          //                                          {
          //                                          case 8:
          //                                              if
          //                                              (primitive->isSigned())
          //                                              {
          //                                                  return
          //                                                  castVariant<std::int8_t>(old);
          //                                              }
          //                                              else
          //                                              {
          //                                                  return
          //                                                  castVariant<std::uint8_t>(old);
          //                                              }
          //                                          case 16:
          //                                              if
          //                                              (primitive->isSigned())
          //                                              {
          //                                                  return
          //                                                  castVariant<std::int16_t>(old);
          //                                              }
          //                                              else
          //                                              {
          //                                                  return
          //                                                  castVariant<std::uint16_t>(old);
          //                                              }
          //                                          case 32:
          //                                              if
          //                                              (primitive->isSigned())
          //                                              {
          //                                                  return
          //                                                  castVariant<std::int32_t>(old);
          //                                              }
          //                                              else
          //                                              {
          //                                                  return
          //                                                  castVariant<std::uint32_t>(old);
          //                                              }
          //                                          case 64:
          //                                              if
          //                                              (primitive->isSigned())
          //                                              {
          //                                                  return
          //                                                  castVariant<std::int64_t>(old);
          //                                              }
          //                                              else
          //                                              {
          //                                                  return
          //                                                  castVariant<std::uint64_t>(old);
          //                                              }
          //                                          default:throw
          //                                          std::runtime_error("Invalid
          //                                          bitcount for integer
          //                                          type");
          //                                          }
          //                                      }
          //                                  }
          //                                  else
          { throw std::runtime_error("Not implemented yet"); }
        }
      },
      node.getVariant());
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::MultiExpr &node) {
  if (node.getOptionalCastExpr().empty()) {
    return visit(node.getCastExpr());
  }
  auto value = visit(node.getCastExpr());
  for (auto &[op, exp] : node.getOptionalCastExpr()) {
    if (!value) {
      return value;
    }
    switch (op) {
    case tok::star: {
      value = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasMultiply<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs * rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *value);
    } break;
    case tok::slash: {
      value = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasDivide<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs / rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *value);
    } break;
    case tok::percent: {
      value = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasModulo<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs % rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *value);
    } break;
    }
  }
  return value;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::AdditiveExpr &node) {
  if (node.getOptionalMultiExpr().empty()) {
    return visit(node.getMultiExpr());
  }
  auto currentValue = visit(node.getMultiExpr());
  for (auto &[op, exp] : node.getOptionalMultiExpr()) {
    if (!currentValue) {
      return currentValue;
    }
    switch (op) {
    case tok::plus: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasPlus<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs + rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::minus: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (!std::is_void_v<std::remove_pointer_t<T1>> ||
                                !std::is_void_v<std::remove_pointer_t<T2>>) {
                    if constexpr (hasMinus<T1, T2>{}) {
                      return ConstRetType::ValueType(lhs - rhs);
                    } else {
                      return FailureReason("Can't apply minux to operands in "
                                           "constant expression");
                    }
                  } else {
                    return FailureReason(
                        "Can't apply minux to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    }
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::ShiftExpr &node) {
  if (node.getOptAdditiveExps().empty()) {
    return visit(node.getAdditiveExpr());
  }
  auto currentValue = visit(node.getAdditiveExpr());
  for (auto &[op, exp] : node.getOptAdditiveExps()) {
    if (!currentValue) {
      return currentValue;
    }
    switch (op) {
    case tok::less: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return *second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasLShift<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs << rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::greater: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasRShift<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs >> rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    }
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::BitAndExpr &node) {
  if (node.getOptionalEqualExpr().empty()) {
    return visit(node.getEqualExpr());
  }
  auto currentValue = visit(node.getEqualExpr());
  for (auto &exp : node.getOptionalEqualExpr()) {
    if (!currentValue) {
      return currentValue;
    }
    currentValue = std::visit(
        [&exp, this](auto &&lhs) -> ConstRetType {
          using T1 = std::decay_t<decltype(lhs)>;
          auto second = visit(exp);
          if (second) {
            return second;
          }
          return std::visit(
              [lhs](auto &&rhs) -> ConstRetType {
                using T2 = std::decay_t<decltype(rhs)>;
                if constexpr (hasBitAnd<T1, T2>{}) {
                  return ConstRetType::ValueType(lhs & rhs);
                } else {
                  return FailureReason(
                      "Can't apply plus to operands in constant expression");
                }
              },
              *second);
        },
        *currentValue);
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::BitXorExpr &node) {
  if (node.getOptionalBitAndExpressions().empty()) {
    return visit(node.getBitAndExpr());
  }
  auto currentValue = visit(node.getBitAndExpr());
  for (auto &exp : node.getOptionalBitAndExpressions()) {
    if (!currentValue) {
      return currentValue;
    }
    currentValue = std::visit(
        [&exp, this](auto &&lhs) -> ConstRetType {
          using T1 = std::decay_t<decltype(lhs)>;
          auto second = visit(exp);
          return std::visit(
              [lhs](auto &&rhs) -> ConstRetType {
                using T2 = std::decay_t<decltype(rhs)>;
                if constexpr (hasBitXor<T1, T2>{}) {
                  return ConstRetType::ValueType(lhs ^ rhs);
                } else {
                  return FailureReason(
                      "Can't apply plus to operands in constant expression");
                }
              },
              *second);
        },
        *currentValue);
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::BitOrExpr &node) {
  if (node.getOptionalBitXorExpressions().empty()) {
    return visit(node.getBitXorExpression());
  }
  auto currentValue = visit(node.getBitXorExpression());
  for (auto &exp : node.getOptionalBitXorExpressions()) {
    if (!currentValue) {
      return currentValue;
    }
    currentValue = std::visit(
        [&exp, this](auto &&lhs) -> ConstRetType {
          using T1 = std::decay_t<decltype(lhs)>;
          auto second = visit(exp);
          if (!second) {
            return second;
          }
          return std::visit(
              [lhs](auto &&rhs) -> ConstRetType {
                using T2 = std::decay_t<decltype(rhs)>;
                if constexpr (hasBitOr<T1, T2>{}) {
                  return ConstRetType::ValueType(lhs | rhs);
                } else {
                  return FailureReason(
                      "Can't apply plus to operands in constant expression");
                }
              },
              *second);
        },
        *currentValue);
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::LogAndExpr &node) {
  if (node.getOptionalBitOrExpressions().empty()) {
    return visit(node.getBitOrExpression());
  }
  auto currentValue = visit(node.getBitOrExpression());
  for (auto &exp : node.getOptionalBitOrExpressions()) {
    if (!currentValue) {
      return currentValue;
    }
    currentValue = std::visit(
        [&exp, this](auto &&lhs) -> ConstRetType {
          using T1 = std::decay_t<decltype(lhs)>;
          auto second = visit(exp);
          if (!second) {
            return second;
          }
          return std::visit(
              [lhs](auto &&rhs) -> ConstRetType {
                using T2 = std::decay_t<decltype(rhs)>;
                if constexpr (hasLogicAnd<T1, T2>{}) {
                  return ConstRetType::ValueType(lhs && rhs);
                } else {
                  return FailureReason(
                      "Can't apply plus to operands in constant expression");
                }
              },
              *second);
        },
        *currentValue);
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::LogOrExpr &node) {
  if (node.getOptionalAndExpressions().empty()) {
    return visit(node.getAndExpression());
  }
  auto currentValue = visit(node.getAndExpression());
  for (auto &exp : node.getOptionalAndExpressions()) {
    if (!currentValue) {
      return currentValue;
    }
    currentValue = std::visit(
        [&exp, this](auto &&lhs) -> ConstRetType {
          using T1 = std::decay_t<decltype(lhs)>;
          auto second = visit(exp);
          if (!second) {
            return second;
          }
          return std::visit(
              [lhs](auto &&rhs) -> ConstRetType {
                using T2 = std::decay_t<decltype(rhs)>;
                if constexpr (hasLogicOr<T1, T2>{}) {
                  return ConstRetType::ValueType(lhs || rhs);
                } else {
                  return FailureReason(
                      "Can't apply plus to operands in constant expression");
                }
              },
              *second);
        },
        *currentValue);
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::ConditionalExpr &node) {
  if (node.getOptionalExpression() && node.getOptionalConditionalExpression()) {
    auto value = visit(node.getLogicalOrExpression());
    if (!value) {
      return value;
    }
    if (std::visit([](auto &&value) -> bool { return value; }, *value)) {
      return visit(*node.getOptionalExpression());
    } else {
      return visit(*node.getOptionalConditionalExpression());
    }
  } else {
    return visit(node.getLogicalOrExpression());
  }
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-compare"

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::RelationalExpr &node) {
  if (node.getOptionalShiftExpressions().empty()) {
    return visit(node.getShiftExpr());
  }
  auto currentValue = visit(node.getShiftExpr());
  for (auto &[op, exp] : node.getOptionalShiftExpressions()) {
    if (!currentValue) {
      return currentValue;
    }
    switch (op) {
    case tok::greater: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasGT<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs > rhs);
                  } else {
                    return FailureReason(
                        "Can't > to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::greater_equal: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasGE<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs > rhs);
                  } else {
                    return FailureReason(
                        "Can't apply >= to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::less: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasLT<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs < rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::less_equal: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasLE<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs <= rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    }
  }
  return currentValue;
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::EqualExpr &node) {
  if (node.getOptionalRelationalExpr().empty()) {
    return visit(node.getRelationalExpr());
  }
  auto currentValue = visit(node.getRelationalExpr());
  for (auto &[op, exp] : node.getOptionalRelationalExpr()) {
    if (!currentValue) {
      return currentValue;
    }

    switch (op) {
    case tok::equal_equal: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasEQ<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs == rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    case tok::pipe_equal: {
      currentValue = std::visit(
          [exp = &exp, this](auto &&lhs) -> ConstRetType {
            using T1 = std::decay_t<decltype(lhs)>;
            auto second = visit(*exp);
            if (!second) {
              return second;
            }
            return std::visit(
                [lhs](auto &&rhs) -> ConstRetType {
                  using T2 = std::decay_t<decltype(rhs)>;
                  if constexpr (hasNE<T1, T2>{}) {
                    return ConstRetType::ValueType(lhs != rhs);
                  } else {
                    return FailureReason(
                        "Can't apply plus to operands in constant expression");
                  }
                },
                *second);
          },
          *currentValue);
    } break;
    }
  }
  return currentValue;
}

#pragma GCC diagnostic pop
#pragma GCC diagnostic pop

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PostFixExprSubscript &) {
  throw std::runtime_error("Not implemented yet");
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PostFixExprArrow &) {
  throw std::runtime_error("Not implemented yet");
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PostFixExprDot &) {
  throw std::runtime_error("Not implemented yet");
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PrimaryExpr &node) {
  return std::visit(
      overload{[this](auto &&value) -> ConstRetType { return visit(value); },
               [](const Syntax::PrimaryExprIdentifier &) -> ConstRetType {
                 return FailureReason(
                     "Identifier not allowed in constant expression");
               }},
      node.getVariant());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::PostFixExpr &node) {
  return std::visit(
      overload{[this](auto &&value) -> ConstRetType { return visit(value); },
               [](const Syntax::PostFixExprFuncCall &) -> ConstRetType {
                 return FailureReason(
                     "Function call not allowed in constant expression");
               },
               [](const Syntax::PostFixExprIncrement &) -> ConstRetType {
                 return FailureReason(
                     "Increment not allowed in constant expression");
               },
               [](const Syntax::PostFixExprDecrement &) -> ConstRetType {
                 return FailureReason(
                     "Decrement not allowed in constant expression");
               }},
      node.getVariant());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::UnaryExpr &node) {
  return std::visit(
      [this](auto &&value) -> Semantics::ConstRetType { return visit(value); },
      node.getVariant());
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::Expr &node) {
  if (node.getAssignExpressions().size() != 1) {
    return FailureReason(", operator not allowed in constant expression");
  }
  return visit(node.getAssignExpressions()[0]);
}

Semantics::ConstRetType
Semantics::ConstantEvaluator::visit(const Syntax::AssignExpr &node) {
  return std::visit(
      [this](auto &&value) -> Semantics::ConstRetType {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<Syntax::ConditionalExpr, T>) {
          return visit(value);
        } else {
          return FailureReason("assignment not allowed in constant expression");
        }
      },
      node.getVariant());
}

Semantics::ConstantEvaluator::ConstantEvaluator(
    const std::map<std::string, Semantics::RecordType> &structOrUnions,
    const std::map<std::string, std::reference_wrapper<const Semantics::Type>>
        &typedefs)
    : m_structOrUnions(structOrUnions), m_typedefs(typedefs) {}
} // namespace lcc::Semantics
