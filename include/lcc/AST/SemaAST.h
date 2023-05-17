/***********************************
 * File:     SemaNode.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/5/4
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SEMAAST_H
#define LCC_SEMAAST_H
#include "lcc/Basic/Box.h"
#include "lcc/Sema/Type.h"
#include <string>

namespace lcc::SemaSyntax {

class Expression;

class Constant final {
public:
  using Variant = std::variant<int32_t, uint32_t, int64_t, uint64_t, float,
                               double, std::string>;

private:
  Variant value_;

public:
  Constant(Variant value) : value_(value) {}

  DECL_GETTER(Variant, value);
};

class Conversion final {
public:
  enum Kind {
    LValue,
    IntegerPromotion,
    ArithmeticConversion,
    DefaultArgumentPromotion,
    Implicit
  };

private:
  Kind kind_;
  box<Expression> expression_;

public:
  Conversion(Kind kind, box<Expression> &&expression)
      : kind_(kind), expression_(std::move(expression)) {}

  DECL_GETTER(Kind, kind);
  DECL_GETTER(const box<Expression> &, expression);
};

class Cast final {
private:
  std::shared_ptr<Type> newType_;
  box<Expression> expression_;

public:
  Cast(std::shared_ptr<Type> type, box<Expression> &&expression)
      : newType_(type), expression_(std::move(expression)) {}

  DECL_GETTER(std::shared_ptr<Type>, newType);
  DECL_GETTER(const box<Expression> &, expression);
};

class MemberAccess final {
private:
  box<Expression> recordExpr_;
  uint64_t memberIndex_;

public:
  MemberAccess(box<Expression> &&recordExpr, uint64_t memberIndex)
      : recordExpr_(std::move(recordExpr)), memberIndex_(memberIndex) {}
  DECL_GETTER(const box<Expression> &, recordExpr);
  DECL_GETTER(uint64_t, memberIndex);
};

class SubscriptOperator final {
private:
  box<Expression> leftExpr_;
  box<Expression> rightExpr_;

public:
  SubscriptOperator(box<Expression> &&leftExpr, box<Expression> &&rightExpr)
      : leftExpr_(std::move(leftExpr)), rightExpr_(std::move(rightExpr)) {}

  DECL_GETTER(const box<Expression> &, leftExpr);
  DECL_GETTER(const box<Expression> &, rightExpr);
};

class CallExpression final {
private:
  box<Expression> funcExpr_;
  std::vector<box<Expression>> argumentExprs_;

public:
  CallExpression(box<Expression> &&funcExpr,
                 std::vector<box<Expression>> &&argumentExprs)
      : funcExpr_(std::move(funcExpr)),
        argumentExprs_(std::move(argumentExprs)) {}
  DECL_GETTER(const box<Expression> &, funcExpr);
  DECL_GETTER(const std::vector<box<Expression>> &, argumentExprs);
};

class BinaryOperator final {
public:
  enum Kind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LeftShift,
    RightShift,
    LT,
    GT,
    LE,
    GE,
    Eq,
    NotEq,
    BitOr,
    BitAnd,
    BitXor,
    LogicAnd,
    LogicOr
  };

private:
  Kind kind_;
  box<Expression> leftOperand_;
  box<Expression> rightOperand_;

public:
  BinaryOperator(box<Expression> &&leftOperand, Kind kind,
                 box<Expression> &&rightOperand)
      : kind_(kind), leftOperand_(std::move(leftOperand)),
        rightOperand_(std::move(rightOperand)) {}
  DECL_GETTER(Kind, kind);
  DECL_GETTER(const box<Expression> &, leftOperand);
  DECL_GETTER(const box<Expression> &, rightOperand);
};

class UnaryOperator final {
public:
  enum Kind {
    AddressOf,
    Dereference,
    PostIncrement,
    PostDecrement,
    PreIncrement,
    PreDecrement,
    Plus,
    Minus,
    LogicNeg,
    BitNeg
  };

private:
  Kind kind_;
  box<Expression> operand_;

public:
  UnaryOperator(Kind kind, box<Expression> &&operand)
      : kind_(kind), operand_(std::move(operand)) {}
  DECL_GETTER(Kind, kind);
  DECL_GETTER(const box<Expression> &, operand);
};

class SizeOfOperator final {
public:
  using Variant = std::variant<box<Expression>, std::shared_ptr<Type>>;

private:
  Variant variant_;

public:
  SizeOfOperator(Variant variant) : variant_(std::move(variant)) {}
  DECL_GETTER(const Variant &, variant);
};

class Conditional final {
private:
  box<Expression> boolExpr_;
  box<Expression> trueExpr_;
  box<Expression> falseExpr_;

public:
  Conditional(box<Expression> &&boolExpr, box<Expression> &&trueExpr,
              box<Expression> &&falseExpr)
      : boolExpr_(std::move(boolExpr)), trueExpr_(std::move(trueExpr)),
        falseExpr_(std::move(falseExpr)) {}

  DECL_GETTER(const box<Expression> &, boolExpr);
  DECL_GETTER(const box<Expression> &, trueExpr);
  DECL_GETTER(const box<Expression> &, falseExpr);
};

class Assignment final {
public:
  enum Kind {
    Simple,
    PlusAssign,
    MinusAssign,
    DivAssign,
    MulAssign,
    ModAssign,
    LeftShiftAssign,
    RightShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign
  };

private:
  Kind kind_;
  box<Expression> leftOperand_;
  box<Expression> rightOperand_;

public:
  Assignment(Kind kind, box<Expression> &&leftOperand,
             box<Expression> &&rightOperand)
      : kind_(kind), leftOperand_(std::move(leftOperand)),
        rightOperand_(std::move(rightOperand)) {}

  DECL_GETTER(Kind, kind);
  DECL_GETTER(const box<Expression> &, leftOperand);
  DECL_GETTER(const box<Expression> &, rightOperand);
};

class CommaExpression final {
private:
  std::vector<box<Expression>> commaExprs_;
  box<Expression> lastExpr_;

public:
  CommaExpression(std::vector<box<Expression>> &&commaExprs,
                  box<Expression> &&lastExpr)
      : commaExprs_(std::move(commaExprs)), lastExpr_(std::move(lastExpr)) {}
  DECL_GETTER(const std::vector<box<Expression>> &, commaExprs);
  DECL_GETTER(const box<Expression> &, lastExpr);
};

enum class ValueCategory : uint8_t { LValue, RValue };

class Expression final {
private:
  std::shared_ptr<Type> type_;
  ValueCategory valueCategory_;

public:
  using Variant =
      std::variant<std::monostate, Constant, Conversion, MemberAccess,
                   SubscriptOperator, CallExpression, BinaryOperator,
                   UnaryOperator, Cast, SizeOfOperator, Assignment,
                   CommaExpression, Conditional>;

private:
  Variant expression_;

public:
  Expression(std::shared_ptr<Type> type, ValueCategory valueCategory,
             Variant expression)
      : type_(type), valueCategory_(valueCategory),
        expression_(std::move(expression)) {}
  Expression(const Expression &) = delete;
  Expression &operator=(const Expression &) = delete;
  Expression(Expression &&) = default;
  Expression &operator=(Expression &&) = default;

  DECL_GETTER(std::shared_ptr<Type>, type);
  DECL_GETTER(ValueCategory, valueCategory);
  DECL_GETTER(const Variant &, expression);

  bool isUndefined() const {
    return std::holds_alternative<std::monostate>(expression_);
  }
};

class ExpressionStatement final {
private:
  std::optional<Expression> expression_;

public:
  explicit ExpressionStatement(std::optional<Expression> expression)
      : expression_(std::move(expression)) {}
};

class ReturnStatement final {
private:
  std::optional<Expression> expression_;

public:
  explicit ReturnStatement(std::optional<Expression> expression)
      : expression_(std::move(expression)) {}
};

class CompoundStatement final {
public:
  using Variant = std::variant<ExpressionStatement, ReturnStatement>;

private:
  int64_t scope_;
  std::vector<Variant> compoundItems_;

public:
  CompoundStatement(std::int64_t scope, std::vector<Variant> &&compoundItems)
      : scope_(scope), compoundItems_(std::move(compoundItems)) {}

  CompoundStatement(const CompoundStatement &) = delete;
  CompoundStatement &operator=(const CompoundStatement &) = delete;

  CompoundStatement(CompoundStatement &&) noexcept = default;
  CompoundStatement &operator=(CompoundStatement &&) noexcept = default;

  DECL_GETTER(const std::vector<Variant> &, compoundItems);
  DECL_GETTER(int64_t, scope);
};

enum class Linkage : uint8_t { Internal, External, None };

enum class Lifetime : uint8_t { Automatic, Static, Register };

class Declaration final {
public:
  enum Kind { DeclarationOnly, TentativeDefinition, Definition };

private:
  std::shared_ptr<Type> type_;
  Linkage linkage_;
  Lifetime lifetime_;
  Kind kind_;

public:
  Declaration(std::shared_ptr<Type> type, Linkage linkage, Lifetime lifetime,
              Kind kind)
      : type_(type), linkage_(linkage), lifetime_(lifetime), kind_(kind) {}

  DECL_GETTER(std::shared_ptr<Type>, type);
  DECL_GETTER(Linkage, linkage);
  DECL_GETTER(Lifetime, lifetime);
  DECL_GETTER(Kind, kind);
};

class FunctionDefinition final {
private:
  std::shared_ptr<Type> type_;
  std::vector<Declaration> paramDecls_;
  Linkage linkage_;
  CompoundStatement compoundStatement_;

public:
  FunctionDefinition(std::shared_ptr<Type> type,
                     std::vector<Declaration> &&paramDecls, Linkage linkage,
                     CompoundStatement &&compoundStatement)
      : type_(type), paramDecls_(MV_(paramDecls)), linkage_(linkage),
        compoundStatement_(MV_(compoundStatement)) {}

  DECL_GETTER(std::shared_ptr<Type>, type);
  DECL_GETTER(const std::vector<Declaration> &, paramDecls);
  DECL_GETTER(Linkage, linkage);
  DECL_GETTER(const CompoundStatement &, compoundStatement);
};

class TranslationUnit final {
public:
  using Variant = std::variant<FunctionDefinition, Declaration>;

private:
  std::vector<Variant> globals_;

public:
  explicit TranslationUnit(std::vector<Variant> &&globals)
      : globals_(MV_(globals)) {}

  const std::vector<Variant> &getGlobals() const { return globals_; }
};

} // namespace lcc::SemaSyntax
#endif // LCC_SEMAAST_H
