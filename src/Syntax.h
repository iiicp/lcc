/***********************************
 * File:     Syntax.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/13
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SYNTAX_H
#define LCC_SYNTAX_H
#include "Token.h"
#include <optional>
#include <string>
#include <variant>
#include <vector>
namespace lcc::Syntax {
class PrimaryExprIdent;
class PrimaryExprConstant;
class PrimaryExprParent;
class PostFixExprSubscript;
class PostFixExprIncrement;
class PostFixExprDecrement;
class PostFixExprDot;
class PostFixExprArrow;
class PostFixExprFuncCall;
class PostFixExprPrimaryExpr;
class PostFixExprTypeInitializer;
class UnaryExprPostFixExpr;
class UnaryExprUnaryOperator;
class UnaryExprSizeOf;
class CastExpr;
class MultiExpr;
class AdditiveExpr;
class ShiftExpr;
class RelationalExpr;
class EqualExpr;
class BitAndExpr;
class BitXorExpr;
class BitOrExpr;
class LogAndExpr;
class LogOrExpr;
class ConditionalExpr;
using ConstantExpr = ConditionalExpr;
class AssignExpr;
class AssignExprAssign;
class Expr;

class ReturnStmt;
class ExprStmt;
class IfStmt;
class SwitchStmt;
class DefaultStmt;
class CaseStmt;
class BlockStmt;
class ForStmt;
class DoWhileStmt;
class WhileStmt;
class BreakStmt;
class ContinueStmt;
class GotoStmt;
class LabelStmt;

class TranslationUnit;
class FunctionDefinition;
class Declaration;
class StorageClassSpecifier;
class TypeQualifier;
class TypeSpecifier;
class EnumSpecifier;
class EnumeratorList;
class StructOrUnionSpecifier;
class Declarator;
class DirectDeclaratorIdent;
class DirectDeclaratorParent;
class DirectDeclaratorAssignExpr;
class DirectDeclaratorParentParamTypeList;
class TypeName;
class AbstractDeclarator;
class DirectAbstractDeclaratorParent;
class DirectAbstractDeclaratorParamTypeList;
class DirectAbstractDeclaratorAssignExpr;
class Pointer;
class ParamTypeList;
class ParamList;
class InitializerList;
class Initializer;

class Node {
public:
  Node(){};
  virtual ~Node() = default;
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
  Node(Node &&) = default;
  Node &operator=(Node &&) = default;
};

/**
 expression:
    assignment-expression
    expression , assignment-expression
 */
class Expr final : public Node {
private:
  std::vector<AssignExpr> mAssignExpressions;

public:
  Expr(std::vector<AssignExpr> assignExpressions)
      : mAssignExpressions(std::move(assignExpressions)) {}
  [[nodiscard]] const std::vector<AssignExpr> &getAssignExpressions() const {
    return mAssignExpressions;
  }
};

/*
 * primary-expression:
 *    identifier
 */
class PrimaryExprIdent final : public Node {
private:
  std::string mIdent;

public:
  PrimaryExprIdent(std::string identifier) : mIdent(std::move(identifier)){};
  [[nodiscard]] const std::string &getIdentifier() const { return mIdent; }
};

/*
 * primary-expression:
 *    constant
 */
class PrimaryExprConstant final : public Node {
public:
  using Variant = std::variant<std::int32_t, std::uint32_t, std::int64_t,
                               std::uint64_t, float, double, std::string>;

private:
  Variant mValue;

public:
  PrimaryExprConstant(Variant variant) : mValue(std::move(variant)){};
  [[nodiscard]] const Variant &getValue() const { return mValue; }
};

/*
 * primary-expression:
 *    ( expression )
 */
class PrimaryExprParent final : public Node {
private:
  Expr mExpr;

public:
  PrimaryExprParent(Expr &&expr) : mExpr(std::move(expr)){};
  [[nodiscard]] const Expr &getExpr() const { return mExpr; }
};

/*
 * primary-expression:
 *    identifier
 *    constant
 *    string-literal
 *    ( expression )
 */
using PrimaryExpr =
    std::variant<PrimaryExprIdent, PrimaryExprConstant, PrimaryExprParent>;

/**
 * postfix-expression:
 *    primary-expression
 *    postfix-expression [ expression ]
 *    postfix-expression ( argument-expression-list{opt} )
 *    postfix-expression . identifier
 *    postfix-expression -> identifier
 *    postfix-expression ++
 *    postfix-expression --
 *    ( type-name ) { initializer-list }
 *    ( type-name ) { initializer-list , }
 */
using PostFixExpr =
    std::variant<PostFixExprPrimaryExpr, PostFixExprSubscript,
                 PostFixExprFuncCall, PostFixExprDot, PostFixExprArrow,
                 PostFixExprIncrement, PostFixExprDecrement,
                 PostFixExprTypeInitializer>;

/**
 * postfix-expression:
 *    primary-expression
 */
class PostFixExprPrimaryExpr final : public Node {
private:
  PrimaryExpr mPrimaryExpr;

public:
  PostFixExprPrimaryExpr(PrimaryExpr &&primaryExpr)
      : mPrimaryExpr(std::move(primaryExpr)){};

  [[nodiscard]] const PrimaryExpr &getPrimaryExpr() const {
    return mPrimaryExpr;
  }
};

/**
 * postfix-expression:
 *    postfix-expression [ expression ]
 */
class PostFixExprSubscript final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  Expr mExpr;

public:
  PostFixExprSubscript(std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr)
      : mPostFixExpr(std::move(postFixExpr)), mExpr(std::move(expr)) {}

  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
  [[nodiscard]] const Expr &getExpr() const { return mExpr; }
};

/**
 * postfix-expression:
 *    postfix-expression ( argument-expression-list{opt} )
 *
 * argument-expression-list:
 *    assignment-expression
 *    argument-expression-list , assignment-expression
 */
class PostFixExprFuncCall final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::vector<AssignExpr> mOptParams;

public:
  PostFixExprFuncCall(std::unique_ptr<PostFixExpr> &&postFixExpr,
                      std::vector<AssignExpr> &&optParams)
      : mPostFixExpr(std::move(postFixExpr)), mOptParams(std::move(optParams)) {
  }

  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
  [[nodiscard]] const std::vector<AssignExpr> &
  getOptionalAssignExpressions() const {
    return mOptParams;
  }
};

/**
 * postfix-expression:
 *    postfix-expression . identifier
 */
class PostFixExprDot final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string mIdentifier;

public:
  PostFixExprDot(std::unique_ptr<PostFixExpr> &&postFixExpr,
                 std::string identifier)
      : mPostFixExpr(std::move(postFixExpr)),
        mIdentifier(std::move(identifier)) {}

  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
  [[nodiscard]] const std::string &getIdentifier() const { return mIdentifier; }
};

/**
 * postfix-expression:
 *    postfix-expression -> identifier
 */
class PostFixExprArrow final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string mIdentifier;

public:
  PostFixExprArrow(std::unique_ptr<PostFixExpr> &&postFixExpr,
                   std::string identifier)
      : mPostFixExpr(std::move(postFixExpr)),
        mIdentifier(std::move(identifier)) {}
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
  [[nodiscard]] const std::string &getIdentifier() const { return mIdentifier; }
};

/**
 * postfix-expression:
 *    postfix-expression ++
 */
class PostFixExprIncrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprIncrement(std::unique_ptr<PostFixExpr> &&postFixExpr)
      : mPostFixExpr(std::move(postFixExpr)) {}
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
};

/**
 * postfix-expression:
 *    postfix-expression --
 */
class PostFixExprDecrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprDecrement(std::unique_ptr<PostFixExpr> &&postFixExpr)
      : mPostFixExpr(std::move(postFixExpr)) {}
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const {
    return mPostFixExpr.get();
  }
};

/**
 * postfix-expression:
 *   ( type-name ) { initializer-list }
 *   ( type-name ) { initializer-list , }
 */
class PostFixExprTypeInitializer final : public Node {
private:
  std::unique_ptr<TypeName> mTypeName;
  std::unique_ptr<InitializerList> mInitializerList;

public:
  PostFixExprTypeInitializer(TypeName &&typeName,
                             InitializerList &&initializerList)
      : mTypeName(std::make_unique<TypeName>(std::move(typeName))),
        mInitializerList(std::make_unique<InitializerList>(std::move(initializerList))) {}

  [[nodiscard]] const InitializerList *getInitializerList() const {
    return mInitializerList.get();
  }
  [[nodiscard]] const TypeName *getTypeName() const { return mTypeName.get(); }
};

/**
 * unary-expression:
 *  postfix-expression
 *  ++ unary-expression
 *  -- unary-expression
 *  unary-operator cast-expression
 *  sizeof unary-expression
 *  sizeof ( type-name )
 *
 *  unary-operator: one of
 *      & * + - ~ !
 */
using UnaryExpr =
    std::variant<UnaryExprPostFixExpr, UnaryExprUnaryOperator, UnaryExprSizeOf>;

/**
 * unary-expression:
 *  postfix-expression
 */
class UnaryExprPostFixExpr final : public Node {
private:
  PostFixExpr mPostExpr;

public:
  UnaryExprPostFixExpr(PostFixExpr &&postExpr)
      : mPostExpr(std::move(postExpr)) {}

  [[nodiscard]] const PostFixExpr &getPostExpr() const { return mPostExpr; }
};

/**
 * unary-expression:
 *  unary-operator cast-expression
 *  ++ unary-expression
 *  -- unary-expression
 *
 *  unary-operator: one of
 *  & * + - ~ !
 */
class UnaryExprUnaryOperator final : public Node {
public:
  enum class UnaryOperator : uint8_t {
    Increment,
    Decrement,
    Ampersand,
    Asterisk,
    Plus,
    Minus,
    BitNot,
    LogicalNot
  };

private:
  UnaryOperator mOperator;
  std::unique_ptr<CastExpr> mCastExpr;

public:
  UnaryExprUnaryOperator(UnaryOperator anOperator,
                         std::unique_ptr<CastExpr> &&castExpr)
      : mOperator(anOperator), mCastExpr(std::move(castExpr)) {}

  [[nodiscard]] UnaryOperator getOperator() const { return mOperator; }
  [[nodiscard]] const CastExpr *getCastExpr() const { return mCastExpr.get(); }
};

/**
 * unary-expression:
 *  sizeof unary-expression
 *  sizeof ( type-name )
 */
class UnaryExprSizeOf final : public Node {
  using Variant =
      std::variant<std::unique_ptr<UnaryExpr>, std::unique_ptr<TypeName>>;
  Variant mValue;

public:
  UnaryExprSizeOf(Variant &&variant) : mValue(std::move(variant)){};

  [[nodiscard]] const Variant &getVariant() const { return mValue; }
};

/**
 * type-qualifier:
 *      const
 *      restrict
 *      volatile
 */
class TypeQualifier final : public Node {
public:
  enum Qualifier { Const, Restrict, Volatile };

private:
  Qualifier mQualifier;

public:
  TypeQualifier(Qualifier qualifier) : mQualifier(qualifier){};
  [[nodiscard]] Qualifier getQualifier() const { return mQualifier; };
};

/**
 * SpecifierQualifier
 *      type-specifier
 *      type-qualifier
 */
using SpecifierQualifier = std::variant<TypeSpecifier, TypeQualifier>;

/**
 * type-name:
 *  specifier-qualifier-list abstract-declarator{opt}
 */
class TypeName final : public Node {
private:
  std::vector<SpecifierQualifier> mSpecifierQualifiers;
  std::unique_ptr<AbstractDeclarator> mAbstractDeclarator;

public:
  TypeName(std::vector<SpecifierQualifier> &&specifierQualifiers,
           std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
      : mSpecifierQualifiers(std::move(specifierQualifiers)),
        mAbstractDeclarator(std::move(abstractDeclarator)) {}

  [[nodiscard]] const std::vector<SpecifierQualifier> &getSpecifierQualifiers() const {
    return mSpecifierQualifiers;
  }
  [[nodiscard]] const AbstractDeclarator *getAbstractDeclarator() const {
    return mAbstractDeclarator.get();
  }
};

/**
 * cast-expression:
 *      unary-expression
 *      ( type-name ) cast-expression
 */
class CastExpr final : public Node {
  using Variant =
      std::variant<UnaryExpr, std::pair<TypeName, std::unique_ptr<CastExpr>>>;

public:
  Variant mVariant;

public:
  CastExpr(Variant &&unaryOrCast) : mVariant(std::move(unaryOrCast)) {}

  [[nodiscard]] const Variant &getVariant() const { return mVariant; }
};

/**
 * multiplicative-expression:
 *  cast-expression
 *  multiplicative-expression * cast-expression
 *  multiplicative-expression / cast-expression
 *  multiplicative-expression % cast-expression
 */
class MultiExpr final : public Node {
public:
  enum BinaryOperator { Multiply, Divide, Modulo };

private:
  CastExpr mCastExpr;
  std::vector<std::pair<BinaryOperator, CastExpr>> mOptCastExps;

public:
  explicit MultiExpr(
      CastExpr &&castExpr,
      std::vector<std::pair<BinaryOperator, CastExpr>> &&optCastExps)
      : mCastExpr(std::move(castExpr)), mOptCastExps(std::move(optCastExps)) {}
  [[nodiscard]] const CastExpr &getCastExpr() const { return mCastExpr; }
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, CastExpr>> &
  getOptionalCastExpr() const {
    return mOptCastExps;
  }
};

/**
 * additive-expression:
 * multiplicative-expression
 * additive-expression + multiplicative-expression
 * additive-expression - multiplicative-expression
 */
class AdditiveExpr final : public Node {
public:
  enum BinaryOperator { Plus, Minus };

private:
  MultiExpr mMultiExpr;
  std::vector<std::pair<BinaryOperator, MultiExpr>> mOptionalMultiExpr;

public:
  AdditiveExpr(
      MultiExpr &&multiExpr,
      std::vector<std::pair<BinaryOperator, MultiExpr>> &&optionalMultiExps)
      : mMultiExpr(std::move(multiExpr)),
        mOptionalMultiExpr(std::move(optionalMultiExps)) {}
  [[nodiscard]] const MultiExpr &getMultiExpr() const { return mMultiExpr; }
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, MultiExpr>> &
  getOptionalMultiExpr() const {
    return mOptionalMultiExpr;
  }
};

/**
 * shift-expression:
 *      additive-expression
 *      shift-expression << additive-expression
 *      shift-expression >> additive-expression
 */
class ShiftExpr final : public Node {
public:
  enum BinaryOperator { Right, Left };

private:
  AdditiveExpr mAdditiveExpr;
  std::vector<std::pair<BinaryOperator, AdditiveExpr>> mOptAdditiveExps;

public:
  ShiftExpr(
      AdditiveExpr &&additiveExpr,
      std::vector<std::pair<BinaryOperator, AdditiveExpr>> &&optAdditiveExps)
      : mAdditiveExpr(std::move(additiveExpr)),
        mOptAdditiveExps(std::move(optAdditiveExps)) {}
  [[nodiscard]] const AdditiveExpr &getAdditiveExpr() const {
    return mAdditiveExpr;
  }
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, AdditiveExpr>> &
  getOptAdditiveExps() const {
    return mOptAdditiveExps;
  }
};

/**
 * relational-expression:
 *      shift-expression
 *      relational-expression < shift-expression
 *      relational-expression > shift-expression
 *      relational-expression <= shift-expression
 *      relational-expression >= shift-expression
 */
class RelationalExpr final : public Node {
public:
  enum BinaryOperator {
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
  };

private:
  ShiftExpr mShiftExpr;
  std::vector<std::pair<BinaryOperator, ShiftExpr>> mOptShiftExps;

public:
  RelationalExpr(
      ShiftExpr &&shiftExpr,
      std::vector<std::pair<BinaryOperator, ShiftExpr>> &&optShiftExps)
      : mShiftExpr(std::move(shiftExpr)),
        mOptShiftExps(std::move(optShiftExps)) {}
  [[nodiscard]] const ShiftExpr &getShiftExpr() const { return mShiftExpr; }
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, ShiftExpr>> &
  getOptionalShiftExpressions() const {
    return mOptShiftExps;
  }
};

/**
 * equality-expression:
 *      relational-expression
 *      equality-expression == relational-expression
 *      equality-expression != relational-expression
 */
class EqualExpr final : public Node {
public:
  enum BinaryOperator { Equal, NotEqual };

private:
  RelationalExpr mRelationalExpr;
  std::vector<std::pair<BinaryOperator, RelationalExpr>> mOptRelationExps;

public:
  EqualExpr(RelationalExpr &&relationalExpr,
            std::vector<std::pair<BinaryOperator, RelationalExpr>>
                &&optRelationalExps)
      : mRelationalExpr(std::move(relationalExpr)),
        mOptRelationExps(std::move(optRelationalExps)) {}
  [[nodiscard]] const RelationalExpr &getRelationalExpr() const {
    return mRelationalExpr;
  }

  [[nodiscard]] const std::vector<std::pair<BinaryOperator, RelationalExpr>> &
  getOptionalRelationalExpr() const {
    return mOptRelationExps;
  }
};

/**
 * AND-expression:
 *      equality-expression
 *      AND-expression & equality-expression
 */
class BitAndExpr final : public Node {
private:
  EqualExpr mEqualExpr;
  std::vector<EqualExpr> mOptEqualExps;

public:
  BitAndExpr(EqualExpr &&equalExpr, std::vector<EqualExpr> &&optEqualExps)
      : mEqualExpr(std::move(equalExpr)),
        mOptEqualExps(std::move(optEqualExps)) {}
  [[nodiscard]] const EqualExpr &getEqualExpr() const { return mEqualExpr; }

  [[nodiscard]] const std::vector<EqualExpr> &getOptionalEqualExpr() const {
    return mOptEqualExps;
  }
};

/**
 * exclusive-OR-expression:
 *      AND-expression
 *      exclusive-OR-expression ^ AND-expression
 */
class BitXorExpr final : public Node {
private:
  BitAndExpr mBitAndExpr;
  std::vector<BitAndExpr> mOptBitAndExps;

public:
  BitXorExpr(BitAndExpr &&bitAndExpr, std::vector<BitAndExpr> &&optBitAndExps)
      : mBitAndExpr(std::move(bitAndExpr)),
        mOptBitAndExps(std::move(optBitAndExps)) {}
  [[nodiscard]] const BitAndExpr &getBitAndExpr() const { return mBitAndExpr; }
  [[nodiscard]] const std::vector<BitAndExpr> &
  getOptionalBitAndExpressions() const {
    return mOptBitAndExps;
  }
};

/**
 * inclusive-OR-expression:
 *      exclusive-OR-expression
 *      inclusive-OR-expression | exclusive-OR-expression
 */
class BitOrExpr final : public Node {
private:
  BitXorExpr mBitXorExpr;
  std::vector<BitXorExpr> mOptBitXorExps;

public:
  BitOrExpr(BitXorExpr &&bitXorExpr, std::vector<BitXorExpr> &&optBitXorExps)
      : mBitXorExpr(std::move(bitXorExpr)),
        mOptBitXorExps(std::move(optBitXorExps)) {}
  [[nodiscard]] const BitXorExpr &getBitXorExpression() const {
    return mBitXorExpr;
  }

  [[nodiscard]] const std::vector<BitXorExpr> &
  getOptionalBitXorExpressions() const {
    return mOptBitXorExps;
  }
};

/**
 * logical-AND-expression:
 *      inclusive-OR-expression
 *      logical-AND-expression && inclusive-OR-expression
 */
class LogAndExpr final : public Node {
private:
  BitOrExpr mBitOrExpr;
  std::vector<BitOrExpr> mOptBitOrExps;

public:
  LogAndExpr(BitOrExpr &&bitOrExpr, std::vector<BitOrExpr> &&optBitOrExps)
      : mBitOrExpr(std::move(bitOrExpr)),
        mOptBitOrExps(std::move(optBitOrExps)) {}
  [[nodiscard]] const BitOrExpr &getBitOrExpression() const {
    return mBitOrExpr;
  }
  [[nodiscard]] const std::vector<BitOrExpr> &
  getOptionalBitOrExpressions() const {
    return mOptBitOrExps;
  }
};

/**
 * logical-OR-expression:
 *      logical-AND-expression
 *      logical-OR-expression || logical-AND-expression
 */
class LogOrExpr final : public Node {
private:
  LogAndExpr mLogAndExpr;
  std::vector<LogAndExpr> mOptLogAndExps;

public:
  LogOrExpr(LogAndExpr &&logAndExpr, std::vector<LogAndExpr> &&optLogAndExps)
      : mLogAndExpr(std::move(logAndExpr)),
        mOptLogAndExps(std::move(optLogAndExps)) {}
  [[nodiscard]] const LogAndExpr &getAndExpression() const {
    return mLogAndExpr;
  }
  [[nodiscard]] const std::vector<LogAndExpr> &
  getOptionalAndExpressions() const {
    return mOptLogAndExps;
  }
};

/**
 * conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 */
class ConditionalExpr final : public Node {
private:
  LogOrExpr mLogOrExpr;
  std::unique_ptr<Expr> mOptExpr;
  std::unique_ptr<ConditionalExpr> mOptCondExpr;

public:
  explicit ConditionalExpr(
      LogOrExpr &&logOrExpr, std::unique_ptr<Expr> &&optExpr = nullptr,
      std::unique_ptr<ConditionalExpr> &&optCondExpr = nullptr)
      : Node(), mLogOrExpr(std::move(logOrExpr)), mOptExpr(std::move(optExpr)),
        mOptCondExpr(std::move(optCondExpr)) {}
  [[nodiscard]] const LogOrExpr &getLogicalOrExpression() const {
    return mLogOrExpr;
  }
  [[nodiscard]] const Expr *getOptionalExpression() const {
    return mOptExpr.get();
  }
  [[nodiscard]] const ConditionalExpr *
  getOptionalConditionalExpression() const {
    return mOptCondExpr.get();
  }
};

/**
 * assignment-expression:
 *      conditional-expression
 *      unary-expression assignment-operator assignment-expression
 *
 * assignment-operator: one of
 *      =  *=  /=  %=  +=  -=  <<=  >>=  &=  ^=  |=
 *
 * Instead we are doing something similar to clang here though:
 * We'll be using the grammar of the form:
 *
 * assignment-expression:
 *      conditional-expression
 *      conditional-expression assignment-operator assignment-expression
 */
class AssignExpr final : public Node {
public:
  enum AssignmentOperator {
    Assign,
    PlusAssign,
    MinusAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
    LeftShiftAssign,
    RightShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign
  };

private:
  ConditionalExpr mCondExpr;
  std::vector<std::pair<AssignmentOperator, ConditionalExpr>> mOptConditionExpr;

public:
  AssignExpr(ConditionalExpr &&conditionalExpression,
             std::vector<std::pair<AssignmentOperator, ConditionalExpr>>
                 &&optConditionExpr)
      : mCondExpr(std::move(conditionalExpression)),
        mOptConditionExpr(std::move(optConditionExpr)) {}

  [[nodiscard]] const ConditionalExpr &getConditionalExpr() const {
    return mCondExpr;
  }
  [[nodiscard]] const std::vector<
      std::pair<AssignmentOperator, ConditionalExpr>> &
  getOptionalConditionalExpr() const {
    return mOptConditionExpr;
  }
};

using Stmt =
    std::variant<ReturnStmt, ExprStmt, IfStmt, BlockStmt, ForStmt, WhileStmt,
                 DoWhileStmt, BreakStmt, ContinueStmt, SwitchStmt, DefaultStmt,
                 CaseStmt, GotoStmt, LabelStmt>;

/**
 * expression-statement:
 *      expression{opt} ;
 */
class ExprStmt final : public Node {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  ExprStmt(std::unique_ptr<Expr> &&optExpr = nullptr)
      : mOptExpr(std::move(optExpr)) {}
  [[nodiscard]] const Expr *getOptionalExpression() const {
    return mOptExpr.get();
  }
  std::unique_ptr<Expr> moveOptionalExpr() { return std::move(mOptExpr); }
};

/**
 * if-statement:
 *      if ( expression ) statement
 *      if ( expression ) statement else statement
 */
class IfStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mThenStmt;
  std::unique_ptr<Stmt> mOptElseStmt;

public:
  IfStmt(Expr &&expr, std::unique_ptr<Stmt> &&thenStmt,
         std::unique_ptr<Stmt> &&optElseStmt = nullptr)
      : mExpr(std::move(expr)), mThenStmt(std::move(thenStmt)),
        mOptElseStmt(std::move(optElseStmt)) {}

  [[nodiscard]] const Expr &getExpression() const { return mExpr; }

  [[nodiscard]] const Stmt *getThenStmt() const { return mThenStmt.get(); }

  [[nodiscard]] const Stmt *getElseStmt() const { return mOptElseStmt.get(); }
};

/**
 * switch-statement:
 *      switch ( expression ) statement
 */
class SwitchStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  SwitchStmt(Expr &&expression, std::unique_ptr<Stmt> &&statement)
      : mExpr(std::move(expression)), mStmt(std::move(statement)) {}

  [[nodiscard]] const Expr &getExpression() const { return mExpr; }

  [[nodiscard]] const Stmt *getStatement() const { return mStmt.get(); }
};

/**
 * default-statement:
 *      default : statement
 */
class DefaultStmt final : public Node {
private:
  std::unique_ptr<Stmt> mStmt;

public:
  DefaultStmt(std::unique_ptr<Stmt> &&statement)
      : mStmt(std::move(statement)) {}
  [[nodiscard]] const Stmt *getStatement() const { return mStmt.get(); }
};

/**
 * case-statement:
 *      case constant-expression : statement
 */
class CaseStmt final : public Node {
private:
  ConstantExpr mConstantExpr;
  std::unique_ptr<Stmt> mStatement;

public:
  CaseStmt(ConstantExpr &&constantExpr, std::unique_ptr<Stmt> &&statement)
      : mConstantExpr(std::move(constantExpr)),
        mStatement(std::move(statement)) {}

  [[nodiscard]] const ConstantExpr &getConstantExpr() const {
    return mConstantExpr;
  }
  [[nodiscard]] const Stmt *getStatement() const { return mStatement.get(); }
};

/**
 * label-statement:
 *      identifier : statement
 */
class LabelStmt final : public Node {
private:
  std::string mIdentifier;

public:
  LabelStmt(std::string identifier) : mIdentifier(std::move(identifier)) {}
  [[nodiscard]] const std::string &getIdentifier() const { return mIdentifier; }
};

/**
 * goto-statement:
 *      goto identifier ;
 */
class GotoStmt final : public Node {
private:
  std::string mIdentifier;

public:
  GotoStmt(std::string identifier) : mIdentifier(std::move(identifier)) {}
  [[nodiscard]] const std::string &getIdentifier() const { return mIdentifier; }
};

/**
 * do-while-statement:
 *      do statement while ( expression ) ;
 */
class DoWhileStmt final : public Node {
private:
  std::unique_ptr<Stmt> mStmt;
  Expr mExpr;

public:
  DoWhileStmt(std::unique_ptr<Stmt> &&stmt, Expr &&expr)
      : mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
  [[nodiscard]] const Stmt *getStatement() const { return mStmt.get(); }
  [[nodiscard]] const Expr &getExpression() const { return mExpr; }
};

/**
 * while-statement:
 *      while ( expression ) statement
 */
class WhileStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  WhileStmt(Expr &&expr, std::unique_ptr<Stmt> &&stmt)
      : mStmt(std::move(stmt)), mExpr(std::move(expr)) {}
  [[nodiscard]] const Expr &getExpression() const { return mExpr; }
  [[nodiscard]] const Stmt *getStatement() const { return mStmt.get(); }
};

/**
 * for-statement:
 *      for ( expression{opt} ; expression{opt} ; expression{opt} ) statement
 *      for ( declaration expression{opt} ; expression{opt} ) statement
 */
class ForStmt final : public Node {
private:
  std::variant<std::unique_ptr<Declaration>, std::unique_ptr<Expr>> mInitial;
  std::unique_ptr<Expr> mControlExpr;
  std::unique_ptr<Expr> mPostExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  ForStmt(std::unique_ptr<Stmt> &&stmt,
          std::variant<std::unique_ptr<Declaration>, std::unique_ptr<Expr>>
              &&initial,
          std::unique_ptr<Expr> &&controlExpr = nullptr,
          std::unique_ptr<Expr> &&postExpr = nullptr)
      : mStmt(std::move(stmt)), mInitial(std::move(initial)),
        mControlExpr(std::move(controlExpr)), mPostExpr(std::move(postExpr)) {}
  [[nodiscard]] const Stmt *getStatement() const { return mStmt.get(); }

  [[nodiscard]] const std::variant<std::unique_ptr<Declaration>,
                                   std::unique_ptr<Expr>> &
  getInitial() const {
    return mInitial;
  }
  [[nodiscard]] const Expr *getControlling() const {
    return mControlExpr.get();
  }
  [[nodiscard]] const Expr *getPost() const { return mPostExpr.get(); }
};

/**
 * storage-class-specifier:
 *      typedef
 *      extern
 *      static
 *      auto
 *      register
 */
class StorageClassSpecifier final : public Node {
public:
  enum Specifiers { Typedef, Extern, Static, Auto, Register };

private:
  Specifiers mSpecifier;

public:
  StorageClassSpecifier(Specifiers specifier) : mSpecifier(specifier) {}
  [[nodiscard]] Specifiers getSpecifier() const { return mSpecifier; }
};

inline bool operator==(const StorageClassSpecifier &storageClassSpecifier,
                       StorageClassSpecifier::Specifiers specifier) {
  return specifier == storageClassSpecifier.getSpecifier();
}

inline bool operator!=(const StorageClassSpecifier &storageClassSpecifier,
                       StorageClassSpecifier::Specifiers specifier) {
  return specifier != storageClassSpecifier.getSpecifier();
}

inline bool operator==(StorageClassSpecifier::Specifiers specifier,
                       const StorageClassSpecifier &storageClassSpecifier) {
  return specifier == storageClassSpecifier.getSpecifier();
}

inline bool operator!=(StorageClassSpecifier::Specifiers specifier,
                       const StorageClassSpecifier &storageClassSpecifier) {
  return specifier != storageClassSpecifier.getSpecifier();
}

/**
 * function-specifier:
 *      inline
 */
class FunctionSpecifier final : public Node {
public:
  FunctionSpecifier() {}
};

using DeclarationSpecifier = std::variant<StorageClassSpecifier, TypeSpecifier,
                                          TypeQualifier, FunctionSpecifier>;

/**
 * declaration:
 *      declaration-specifiers init-declarator-list{opt} ;
 *
 *  init-declarator-list:
 *      init-declarator
 *      init-declarator-list , init-declarator
 *
 *  init-declarator:
 *      declarator
 *      declarator = initializer
 */
class Declaration final : public Node {
public:
  struct InitDeclarator {
    std::unique_ptr<Declarator> mDeclarator;
    std::unique_ptr<Initializer> mOptInitializer;
  };

private:
  std::vector<DeclarationSpecifier> mDeclarationSpecifiers;
  std::vector<InitDeclarator> mInitDeclarators;

public:
  Declaration(std::vector<DeclarationSpecifier> &&declarationSpecifiers,
              std::vector<InitDeclarator> &&initDeclarators)
      : mDeclarationSpecifiers(std::move(declarationSpecifiers)),
        mInitDeclarators(std::move(initDeclarators)) {}
  [[nodiscard]] const std::vector<DeclarationSpecifier> &
  getDeclarationSpecifiers() const {
    return mDeclarationSpecifiers;
  }
  [[nodiscard]] const std::vector<InitDeclarator> &getInitDeclarators() const {
    return mInitDeclarators;
  }
};

/**
 * break-statement:
 *      break ;
 */
class BreakStmt final : public Node {
public:
  BreakStmt() {}
};

/**
 * continue-statement:
 *      continue ;
 */
class ContinueStmt final : public Node {
public:
  ContinueStmt() {}
};

/**
 * return-statement:
 *      return expr{opt} ;
 */
class ReturnStmt final : public Node {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  ReturnStmt(std::unique_ptr<Expr> &&optExpr = nullptr)
      : mOptExpr(std::move(optExpr)) {}
  [[nodiscard]] const Expr *getExpression() const { return mOptExpr.get(); }
};

using BlockItem = std::variant<Stmt, Declaration>;

/**
 * compound-statement:
 *      { block-item-list{opt} }
 * block-item-list:
 *      block-item
 *      block-item-list block-item
 * block-item:
 *      declaration
 *      statement
 */
class BlockStmt final : public Node {
private:
  std::vector<BlockItem> mBlockItems;

public:
  BlockStmt(std::vector<BlockItem> &&blockItems)
      : mBlockItems(std::move(blockItems)) {}
  [[nodiscard]] const std::vector<BlockItem> &getBlockItems() const {
    return mBlockItems;
  }
};

/**
 * direct-abstract-declarator:
 *      ( abstract-declarator )
 *      direct-abstract-declarator{opt} [ type-qualifier-list{opt}
 * assignment-expression{opt} ] direct-abstract-declarator{opt} [ static
 * type-qualifier-list{opt} assignment-expression ]
 *      direct-abstract-declarator{opt} [ type-qualifier-list static
 * assignment-expression ] direct-abstract-declarator{opt} [*]
 *      direct-abstract-declarator{opt} ( parameter-type-list{opt} )
 */
using DirectAbstractDeclarator =
    std::variant<DirectAbstractDeclaratorParent,
                 DirectAbstractDeclaratorAssignExpr,
                 DirectAbstractDeclaratorParamTypeList>;

/**
 * direct-abstract-declarator:
 *      ( abstract-declarator )
 */
class DirectAbstractDeclaratorParent final : public Node {
  std::unique_ptr<AbstractDeclarator> mAbstractDeclarator;

public:
  DirectAbstractDeclaratorParent(
      std::unique_ptr<AbstractDeclarator> &&abstractDeclarator)
      : mAbstractDeclarator(std::move(abstractDeclarator)) {}

  [[nodiscard]] const AbstractDeclarator *getAbstractDeclarator() const {
    return mAbstractDeclarator.get();
  }
};

/**
 * direct-abstract-declarator:
 *      direct-abstract-declarator{opt} [ type-qualifier-list{opt}
 * assignment-expression{opt} ] direct-abstract-declarator{opt} [ static
 * type-qualifier-list{opt} assignment-expression ]
 *      direct-abstract-declarator{opt} [ type-qualifier-list static
 * assignment-expression ]
 */
class DirectAbstractDeclaratorAssignExpr final : public Node {
  std::unique_ptr<DirectAbstractDeclarator> mDirectAbstractDeclarator;
  std::unique_ptr<AssignExpr> mAssignmentExpression;

public:
  DirectAbstractDeclaratorAssignExpr(
      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::unique_ptr<AssignExpr> &&assignmentExpression)
      : mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
        mAssignmentExpression(std::move(assignmentExpression)) {}

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    return mDirectAbstractDeclarator.get();
  }

  [[nodiscard]] const AssignExpr *getAssignmentExpression() const {
    return mAssignmentExpression.get();
  }
};

/**
 * direct-abstract-declarator:
 *  direct-abstract-declarator{opt} ( parameter-type-list{opt} )
 */
class DirectAbstractDeclaratorParamTypeList final : public Node {
  std::unique_ptr<DirectAbstractDeclarator> mDirectAbstractDeclarator;
  std::unique_ptr<ParamTypeList> mParameterTypeList;

public:
  DirectAbstractDeclaratorParamTypeList(
      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::unique_ptr<ParamTypeList> &&parameterTypeList)
      : mDirectAbstractDeclarator(std::move(directAbstractDeclarator)),
        mParameterTypeList(std::move(parameterTypeList)) {}

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    return mDirectAbstractDeclarator.get();
  }

  [[nodiscard]] const ParamTypeList *getParameterTypeList() const {
    return mParameterTypeList.get();
  }
};

/**
 * abstract-declarator:
 *  pointer
 *  pointer{opt} direct-abstract-declarator
 */
class AbstractDeclarator final : public Node {
  std::vector<Pointer> mPointers;
  std::optional<DirectAbstractDeclarator> mDirectAbstractDeclarator;

public:
  AbstractDeclarator(
      std::vector<Pointer> &&pointers,
      std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator = {})
      : mPointers(std::move(pointers)),
        mDirectAbstractDeclarator(std::move(directAbstractDeclarator)) {}

  [[nodiscard]] const std::vector<Pointer> &getPointers() const {
    return mPointers;
  }

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    return mDirectAbstractDeclarator ? &*mDirectAbstractDeclarator : nullptr;
  }
};

/**
 * parameter-declaration:
 *      declaration-specifiers declarator
 *      declaration-specifiers abstract-declarator{opt}
 */
struct ParameterDeclaration : public Node {
  std::vector<DeclarationSpecifier> declarationSpecifiers;
  std::variant<std::unique_ptr<Declarator>, std::unique_ptr<AbstractDeclarator>>
      declarator;

  ParameterDeclaration(std::vector<DeclarationSpecifier> declarationSpecifiers,
                       std::variant<std::unique_ptr<Declarator>,
                                    std::unique_ptr<AbstractDeclarator>>
                           variant = std::unique_ptr<AbstractDeclarator>{})
      : declarationSpecifiers(std::move(declarationSpecifiers)),
        declarator(std::move(variant)) {}
};

/**
 * parameter-list:
 *  parameter-declaration
 *  parameter-list , parameter-declaration
 */
class ParamList final : public Node {
private:
  std::vector<ParameterDeclaration> mParameterList;

public:
  ParamList(std::vector<ParameterDeclaration> &&parameterList)
      : mParameterList(std::move(parameterList)) {}

  [[nodiscard]] const std::vector<ParameterDeclaration> &
  getParameterDeclarations() const {
    return mParameterList;
  }
};

/**
 * parameter-type-list:
 *  parameter-list
 *  parameter-list , ...
 */
class ParamTypeList final : public Node {
  ParamList mParameterList;
  bool mHasEllipse;

public:
  ParamTypeList(ParamList &&parameterList, bool hasEllipse)
      : mParameterList(std::move(parameterList)), mHasEllipse(hasEllipse) {}

  [[nodiscard]] const ParamList &getParameterList() const {
    return mParameterList;
  }

  [[nodiscard]] bool hasEllipse() const { return mHasEllipse; }
};

/**
 * direct-declarator:
 *   identifier
 *   ( declarator )
 *   direct-declarator [ type-qualifier-list{opt} assignment-expression{opt} ]
 *   direct-declarator [ static type-qualifier-list{opt} assignment-expression ]
 *   direct-declarator [ type-qualifier-list static assignment-expression ]
 *   direct-declarator [ type-qualifier-list{opt} * ]
 *   direct-declarator ( parameter-type-list )
 *   direct-declarator ( identifier-list{opt} )
 */
using DirectDeclarator =
    std::variant<DirectDeclaratorIdent, DirectDeclaratorParent,
                 DirectDeclaratorAssignExpr,DirectDeclaratorParentParamTypeList>;

/**
 * direct-declarator:
 *  identifier
 */
class DirectDeclaratorIdent final : public Node {
  std::string mIdentifierLoc;

public:
  DirectDeclaratorIdent(std::string identifierLoc)
      : mIdentifierLoc(std::move(identifierLoc)) {}

  [[nodiscard]] std::string getIdentifierLoc() const { return mIdentifierLoc; }
};

/**
 * direct-declarator:
 *  ( declarator )
 */
class DirectDeclaratorParent final : public Node {
  std::unique_ptr<Declarator> mDeclarator;

public:
  DirectDeclaratorParent(std::unique_ptr<Declarator> &&declarator)
      : mDeclarator(std::move(declarator)) {}

  [[nodiscard]] const Declarator *getDeclarator() const {
    return mDeclarator.get();
  }
};

/**
 * direct-declarator:
 *  direct-declarator ( parameter-type-list )
 */
class DirectDeclaratorParentParamTypeList final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  ParamTypeList mParameterTypeList;

public:
  DirectDeclaratorParentParamTypeList(std::unique_ptr<DirectDeclarator> &&directDeclarator,
                              ParamTypeList &&parameterTypeList)
      : mDirectDeclarator(std::move(directDeclarator)),
        mParameterTypeList(std::move(parameterTypeList)) {}

  [[nodiscard]] const DirectDeclarator *getDirectDeclarator() const {
    return mDirectDeclarator.get();
  }

  [[nodiscard]] const ParamTypeList &getParameterTypeList() const {
    return mParameterTypeList;
  }
};

/**
 * direct-declarator:
 *  direct-declarator [ type-qualifier-list{opt} assignment-expression{opt} ]
 *  direct-declarator [ static type-qualifier-list{opt} assignment-expression ]
 *  direct-declarator [ type-qualifier-list static assignment-expression ]
 */
class DirectDeclaratorAssignExpr final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  std::unique_ptr<AssignExpr> mAssignmentExpression;

public:
  DirectDeclaratorAssignExpr(
      std::unique_ptr<DirectDeclarator> &&directDeclarator,
      std::unique_ptr<AssignExpr> &&assignmentExpression)
      : mDirectDeclarator(std::move(directDeclarator)),
        mAssignmentExpression(std::move(assignmentExpression)) {}

  [[nodiscard]] const DirectDeclarator *getDirectDeclarator() const {
    return mDirectDeclarator.get();
  }

  [[nodiscard]] const std::unique_ptr<AssignExpr> &
  getAssignmentExpression() const {
    return mAssignmentExpression;
  }
};

/**
 * declarator:
 *  pointer{opt} direct-declarator
 */
class Declarator final : public Node {
  std::vector<Pointer> mPointers;
  DirectDeclarator mDirectDeclarator;

public:
  Declarator(std::vector<Pointer> &&pointers,
             DirectDeclarator &&directDeclarator)
      : mPointers(std::move(pointers)),
        mDirectDeclarator(std::move(directDeclarator)) {}

  [[nodiscard]] const std::vector<Pointer> &getPointers() const {
    return mPointers;
  }

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const {
    return mDirectDeclarator;
  }
};

/**
 * struct-or-union-specifier:
 *  struct-or-union identifier{opt} { struct-declaration-list }
 *  struct-or-union identifier
 *
 *  struct-declaration-list:
 *    struct-declaration
 *    struct-declaration-list struct-declaration
 *
 *  struct-declaration:
 *    specifier-qualifier-list struct-declarator-list ;
 *
 *  struct-declarator-list:
 *      struct-declarator
 *      struct-declarator-list , struct-declarator
 *
 *  struct-declarator:
 *      declarator
 *      declarator{opt} : constant-expression
 */
class StructOrUnionSpecifier final : public Node {
private:
  std::string mIdentifier;
  bool mIsUnion;

public:
  struct StructDeclaration {
    std::vector<SpecifierQualifier> specifierQualifiers;
    struct StructDeclarator {
      std::unique_ptr<Declarator> optionalDeclarator;
      std::optional<ConstantExpr> optionalBitfield;
    };
    std::vector<StructDeclarator> structDeclarators;
  };

private:
  std::vector<StructDeclaration> mStructDeclarations;

public:
  StructOrUnionSpecifier(bool isUnion, std::string identifier,
                         std::vector<StructDeclaration> &&structDeclarations)
      : mIsUnion(isUnion), mIdentifier(identifier),
        mStructDeclarations(std::move(structDeclarations)) {}

  [[nodiscard]] bool isUnion() const { return mIsUnion; }

  [[nodiscard]] const std::string &getIdentifier() const { return mIdentifier; }

  [[nodiscard]] const std::vector<StructDeclaration> &
  getStructDeclarations() const {
    return mStructDeclarations;
  }
};

/**
 * enumerator-list:
 *  enumerator
 *  enumerator-list , enumerator
 *
 * enumerator:
 *  enumeration-constant
 *  enumeration-constant = constant-expression
 */
class EnumeratorList final : public Node {
public:
  struct Enumerator {
    std::string name;
    std::optional<ConstantExpr> value;
  };

private:
  std::vector<Enumerator> mValues;
  std::string mName;

public:
  EnumeratorList(std::string name, std::vector<Enumerator> &&values)
      : mName(std::move(name)), mValues(std::move(values)) {}

  [[nodiscard]] const std::string &getName() const { return mName; }

  [[nodiscard]] const std::vector<Enumerator> &getValues() const {
    return mValues;
  }
};

/**
 * enum-specifier:
 *  enum identifier{opt} { enumerator-list }
 *  enum identifier{opt} { enumerator-list , }
 *  enum identifier
 */
class EnumSpecifier final : public Node {
  using variant = std::variant<EnumeratorList, std::string>;

  variant mVariant;

public:
  EnumSpecifier(variant &&variant) : mVariant(std::move(variant)) {}

  [[nodiscard]] const variant &getVariant() const { return mVariant; }
};

/**
 * type-specifier:
 *  void char short int long float double signed unsigned _Bool
 *  struct-or-union-specifier
 *  enum-specifier
 *  typedef-name
 */
class TypeSpecifier final : public Node {
public:
  enum PrimitiveTypeSpecifier {
    Void = 0b1,
    Char = 0b10,
    Short = 0b100,
    Int = 0b1000,
    Long = 0b10000,
    Float = 0b1000000,
    Double = 0b10000000,
    Signed = 0b100000000,
    Unsigned = 0b1000000000,
    Bool = 0b10000000000,
  };

private:
  using variant =
      std::variant<PrimitiveTypeSpecifier,
                   std::unique_ptr<StructOrUnionSpecifier>,
                   std::unique_ptr<EnumSpecifier>, std::string_view>;

  variant mVariant;

public:
  TypeSpecifier(variant &&variant) : mVariant(std::move(variant)) {}

  [[nodiscard]] const variant &getVariant() const { return mVariant; }
};

/**
 * pointer:
 *  type-qualifier-list{opt}
 *  type-qualifier-list{opt} pointer
 */
class Pointer final : public Node {
  std::vector<TypeQualifier> mTypeQualifiers;

public:
  Pointer(std::vector<TypeQualifier> &&typeQualifiers)
      : mTypeQualifiers(std::move(typeQualifiers)) {}

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const {
    return mTypeQualifiers;
  }
};

/**
 * initializer-list:
 *  designation{opt} initializer
 *  initializer-list , designation{opt} initializer
 *
 *  designation:
 *      designator-list =
 *
 *  designator-list:
 *      designator
 *      designator-list designator
 *
 *  designator:
 *      [ constant-expression ]
 *      . identifier
 *
 *  eg:
 *      struct { int a[3], b; } w[] = { [0].a = {1}, [1].a[0] = 2 };
 *  [0].a  meaning designator designator
 */
class InitializerList final : public Node {
public:
  using Designator = std::variant<ConstantExpr, std::string>;

  using DesignatorList = std::vector<Designator>;

  using vector = std::vector<std::pair<Initializer, DesignatorList>>;

private:
  vector mInitializer;

public:
  InitializerList(vector &&initializer)
      : mInitializer(std::move(initializer)) {}

  const vector &getInitializerList() const { return mInitializer; }
};

/**
 * initializer:
 *  assignment-expression
 *  { initializer-list }
 *  { initializer-list , }
 */
class Initializer final : public Node {
  using variant = std::variant<AssignExpr, InitializerList>;
  variant mVariant;

public:
  Initializer(variant &&variant) : mVariant(std::move(variant)) {}

  [[nodiscard]] const variant &getVariant() const { return mVariant; }
};

/**
 * function-definition:
 *  declaration-specifiers declarator declaration-list{opt} compound-statement
 */
class FunctionDefinition final : public Node {
  std::vector<DeclarationSpecifier> mDeclarationSpecifiers;
  Declarator mDeclarator;
  BlockStmt mCompoundStatement;

public:
  FunctionDefinition(std::vector<DeclarationSpecifier> &&declarationSpecifiers,
                     Declarator &&declarator, BlockStmt &&compoundStatement)
      : mDeclarationSpecifiers(std::move(declarationSpecifiers)),
        mDeclarator(std::move(declarator)),
        mCompoundStatement(std::move(compoundStatement)) {}

  [[nodiscard]] const std::vector<DeclarationSpecifier> &
  getDeclarationSpecifiers() const {
    return mDeclarationSpecifiers;
  }

  [[nodiscard]] const Declarator &getDeclarator() const { return mDeclarator; }

  [[nodiscard]] const BlockStmt &getCompoundStatement() const {
    return mCompoundStatement;
  }
};

/**
 * external-declaration:
 *  function-definition
 *  declaration
 */
using ExternalDeclaration = std::variant<Declaration, FunctionDefinition>;

/**
 * translation-unit:
 *  external-declaration
 *  translation-unit external-declaration
 */
class TranslationUnit final {
  std::vector<ExternalDeclaration> mGlobals;

public:
  explicit TranslationUnit(std::vector<ExternalDeclaration> &&globals) noexcept
      : mGlobals(std::move(globals)) {}

  [[nodiscard]] const std::vector<ExternalDeclaration> &getGlobals() const {
    return mGlobals;
  }
};
} // namespace lcc::Syntax

#endif // LCC_SYNTAX_H
