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
#include <string_view>
#include <string>
#include <variant>
#include <vector>
#include <memory>

namespace lcc::Syntax {
class PrimaryExprIdent;
class PrimaryExprConstant;
class PrimaryExprParentheses;
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
class TypeName;
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
class DeclarationSpecifiers;
class SpecifierQualifiers;
class StorageClassSpecifier;
class TypeQualifier;
class TypeSpecifier;
class EnumSpecifier;
class EnumeratorList;
class StructOrUnionSpecifier;
class Declarator;
class DirectDeclaratorIdent;
class DirectDeclaratorParentheses;
class DirectDeclaratorAssignExpr;
class DirectDeclaratorAsterisk;
class DirectDeclaratorParamTypeList;
class AbstractDeclarator;
class DirectAbstractDeclaratorParentheses;
class DirectAbstractDeclaratorParamTypeList;
class DirectAbstractDeclaratorAssignExpr;
class DirectAbstractDeclaratorAsterisk;
class Pointer;
class ParamTypeList;
class ParameterDeclaration;
class ParamList;
class Initializer;
class InitializerList;

class Node {
public:
  Node(){};
  virtual ~Node() = default;
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
  Node(Node &&) = default;
  Node &operator=(Node &&) = default;
};

/*
 * primary-expression:
 *    identifier
 */
class PrimaryExprIdent final : public Node {
private:
  std::string_view mIdent;

public:
  PrimaryExprIdent(std::string_view identifier);
  [[nodiscard]] const std::string_view &getIdentifier() const;
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
  PrimaryExprConstant(Variant variant);
  [[nodiscard]] const Variant &getValue() const;
};

/*
 * primary-expression:
 *    ( expression )
 */
class PrimaryExprParentheses final : public Node {
private:
  std::unique_ptr<Expr> mExpr;

public:
  PrimaryExprParentheses(Expr &&expr);
  [[nodiscard]] const Expr &getExpr() const;
};

/*
 * primary-expression:
 *    identifier
 *    constant
 *    string-literal
 *    ( expression )
 */
using PrimaryExpr =
    std::variant<PrimaryExprIdent, PrimaryExprConstant, PrimaryExprParentheses>;

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
  PostFixExprPrimaryExpr(PrimaryExpr &&primaryExpr);

  [[nodiscard]] const PrimaryExpr &getPrimaryExpr() const;
};

/**
 * postfix-expression:
 *    postfix-expression [ expression ]
 */
class PostFixExprSubscript final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::unique_ptr<Expr> mExpr;

public:
  PostFixExprSubscript(std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr);
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
  [[nodiscard]] const Expr &getExpr() const;
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
  std::vector<std::unique_ptr<AssignExpr>> mOptParams;

public:
  PostFixExprFuncCall(std::unique_ptr<PostFixExpr> &&postFixExpr,
                      std::vector<std::unique_ptr<AssignExpr>> &&optParams);

  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
  [[nodiscard]] const std::vector<std::unique_ptr<AssignExpr>> &
  getOptionalAssignExpressions() const;
};

/**
 * postfix-expression:
 *    postfix-expression . identifier
 */
class PostFixExprDot final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string_view mIdentifier;

public:
  PostFixExprDot(std::unique_ptr<PostFixExpr> &&postFixExpr,
                 std::string_view identifier);

  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
  [[nodiscard]] const std::string_view &getIdentifier() const;
};

/**
 * postfix-expression:
 *    postfix-expression -> identifier
 */
class PostFixExprArrow final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string_view mIdentifier;

public:
  PostFixExprArrow(std::unique_ptr<PostFixExpr> &&postFixExpr,
                   std::string_view identifier);
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
  [[nodiscard]] const std::string_view &getIdentifier() const;
};

/**
 * postfix-expression:
 *    postfix-expression ++
 */
class PostFixExprIncrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprIncrement(std::unique_ptr<PostFixExpr> &&postFixExpr);
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
};

/**
 * postfix-expression:
 *    postfix-expression --
 */
class PostFixExprDecrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprDecrement(std::unique_ptr<PostFixExpr> &&postFixExpr);
  [[nodiscard]] const PostFixExpr *getPostFixExpr() const;
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
                             InitializerList &&initializerList);

  [[nodiscard]] const InitializerList *getInitializerList() const;
  [[nodiscard]] const TypeName *getTypeName() const;
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
  UnaryExprPostFixExpr(PostFixExpr &&postExpr);

  [[nodiscard]] const PostFixExpr &getPostExpr() const;
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
                         std::unique_ptr<CastExpr> &&castExpr);

  [[nodiscard]] UnaryOperator getOperator() const;
  [[nodiscard]] const CastExpr *getCastExpr() const;
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
  UnaryExprSizeOf(Variant &&variant);

  [[nodiscard]] const Variant &getVariant() const;
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
  TypeSpecifier(variant &&variant);

  [[nodiscard]] const variant &getVariant() const;
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
  TypeQualifier(Qualifier qualifier);
  [[nodiscard]] Qualifier getQualifier() const;
};

/**
 * function-specifier:
 *      inline
 */
class FunctionSpecifier final : public Node {
public:
  FunctionSpecifier();
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
  StorageClassSpecifier(Specifiers specifier);
  [[nodiscard]] Specifiers getSpecifier() const;
};

/**
 * declaration-specifiers:
        storage-class-specifier declaration-specifiers{opt}
        type-specifier declaration-specifiers{opt}
        type-qualifier declaration-specifiers{opt}
        function-specifier declaration-specifiers{opt}
 */
class DeclarationSpecifiers final : public Node {
private:
  std::vector<StorageClassSpecifier> mStorageClassSpecifiers;
  std::vector<TypeSpecifier> mTypeSpecifiers;
  std::vector<TypeQualifier> mTypeQualifiers;
  std::vector<FunctionSpecifier> mFunctionSpecifiers;

public:
  DeclarationSpecifiers();
  void addStorageClassSpecifier(StorageClassSpecifier && specifier);
  void addTypeSpecifier(TypeSpecifier && specifier);
  void addTypeQualifier(TypeQualifier && qualifier);
  void addFunctionSpecifier(FunctionSpecifier && specifier);

  [[nodiscard]] const std::vector<StorageClassSpecifier> &getStorageClassSpecifiers() const;
  [[nodiscard]] const std::vector<TypeSpecifier> &getTypeSpecifiers() const;
  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const;
  [[nodiscard]] const std::vector<FunctionSpecifier> &getFunctionSpecifiers() const;
  [[nodiscard]] bool isEmpty() const;
};

/**
 * SpecifierQualifier
 *      type-specifier
 *      type-qualifier
 */

//class SpecifierQualifiers final : public Node {
//private:
//  std::vector<TypeSpecifier> mTypeSpecifiers;
//  std::vector<TypeQualifier> mTypeQualifiers;
//
//public:
//  SpecifierQualifiers() {}
//  void addTypeSpecifier(TypeSpecifier && specifier) {
//    mTypeSpecifiers.push_back(std::move(specifier));
//  }
//  void addTypeQualifier(TypeQualifier && qualifier) {
//    mTypeQualifiers.push_back(std::move(qualifier));
//  }
//  [[nodiscard]] const std::vector<TypeSpecifier> &getTypeSpecifiers() const {
//    return mTypeSpecifiers;
//  }
//  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const {
//    return mTypeQualifiers;
//  }
//  [[nodiscard]] bool isEmpty() const {
//    return mTypeSpecifiers.empty() && mTypeQualifiers.empty();
//  }
//};

/**
 * type-name:
 *  specifier-qualifier-list abstract-declarator{opt}
 */
class TypeName final : public Node {
private:
  DeclarationSpecifiers mSpecifierQualifiers;
  std::unique_ptr<AbstractDeclarator> mAbstractDeclarator;
public:
  TypeName(DeclarationSpecifiers &&specifierQualifiers,
           std::unique_ptr<AbstractDeclarator> &&abstractDeclarator);
  [[nodiscard]] const DeclarationSpecifiers &getSpecifierQualifiers() const;
  [[nodiscard]] const AbstractDeclarator *getAbstractDeclarator() const;
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
  CastExpr(Variant &&unaryOrCast);
  [[nodiscard]] const Variant &getVariant() const;
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
      std::vector<std::pair<BinaryOperator, CastExpr>> &&optCastExps);
  [[nodiscard]] const CastExpr &getCastExpr() const;
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, CastExpr>> &
  getOptionalCastExpr() const;
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
      std::vector<std::pair<BinaryOperator, MultiExpr>> &&optionalMultiExps);
  [[nodiscard]] const MultiExpr &getMultiExpr() const;
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, MultiExpr>> &
  getOptionalMultiExpr() const;
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
      std::vector<std::pair<BinaryOperator, AdditiveExpr>> &&optAdditiveExps);
  [[nodiscard]] const AdditiveExpr &getAdditiveExpr() const;
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, AdditiveExpr>> &
  getOptAdditiveExps() const;
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
      std::vector<std::pair<BinaryOperator, ShiftExpr>> &&optShiftExps);
  [[nodiscard]] const ShiftExpr &getShiftExpr() const;
  [[nodiscard]] const std::vector<std::pair<BinaryOperator, ShiftExpr>> &
  getOptionalShiftExpressions() const;
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
                &&optRelationalExps);
  [[nodiscard]] const RelationalExpr &getRelationalExpr() const;

  [[nodiscard]] const std::vector<std::pair<BinaryOperator, RelationalExpr>> &
  getOptionalRelationalExpr() const;
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
  BitAndExpr(EqualExpr &&equalExpr, std::vector<EqualExpr> &&optEqualExps);
  [[nodiscard]] const EqualExpr &getEqualExpr() const;

  [[nodiscard]] const std::vector<EqualExpr> &getOptionalEqualExpr() const;
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
  BitXorExpr(BitAndExpr &&bitAndExpr, std::vector<BitAndExpr> &&optBitAndExps);
  [[nodiscard]] const BitAndExpr &getBitAndExpr() const;
  [[nodiscard]] const std::vector<BitAndExpr> &
  getOptionalBitAndExpressions() const;
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
  BitOrExpr(BitXorExpr &&bitXorExpr, std::vector<BitXorExpr> &&optBitXorExps);
  [[nodiscard]] const BitXorExpr &getBitXorExpression() const;

  [[nodiscard]] const std::vector<BitXorExpr> &
  getOptionalBitXorExpressions() const;
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
  LogAndExpr(BitOrExpr &&bitOrExpr, std::vector<BitOrExpr> &&optBitOrExps);
  [[nodiscard]] const BitOrExpr &getBitOrExpression() const;
  [[nodiscard]] const std::vector<BitOrExpr> &
  getOptionalBitOrExpressions() const;
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
  LogOrExpr(LogAndExpr &&logAndExpr, std::vector<LogAndExpr> &&optLogAndExps);
  [[nodiscard]] const LogAndExpr &getAndExpression() const;
  [[nodiscard]] const std::vector<LogAndExpr> &
  getOptionalAndExpressions() const;
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
      std::unique_ptr<ConditionalExpr> &&optCondExpr = nullptr);
  [[nodiscard]] const LogOrExpr &getLogicalOrExpression() const;
  [[nodiscard]] const Expr *getOptionalExpression() const;
  [[nodiscard]] const ConditionalExpr *
  getOptionalConditionalExpression() const;
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
                 &&optConditionExpr);

  [[nodiscard]] const ConditionalExpr &getConditionalExpr() const;
  [[nodiscard]] const std::vector<
      std::pair<AssignmentOperator, ConditionalExpr>> &
  getOptionalConditionalExpr() const;
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
  Expr(std::vector<AssignExpr> assignExpressions);

  const std::vector<AssignExpr> &getAssignExpressions() const;
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
  ExprStmt(std::unique_ptr<Expr> &&optExpr = nullptr);
  [[nodiscard]] const Expr *getOptionalExpression() const;
  std::unique_ptr<Expr> moveOptionalExpr();
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
         std::unique_ptr<Stmt> &&optElseStmt = nullptr);

  [[nodiscard]] const Expr &getExpression() const;

  [[nodiscard]] const Stmt *getThenStmt() const;

  [[nodiscard]] const Stmt *getElseStmt() const;
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
  SwitchStmt(Expr &&expression, std::unique_ptr<Stmt> &&statement);

  [[nodiscard]] const Expr &getExpression() const;

  [[nodiscard]] const Stmt *getStatement() const;
};

/**
 * default-statement:
 *      default : statement
 */
class DefaultStmt final : public Node {
private:
  std::unique_ptr<Stmt> mStmt;

public:
  DefaultStmt(std::unique_ptr<Stmt> &&statement);
  [[nodiscard]] const Stmt *getStatement() const;
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
  CaseStmt(ConstantExpr &&constantExpr, std::unique_ptr<Stmt> &&statement);

  [[nodiscard]] const ConstantExpr &getConstantExpr() const;
  [[nodiscard]] const Stmt *getStatement() const;
};

/**
 * label-statement:
 *      identifier : statement
 */
class LabelStmt final : public Node {
private:
  std::string_view mIdentifier;

public:
  LabelStmt(std::string_view identifier);
  [[nodiscard]] const std::string_view &getIdentifier() const;
};

/**
 * goto-statement:
 *      goto identifier ;
 */
class GotoStmt final : public Node {
private:
  std::string_view mIdentifier;

public:
  GotoStmt(std::string_view identifier);
  [[nodiscard]] const std::string_view &getIdentifier() const;
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
  DoWhileStmt(std::unique_ptr<Stmt> &&stmt, Expr &&expr);
  [[nodiscard]] const Stmt *getStatement() const;
  [[nodiscard]] const Expr &getExpression() const;
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
  WhileStmt(Expr &&expr, std::unique_ptr<Stmt> &&stmt);
  [[nodiscard]] const Expr &getExpression() const;
  [[nodiscard]] const Stmt *getStatement() const;
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
          std::unique_ptr<Expr> &&postExpr = nullptr);
  [[nodiscard]] const Stmt *getStatement() const;

  [[nodiscard]] const std::variant<std::unique_ptr<Declaration>,
                                   std::unique_ptr<Expr>> &getInitial() const;
  [[nodiscard]] const Expr *getControlling() const;
  [[nodiscard]] const Expr *getPost() const;
};

/**
 * break-statement:
 *      break ;
 */
class BreakStmt final : public Node {
public:
  BreakStmt();
};

/**
 * continue-statement:
 *      continue ;
 */
class ContinueStmt final : public Node {
public:
  ContinueStmt();
};

/**
 * return-statement:
 *      return expr{opt} ;
 */
class ReturnStmt final : public Node {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  ReturnStmt(std::unique_ptr<Expr> &&optExpr = nullptr);
  [[nodiscard]] const Expr *getExpression() const;
};

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
  DeclarationSpecifiers mDeclarationSpecifiers;
  std::vector<InitDeclarator> mInitDeclarators;

public:
  Declaration(DeclarationSpecifiers &&declarationSpecifiers,
              std::vector<InitDeclarator> &&initDeclarators);
  [[nodiscard]] const DeclarationSpecifiers &
  getDeclarationSpecifiers() const;
  [[nodiscard]] const std::vector<InitDeclarator> &getInitDeclarators() const;
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
  BlockStmt(std::vector<BlockItem> &&blockItems);
  [[nodiscard]] const std::vector<BlockItem> &getBlockItems() const;
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
    std::variant<
    DirectAbstractDeclaratorParentheses,
                 DirectAbstractDeclaratorAssignExpr,
                 DirectAbstractDeclaratorAsterisk,
                 DirectAbstractDeclaratorParamTypeList>;

/**
 * direct-abstract-declarator:
 *      ( abstract-declarator )
 */
class DirectAbstractDeclaratorParentheses final : public Node {
  std::unique_ptr<AbstractDeclarator> mAbstractDeclarator;

public:
  DirectAbstractDeclaratorParentheses(
      std::unique_ptr<AbstractDeclarator> &&abstractDeclarator);

  [[nodiscard]] const AbstractDeclarator *getAbstractDeclarator() const;
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
  std::vector<TypeQualifier> mTypeQualifiers;
  std::unique_ptr<AssignExpr> mAssignmentExpression;
  bool mHasStatic{false};

public:
  DirectAbstractDeclaratorAssignExpr(
      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::vector<TypeQualifier> &&typeQualifiers,
      std::unique_ptr<AssignExpr> &&assignmentExpression,
      bool hasStatic);

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const;

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const;

  [[nodiscard]] const AssignExpr *getAssignmentExpression() const;

  [[nodiscard]] bool hasStatic() const;
};

/// direct-abstract-declarator{opt} [*]
class DirectAbstractDeclaratorAsterisk final : public Node {
  std::unique_ptr<DirectAbstractDeclarator> mDirectAbstractDeclarator;
public:
  DirectAbstractDeclaratorAsterisk(
      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator);

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const;
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
      std::unique_ptr<ParamTypeList> &&parameterTypeList);

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const;

  [[nodiscard]] const ParamTypeList *getParameterTypeList() const;
};

/**
 * pointer:
 *  type-qualifier-list{opt}
 *  type-qualifier-list{opt} pointer
 */
class Pointer final : public Node {
  std::vector<TypeQualifier> mTypeQualifiers;

public:
  Pointer(std::vector<TypeQualifier> &&typeQualifiers);

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const;
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
      std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator = {});

  [[nodiscard]] const std::vector<Pointer> &getPointers() const;

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const;
};

/**
 * parameter-declaration:
 *      declaration-specifiers declarator
 *      declaration-specifiers abstract-declarator{opt}
 */
struct ParameterDeclaration final : public Node {
public:
  DeclarationSpecifiers declarationSpecifiers;
  std::variant<std::unique_ptr<Declarator>, std::optional<std::unique_ptr<AbstractDeclarator>>>
      declarator;
public:
  ParameterDeclaration(DeclarationSpecifiers declarationSpecifiers,
                       std::variant<std::unique_ptr<Declarator>,
                       std::optional<std::unique_ptr<AbstractDeclarator>>>
                           variant = {std::nullopt});
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
  ParamList(std::vector<ParameterDeclaration> &&parameterList);

  [[nodiscard]] const std::vector<ParameterDeclaration> &
  getParameterDeclarations() const;
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
  ParamTypeList(ParamList &&parameterList, bool hasEllipse);

  [[nodiscard]] const ParamList &getParameterList() const;

  [[nodiscard]] bool hasEllipse() const;
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
    std::variant<DirectDeclaratorIdent, DirectDeclaratorParentheses,
                 DirectDeclaratorAssignExpr,DirectDeclaratorAsterisk,
                 DirectDeclaratorParamTypeList>;

/**
 * direct-declarator:
 *  identifier
 */
class DirectDeclaratorIdent final : public Node {
  std::string_view mIdent;

public:
  DirectDeclaratorIdent(std::string_view ident);

  [[nodiscard]] const std::string_view& getIdent() const;
};

/**
 * direct-declarator:
 *  ( declarator )
 */
class DirectDeclaratorParentheses final : public Node {
  std::unique_ptr<Declarator> mDeclarator;

public:
  DirectDeclaratorParentheses(std::unique_ptr<Declarator> &&declarator);

  [[nodiscard]] const Declarator *getDeclarator() const;
};

/**
 * direct-declarator:
 *  direct-declarator ( parameter-type-list )
 */
class DirectDeclaratorParamTypeList final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  ParamTypeList mParameterTypeList;

public:
  DirectDeclaratorParamTypeList(std::unique_ptr<DirectDeclarator> &&directDeclarator,
                              ParamTypeList &&parameterTypeList);

  [[nodiscard]] const DirectDeclarator *getDirectDeclarator() const;

  [[nodiscard]] const ParamTypeList &getParameterTypeList() const;
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
  std::vector<TypeQualifier> mTypeQualifierList;
  bool mHasStatic{false};
public:
  DirectDeclaratorAssignExpr(
      std::unique_ptr<DirectDeclarator> &&directDeclarator,
      std::vector<TypeQualifier> &&typeQualifierList,
      std::unique_ptr<AssignExpr> &&assignmentExpression,
      bool hasStatic);

  [[nodiscard]] const DirectDeclarator *getDirectDeclarator() const;

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifierList() const;

  [[nodiscard]] const AssignExpr *
  getAssignmentExpression() const;

  [[nodiscard]] bool hasStatic() const;
};

/**
 * direct-declarator:
 *   direct-declarator [ type-qualifier-list{opt} * ]
 */
class DirectDeclaratorAsterisk final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  std::vector<TypeQualifier> mTypeQualifierList;
public:
  DirectDeclaratorAsterisk(
      std::unique_ptr<DirectDeclarator> &&directDeclarator,
      std::vector<TypeQualifier> &&typeQualifierList);

  [[nodiscard]] const DirectDeclarator *getDirectDeclarator() const;

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifierList() const;
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
             DirectDeclarator &&directDeclarator);

  [[nodiscard]] const std::vector<Pointer> &getPointers() const;

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const;
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
public:
  struct StructDeclaration {
    DeclarationSpecifiers specifierQualifiers;
    struct StructDeclarator {
      std::unique_ptr<Declarator> optionalDeclarator;
      std::optional<ConstantExpr> optionalBitfield;
    };
    std::vector<StructDeclarator> structDeclarators;
  };
private:
  std::string_view mId;
  bool mIsUnion;
  std::vector<StructDeclaration> mStructDeclarations;

public:
  StructOrUnionSpecifier(bool isUnion, std::string_view identifier,
                         std::vector<StructDeclaration> &&structDeclarations);

  [[nodiscard]] bool isUnion() const;

  [[nodiscard]] std::string_view getTag() const;

  [[nodiscard]] const std::vector<StructDeclaration> &
  getStructDeclarations() const;
};

/**
 * enum-specifier:
 *  enum identifier{opt} { enumerator-list }
 *  enum identifier{opt} { enumerator-list , }
 *  enum identifier
 *
 * enumerator-list:
 *  enumerator
 *  enumerator-list , enumerator
 *
 * enumerator:
 *  enumeration-constant
 *  enumeration-constant = constant-expression
 *
 * enumeration-constant:
 *  identifier
 */
class EnumSpecifier final : public Node {
public:
  struct Enumerator {
    std::string_view mName;
    std::optional<ConstantExpr> mValue;
    Enumerator() = default;
    Enumerator(std::string_view name, std::optional<ConstantExpr>&& value = {})
        : mName(name), mValue(std::move(value)) {};
    Enumerator(const Enumerator &) = delete;
    Enumerator &operator=(const Enumerator &) = delete;
    Enumerator(Enumerator &&) = default;
    Enumerator &operator=(Enumerator &&) = default;
  };
private:
  std::string_view mId;
  std::vector<Enumerator> mEnumerators;
public:
  EnumSpecifier(std::string_view id, std::vector<Enumerator> &&enumerators);

  [[nodiscard]] const std::string_view &getName() const;
  [[nodiscard]] const std::vector<Enumerator> &getEnumerators() const;
};

/**
 * initializer:
 *  assignment-expression
 *  { initializer-list }
 *  { initializer-list , }
 */
class Initializer final : public Node {
  using variant = std::variant<AssignExpr, std::unique_ptr<InitializerList>>;
  variant mVariant;

public:
  Initializer(variant &&variant);

  [[nodiscard]] const variant &getVariant() const;
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
  using Designator = std::variant<ConstantExpr, std::string_view>;

  using DesignatorList = std::vector<Designator>;

  using vector = std::vector<std::pair<Initializer, DesignatorList>>;

private:
  vector mInitializer;

public:
  InitializerList(vector &&initializer);

  const vector &getInitializerList() const;
};

/**
 * function-definition:
 *  declaration-specifiers declarator declaration-list{opt} compound-statement
 */
class FunctionDefinition final : public Node {
  DeclarationSpecifiers mDeclarationSpecifiers;
  Declarator mDeclarator;
  BlockStmt mCompoundStatement;

public:
  FunctionDefinition(DeclarationSpecifiers &&declarationSpecifiers,
                     Declarator &&declarator, BlockStmt &&compoundStatement);

  [[nodiscard]] const DeclarationSpecifiers & getDeclarationSpecifiers() const;

  [[nodiscard]] const Declarator &getDeclarator() const;

  [[nodiscard]] const BlockStmt &getCompoundStatement() const;
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
