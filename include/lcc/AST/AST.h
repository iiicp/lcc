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
#include "lcc/Basic/Box.h"
#include "lcc/Basic/Util.h"
#include "lcc/Lexer/Token.h"
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

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
class PostFixExprTypeInitializer;

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
class CondExpr;
using ConstantExpr = CondExpr;
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
class DeclSpec;
class SpecifierQualifiers;
class StorageClsSpec;
class TypeQualifier;
class TypeSpec;
class EnumSpecifier;
class EnumeratorList;
class StructOrUnionSpec;
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

using ExprBox = box<Expr>;
using AssignExprBox = box<AssignExpr>;
using CastExprBox = box<CastExpr>;
using TypeNameBox = box<TypeName>;

using InitializerListBox = box<InitializerList>;
using AbstractDeclaratorBox = box<AbstractDeclarator>;

class Node {
private:
  TokIter beginTokLoc_;

public:
  Node(TokIter beginTokLoc) : beginTokLoc_(beginTokLoc) {}
  virtual ~Node() = default;
  Node(const Node &) = delete;
  Node &operator=(const Node &) = delete;
  Node(Node &&) = default;
  Node &operator=(Node &&) = default;
  TokIter getBeginLoc() const { return beginTokLoc_; }
};

/*
 * primary-expression:
 *    identifier
 */
class PrimaryExprIdent final : public Node {
private:
  std::string_view ident_;

public:
  PrimaryExprIdent(TokIter begin, std::string_view identifier)
      : Node(begin), ident_(identifier) {}
  [[nodiscard]] std::string_view getIdentifier() const { return ident_; }
};

/*
 * primary-expression:
 *    constant
 */
class PrimaryExprConstant final : public Node {
public:
  using Variant = std::variant<int32_t, uint32_t, int64_t, uint64_t, float,
                               double, std::string>;

private:
  Variant value_;

public:
  PrimaryExprConstant(TokIter begin, Variant &&value)
      : Node(begin), value_(value) {}
  [[nodiscard]] const Variant &getValue() const { return value_; }
};

/*
 * primary-expression:
 *    ( expression )
 */
class PrimaryExprParentheses final : public Node {
private:
  ExprBox expr_;

public:
  PrimaryExprParentheses(TokIter begin, ExprBox expr)
      : Node(begin), expr_(MV_(expr)) {}
  [[nodiscard]] const Expr &getExpr() const { return *expr_; }
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
    std::variant<PrimaryExpr, box<PostFixExprSubscript>,
                 box<PostFixExprFuncCall>, box<PostFixExprDot>,
                 box<PostFixExprArrow>, box<PostFixExprIncrement>,
                 box<PostFixExprDecrement>, box<PostFixExprTypeInitializer>>;

/**
 * postfix-expression:
 *    postfix-expression [ expression ]
 */
class PostFixExprSubscript final : public Node {
private:
  PostFixExpr postFixExpr_;
  ExprBox expr_;

public:
  PostFixExprSubscript(TokIter begin, PostFixExpr &&postFixExpr, ExprBox expr)
      : Node(begin), postFixExpr_(MV_(postFixExpr)), expr_(MV_(expr)) {}
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
  [[nodiscard]] const Expr &getExpr() const { return *expr_; }
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
  PostFixExpr postFixExpr_;
  std::vector<AssignExprBox> params_;

public:
  PostFixExprFuncCall(TokIter begin, PostFixExpr &&postFixExpr,
                      std::vector<AssignExprBox> &&params)
      : Node(begin), postFixExpr_(MV_(postFixExpr)), params_(MV_(params)) {}

  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
  [[nodiscard]] const std::vector<AssignExprBox> &
  getOptionalAssignExpressions() const {
    return params_;
  }
};

/**
 * postfix-expression:
 *    postfix-expression . identifier
 */
class PostFixExprDot final : public Node {
private:
  PostFixExpr postFixExpr_;
  std::string_view identifier_;

public:
  PostFixExprDot(TokIter begin, PostFixExpr &&postFixExpr,
                 std::string_view identifier)
      : Node(begin), postFixExpr_(MV_(postFixExpr)), identifier_(identifier) {}

  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
  [[nodiscard]] std::string_view getIdentifier() const { return identifier_; }
};

/**
 * postfix-expression:
 *    postfix-expression -> identifier
 */
class PostFixExprArrow final : public Node {
private:
  PostFixExpr postFixExpr_;
  std::string_view identifier_;

public:
  PostFixExprArrow(TokIter begin, PostFixExpr &&postFixExpr,
                   std::string_view identifier)
      : Node(begin), postFixExpr_(MV_(postFixExpr)), identifier_(identifier) {}
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
  [[nodiscard]] std::string_view getIdentifier() const { return identifier_; }
};

/**
 * postfix-expression:
 *    postfix-expression ++
 */
class PostFixExprIncrement final : public Node {
private:
  PostFixExpr postFixExpr_;

public:
  PostFixExprIncrement(TokIter begin, PostFixExpr &&postFixExpr)
      : Node(begin), postFixExpr_(MV_(postFixExpr)) {}
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
};

/**
 * postfix-expression:
 *    postfix-expression --
 */
class PostFixExprDecrement final : public Node {
private:
  PostFixExpr postFixExpr_;

public:
  PostFixExprDecrement(TokIter begin, PostFixExpr &&postFixExpr)
      : Node(begin), postFixExpr_(MV_(postFixExpr)) {}
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const {
    return postFixExpr_;
  }
};

/**
 * postfix-expression:
 *   ( type-name ) { initializer-list }
 *   ( type-name ) { initializer-list , }
 */
class PostFixExprTypeInitializer final : public Node {
private:
  TypeNameBox typeName_;
  InitializerListBox initializerList_;

public:
  PostFixExprTypeInitializer(TokIter begin, TypeNameBox typeName,
                             InitializerListBox initializerList)
      : Node(begin), typeName_(MV_(typeName)),
        initializerList_(MV_(initializerList)) {}

  [[nodiscard]] const InitializerList &getInitializerList() const {
    return *initializerList_;
  }
  [[nodiscard]] const TypeName &getTypeName() const { return *typeName_; }
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
using UnaryExpr = std::variant<PostFixExpr, box<UnaryExprUnaryOperator>,
                               box<UnaryExprSizeOf>>;

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
  enum class Op : uint8_t {
    Increment,
    Decrement,
    Ampersand,
    Asterisk,
    Plus,
    Minus,
    BitNot,
    LogicalNot
  };
  using Variant = std::variant<UnaryExpr, CastExprBox>;

private:
  Op operator_;
  Variant value_;

public:
  UnaryExprUnaryOperator(TokIter begin, Op anOperator, Variant &&value)
      : Node(begin), operator_(anOperator), value_(MV_(value)) {}

  [[nodiscard]] Op getOperator() const { return operator_; }
  [[nodiscard]] const CastExpr *getCastExpr() const {
    if (std::holds_alternative<CastExprBox>(value_)) {
      return std::get<CastExprBox>(value_).get();
    }
    return nullptr;
  }
};

/**
 * unary-expression:
 *  sizeof unary-expression
 *  sizeof ( type-name )
 */
class UnaryExprSizeOf final : public Node {
  using Variant = std::variant<UnaryExpr, TypeNameBox>;
  Variant value_;

public:
  UnaryExprSizeOf(TokIter begin, Variant &&variant)
      : Node(begin), value_(MV_(variant)) {}

  [[nodiscard]] const Variant &getVariant() const { return value_; }
};

/**
 * type-specifier:
 *  void char short int long float double signed unsigned _Bool
 *  struct-or-union-specifier
 *  enum-specifier
 *  typedef-name
 */
class TypeSpec final : public Node {
public:
  enum PrimTypeKind {
    Void = 1 << 0,
    Char = 1 << 1,
    Short = 1 << 2,
    Int = 1 << 3,
    Long = 1 << 4,
    Float = 1 << 5,
    Double = 1 << 6,
    Signed = 1 << 7,
    Unsigned = 1 << 8,
    Bool = 1 << 9
  };
  using TypedefName = std::string_view;

private:
  using Variant = std::variant<PrimTypeKind, box<StructOrUnionSpec>,
                               box<EnumSpecifier>, TypedefName>;

  Variant variant_;

public:
  TypeSpec(TokIter begin, Variant &&variant)
      : Node(begin), variant_(MV_(variant)) {}

  [[nodiscard]] const Variant &getVariant() const { return variant_; }
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
  TypeQualifier(TokIter begin, Qualifier qualifier)
      : Node(begin), mQualifier(qualifier) {}
  [[nodiscard]] Qualifier getQualifier() const { return mQualifier; }
};

/**
 * function-specifier:
 *      inline
 */
class FunctionSpecifier final : public Node {
public:
  FunctionSpecifier(TokIter begin) : Node(begin) {}
};

/**
 * storage-class-specifier:
 *      typedef
 *      extern
 *      static
 *      auto
 *      register
 */
class StorageClsSpec final : public Node {
public:
  enum Specifiers { Typedef, Extern, Static, Auto, Register };

private:
  Specifiers mSpecifier;

public:
  StorageClsSpec(TokIter begin, Specifiers specifier)
      : Node(begin), mSpecifier(specifier) {}
  [[nodiscard]] Specifiers getSpecifier() const { return mSpecifier; }
};

/**
 * declaration-specifiers:
        storage-class-specifier declaration-specifiers{opt}
        type-specifier declaration-specifiers{opt}
        type-qualifier declaration-specifiers{opt}
        function-specifier declaration-specifiers{opt}
 */
class DeclSpec final : public Node {
private:
  std::vector<StorageClsSpec> storageClassSpecifiers_;
  std::vector<TypeSpec> typeSpecifiers_;
  std::vector<TypeQualifier> typeQualifiers_;
  std::vector<FunctionSpecifier> functionSpecifiers_;

public:
  DeclSpec(TokIter begin) : Node(begin) {}
  void addStorageClassSpecifiers(StorageClsSpec &&specifier) {
    storageClassSpecifiers_.push_back(MV_(specifier));
  }
  void addTypeSpec(TypeSpec &&specifier) {
    typeSpecifiers_.push_back(MV_(specifier));
  }
  void addTypeQualifiers(TypeQualifier &&qualifier) {
    typeQualifiers_.push_back(MV_(qualifier));
  }
  void addFunctionSpecifier(FunctionSpecifier &&specifier) {
    functionSpecifiers_.push_back(MV_(specifier));
  }

  [[nodiscard]] const std::vector<StorageClsSpec> &
  getStorageClassSpecifiers() const {
    return storageClassSpecifiers_;
  }
  [[nodiscard]] const std::vector<TypeSpec> &getTypeSpecs() const {
    return typeSpecifiers_;
  }
  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const {
    return typeQualifiers_;
  }
  [[nodiscard]] const std::vector<FunctionSpecifier> &
  getFunctionSpecifier() const {
    return functionSpecifiers_;
  }
  [[nodiscard]] bool isEmpty() const {
    return storageClassSpecifiers_.empty() && typeSpecifiers_.empty() &&
           typeQualifiers_.empty() && functionSpecifiers_.empty();
  }
};

/**
 * SpecifierQualifier
 *      type-specifier
 *      type-qualifier
 */

/**
 * type-name:
 *  specifier-qualifier-list abstract-declarator{opt}
 */
class TypeName final : public Node {
private:
  DeclSpec mSpecifierQualifiers;
  std::optional<AbstractDeclaratorBox> mAbstractDeclarator;

public:
  TypeName(
      TokIter begin, DeclSpec specifierQualifiers,
      std::optional<AbstractDeclaratorBox> abstractDeclarator = {std::nullopt})
      : Node(begin), mSpecifierQualifiers(MV_(specifierQualifiers)),
        mAbstractDeclarator(MV_(abstractDeclarator)) {}
  [[nodiscard]] const DeclSpec &getSpecifierQualifiers() const {
    return mSpecifierQualifiers;
  }
  [[nodiscard]] const AbstractDeclarator *getAbstractDeclarator() const {
    if (mAbstractDeclarator.has_value()) {
      return mAbstractDeclarator.value().get();
    }
    return nullptr;
  }
};

/**
 * cast-expression:
 *      unary-expression
 *      ( type-name ) cast-expression
 */
class CastExpr final : public Node {
public:
  using TypeNameCast = std::pair<TypeName, CastExprBox>;
  using Variant = std::variant<UnaryExpr, TypeNameCast>;

public:
  Variant variant_;

public:
  CastExpr(TokIter begin, Variant &&unaryOrCast)
      : Node(begin), variant_(MV_(unaryOrCast)) {}
  [[nodiscard]] const Variant &getVariant() const { return variant_; }
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
  enum Op { Multiply, Divide, Modulo };

private:
  CastExpr castExpr_;
  std::vector<std::pair<Op, CastExpr>> optionalCastExps_;

public:
  explicit MultiExpr(TokIter begin, CastExpr &&castExpr,
                     std::vector<std::pair<Op, CastExpr>> &&optionalCastExps)
      : Node(begin), castExpr_(MV_(castExpr)),
        optionalCastExps_(MV_(optionalCastExps)) {}
  [[nodiscard]] const CastExpr &getCastExpr() const { return castExpr_; }
  [[nodiscard]] const std::vector<std::pair<Op, CastExpr>> &
  getOptionalCastExps() const {
    return optionalCastExps_;
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
  enum Op { Plus, Minus };

private:
  MultiExpr multiExpr_;
  std::vector<std::pair<Op, MultiExpr>> optionalMultiExps_;

public:
  AdditiveExpr(TokIter begin, MultiExpr &&multiExpr,
               std::vector<std::pair<Op, MultiExpr>> &&optionalMultiExps)
      : Node(begin), multiExpr_(MV_(multiExpr)),
        optionalMultiExps_(MV_(optionalMultiExps)) {}
  [[nodiscard]] const MultiExpr &getMultiExpr() const { return multiExpr_; }
  [[nodiscard]] const std::vector<std::pair<Op, MultiExpr>> &
  getOptionalMultiExps() const {
    return optionalMultiExps_;
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
  enum Op { Right, Left };

private:
  AdditiveExpr additiveExpr_;
  std::vector<std::pair<Op, AdditiveExpr>> optionalAdditiveExps_;

public:
  ShiftExpr(TokIter begin, AdditiveExpr &&additiveExpr,
            std::vector<std::pair<Op, AdditiveExpr>> &&optionalAdditiveExps)
      : Node(begin), additiveExpr_(MV_(additiveExpr)),
        optionalAdditiveExps_(MV_(optionalAdditiveExps)) {}
  [[nodiscard]] const AdditiveExpr &getAdditiveExpr() const {
    return additiveExpr_;
  }
  [[nodiscard]] const std::vector<std::pair<Op, AdditiveExpr>> &
  getOptAdditiveExps() const {
    return optionalAdditiveExps_;
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
  enum Op { LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual };

private:
  ShiftExpr shiftExpr_;
  std::vector<std::pair<Op, ShiftExpr>> optionalShiftExps_;

public:
  RelationalExpr(TokIter begin, ShiftExpr &&shiftExpr,
                 std::vector<std::pair<Op, ShiftExpr>> &&optionalShiftExps)
      : Node(begin), shiftExpr_(MV_(shiftExpr)),
        optionalShiftExps_(MV_(optionalShiftExps)) {}
  [[nodiscard]] const ShiftExpr &getShiftExpr() const { return shiftExpr_; }
  [[nodiscard]] const std::vector<std::pair<Op, ShiftExpr>> &
  getOptionalShiftExpressions() const {
    return optionalShiftExps_;
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
  enum Op { Equal, NotEqual };

private:
  RelationalExpr relationalExpr_;
  std::vector<std::pair<Op, RelationalExpr>> optionalRelationalExps_;

public:
  EqualExpr(TokIter begin, RelationalExpr &&relationalExpr,
            std::vector<std::pair<Op, RelationalExpr>> &&optionalRelationalExps)
      : Node(begin), relationalExpr_(MV_(relationalExpr)),
        optionalRelationalExps_(MV_(optionalRelationalExps)) {}
  [[nodiscard]] const RelationalExpr &getRelationalExpr() const {
    return relationalExpr_;
  }

  [[nodiscard]] const std::vector<std::pair<Op, RelationalExpr>> &
  getOptionalRelationalExpr() const {
    return optionalRelationalExps_;
  }
};

/**
 * AND-expression:
 *      equality-expression
 *      AND-expression & equality-expression
 */
class BitAndExpr final : public Node {
private:
  std::vector<EqualExpr> equalExps_;

public:
  BitAndExpr(TokIter begin, std::vector<EqualExpr> &&equalExps)
      : Node(begin), equalExps_(MV_(equalExps)) {}
  [[nodiscard]] const std::vector<EqualExpr> &getEqualExpr() const {
    return equalExps_;
  }
};

/**
 * exclusive-OR-expression:
 *      AND-expression
 *      exclusive-OR-expression ^ AND-expression
 */
class BitXorExpr final : public Node {
private:
  std::vector<BitAndExpr> bitAndExps_;

public:
  BitXorExpr(TokIter begin, std::vector<BitAndExpr> &&bitAndExps)
      : Node(begin), bitAndExps_(MV_(bitAndExps)) {}
  [[nodiscard]] const std::vector<BitAndExpr> &getBitAndExprs() const {
    return bitAndExps_;
  }
};

/**
 * inclusive-OR-expression:
 *      exclusive-OR-expression
 *      inclusive-OR-expression | exclusive-OR-expression
 */
class BitOrExpr final : public Node {
private:
  std::vector<BitXorExpr> bitXorExps_;

public:
  BitOrExpr(TokIter begin, std::vector<BitXorExpr> &&bitXorExps)
      : Node(begin), bitXorExps_(MV_(bitXorExps)) {}

  [[nodiscard]] const std::vector<BitXorExpr> &getBitXorExprs() const {
    return bitXorExps_;
  }
};

/**
 * logical-AND-expression:
 *      inclusive-OR-expression
 *      logical-AND-expression && inclusive-OR-expression
 */
class LogAndExpr final : public Node {
private:
  std::vector<BitOrExpr> bitOrExps_;

public:
  LogAndExpr(TokIter begin, std::vector<BitOrExpr> &&bitOrExps)
      : Node(begin), bitOrExps_(MV_(bitOrExps)) {}
  [[nodiscard]] const std::vector<BitOrExpr> &getBitOrExprs() const {
    return bitOrExps_;
  }
};

/**
 * logical-OR-expression:
 *      logical-AND-expression
 *      logical-OR-expression || logical-AND-expression
 */
class LogOrExpr final : public Node {
private:
  std::vector<LogAndExpr> logAndExps_;

public:
  LogOrExpr(TokIter begin, std::vector<LogAndExpr> &&logAndExps)
      : Node(begin), logAndExps_(MV_(logAndExps)) {}
  [[nodiscard]] const std::vector<LogAndExpr> &getLogAndExprs() const {
    return logAndExps_;
  }
};

/**
 * conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 */
class CondExpr final : public Node {
private:
  LogOrExpr logOrExpr_;
  std::optional<box<Expr>> optionalExpr_;
  std::optional<box<CondExpr>> optionalCondExpr_;

public:
  explicit CondExpr(
      TokIter begin, LogOrExpr &&logOrExpr,
      std::optional<box<Expr>> &&optionalExpr = {std::nullopt},
      std::optional<box<CondExpr>> &&optionalCondExpr = {std::nullopt})
      : Node(begin), logOrExpr_(MV_(logOrExpr)),
        optionalExpr_(MV_(optionalExpr)),
        optionalCondExpr_(MV_(optionalCondExpr)) {}
  [[nodiscard]] const LogOrExpr &getLogicalOrExpression() const {
    return logOrExpr_;
  }
  [[nodiscard]] const Expr *getOptionalExpression() const {
    if (optionalExpr_.has_value()) {
      return optionalExpr_.value().get();
    }
    return nullptr;
  }
  [[nodiscard]] const CondExpr *getOptionalConditionalExpression() const {
    if (optionalCondExpr_.has_value()) {
      return optionalCondExpr_.value().get();
    }
    return nullptr;
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
  enum AssignOp {
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
  CondExpr condExpr_;
  std::vector<std::pair<AssignOp, CondExpr>> optionalConditionExpr_;

public:
  AssignExpr(TokIter begin, CondExpr &&conditionalExpression,
             std::vector<std::pair<AssignOp, CondExpr>> &&optionalConditionExpr)
      : Node(begin), condExpr_(MV_(conditionalExpression)),
        optionalConditionExpr_(MV_(optionalConditionExpr)) {}

  [[nodiscard]] const CondExpr &getConditionalExpr() const { return condExpr_; }
  [[nodiscard]] const std::vector<std::pair<AssignOp, CondExpr>> &
  getOptionalConditionalExpr() const {
    return optionalConditionExpr_;
  }
};

/**
 expression:
    assignment-expression
    expression , assignment-expression
 */
class Expr final : public Node {
private:
  std::vector<AssignExpr> assignExpressions_;

public:
  Expr(TokIter begin, std::vector<AssignExpr> &&assignExpressions)
      : Node(begin), assignExpressions_(MV_(assignExpressions)) {}

  const std::vector<AssignExpr> &getAssignExpressions() const {
    return assignExpressions_;
  }
};

using Stmt =
    std::variant<box<ReturnStmt>, box<ExprStmt>, box<IfStmt>, box<BlockStmt>,
                 box<ForStmt>, box<WhileStmt>, box<DoWhileStmt>, box<BreakStmt>,
                 box<ContinueStmt>, box<SwitchStmt>, box<DefaultStmt>,
                 box<CaseStmt>, box<GotoStmt>, box<LabelStmt>>;

/**
 * expression-statement:
 *      expression{opt} ;
 */
class ExprStmt final : public Node {
private:
  std::optional<ExprBox> optionalExpr_;

public:
  ExprStmt(TokIter begin,
           std::optional<ExprBox> &&optionalExpr = {std::nullopt})
      : Node(begin), optionalExpr_(MV_(optionalExpr)) {}
  [[nodiscard]] const Expr *getOptionalExpression() const {
    if (optionalExpr_.has_value()) {
      return optionalExpr_.value().get();
    }
    return nullptr;
  }

  [[nodiscard]] ExprBox moveOptionalExpression() {
    assert(optionalExpr_);
    return MV_(*optionalExpr_);
  }
};

/**
 * if-statement:
 *      if ( expression ) statement
 *      if ( expression ) statement else statement
 */
class IfStmt final : public Node {
private:
  Expr expr_;
  Stmt thenStmt_;
  std::optional<Stmt> optionalElseStmt_;

public:
  IfStmt(TokIter begin, Expr &&expr, Stmt &&thenStmt,
         std::optional<Stmt> &&optionalElseStmt = {std::nullopt})
      : Node(begin), expr_(MV_(expr)), thenStmt_(MV_(thenStmt)),
        optionalElseStmt_(MV_(optionalElseStmt)) {}

  [[nodiscard]] const Expr &getExpression() const { return expr_; }

  [[nodiscard]] const Stmt &getThenStmt() const { return thenStmt_; }

  [[nodiscard]] const Stmt *getElseStmt() const {
    if (optionalElseStmt_.has_value()) {
      return &optionalElseStmt_.value();
    }
    return nullptr;
  }
};

/**
 * switch-statement:
 *      switch ( expression ) statement
 */
class SwitchStmt final : public Node {
private:
  Expr expr_;
  Stmt stmt_;

public:
  SwitchStmt(TokIter begin, Expr &&expression, Stmt &&statement)
      : Node(begin), expr_(MV_(expression)), stmt_(MV_(statement)) {}

  [[nodiscard]] const Expr &getExpression() const { return expr_; }

  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }
};

/**
 * default-statement:
 *      default : statement
 */
class DefaultStmt final : public Node {
private:
  Stmt stmt_;

public:
  DefaultStmt(TokIter begin, Stmt &&statement)
      : Node(begin), stmt_(MV_(statement)) {}
  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }
};

/**
 * case-statement:
 *      case constant-expression : statement
 */
class CaseStmt final : public Node {
private:
  ConstantExpr constantExpr_;
  Stmt stmt_;

public:
  CaseStmt(TokIter begin, ConstantExpr &&constantExpr, Stmt &&stmt)
      : Node(begin), constantExpr_(MV_(constantExpr)), stmt_(MV_(stmt)) {}

  [[nodiscard]] const ConstantExpr &getConstantExpr() const {
    return constantExpr_;
  }
  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }
};

/**
 * label-statement:
 *      identifier : statement
 */
class LabelStmt final : public Node {
private:
  std::string_view mIdentifier;

public:
  LabelStmt(TokIter begin, std::string_view identifier)
      : Node(begin), mIdentifier(identifier) {}
  [[nodiscard]] std::string_view getIdentifier() const { return mIdentifier; }
};

/**
 * goto-statement:
 *      goto identifier ;
 */
class GotoStmt final : public Node {
private:
  std::string_view mIdentifier;

public:
  GotoStmt(TokIter begin, std::string_view identifier)
      : Node(begin), mIdentifier(identifier) {}
  [[nodiscard]] std::string_view getIdentifier() const { return mIdentifier; }
};

/**
 * do-while-statement:
 *      do statement while ( expression ) ;
 */
class DoWhileStmt final : public Node {
private:
  Stmt stmt_;
  Expr expr_;

public:
  DoWhileStmt(TokIter begin, Stmt &&stmt, Expr &&expr)
      : Node(begin), stmt_(MV_(stmt)), expr_(MV_(expr)) {}
  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }
  [[nodiscard]] const Expr &getExpression() const { return expr_; }
};

/**
 * while-statement:
 *      while ( expression ) statement
 */
class WhileStmt final : public Node {
private:
  Expr expr_;
  Stmt stmt_;

public:
  WhileStmt(TokIter begin, Expr &&expr, Stmt &&stmt)
      : Node(begin), expr_(MV_(expr)), stmt_(MV_(stmt)) {}
  [[nodiscard]] const Expr &getExpression() const { return expr_; }
  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }
};

/**
 * for-statement:
 *      for ( expression{opt} ; expression{opt} ; expression{opt} ) statement
 *      for ( declaration expression{opt} ; expression{opt} ) statement
 */
class ForStmt final : public Node {
private:
  std::variant<box<Declaration>, std::optional<Expr>> initial_;
  std::optional<Expr> controlExpr_;
  std::optional<Expr> postExpr_;
  Stmt stmt_;

public:
  ForStmt(TokIter begin, Stmt stmt,
          std::variant<box<Declaration>, std::optional<Expr>> &&initial,
          std::optional<Expr> &&controlExpr = {std::nullopt},
          std::optional<Expr> &&postExpr = {std::nullopt})
      : Node(begin), initial_(MV_(initial)), controlExpr_(MV_(controlExpr)),
        postExpr_(MV_(postExpr)), stmt_(MV_(stmt)) {}
  [[nodiscard]] const Stmt &getStatement() const { return stmt_; }

  [[nodiscard]] const std::variant<box<Declaration>, std::optional<Expr>> &
  getInitial() const {
    return initial_;
  }
  [[nodiscard]] const Expr *getControlling() const {
    if (controlExpr_) {
      return &controlExpr_.value();
    }
    return nullptr;
  }
  [[nodiscard]] const Expr *getPost() const {
    if (postExpr_) {
      return &postExpr_.value();
    }
    return nullptr;
  }
};

/**
 * break-statement:
 *      break ;
 */
class BreakStmt final : public Node {
public:
  BreakStmt(TokIter begin) : Node(begin) {}
};

/**
 * continue-statement:
 *      continue ;
 */
class ContinueStmt final : public Node {
public:
  ContinueStmt(TokIter begin) : Node(begin) {}
};

/**
 * return-statement:
 *      return expr{opt} ;
 */
class ReturnStmt final : public Node {
private:
  std::optional<Expr> optionalExpr_;

public:
  ReturnStmt(TokIter begin, std::optional<Expr> &&optionalExpr = {std::nullopt})
      : Node(begin), optionalExpr_(MV_(optionalExpr)) {}
  [[nodiscard]] const Expr *getExpression() const {
    if (optionalExpr_) {
      return &optionalExpr_.value();
    }
    return nullptr;
  }
};

/**
 * initializer:
 *  assignment-expression
 *  { initializer-list }
 *  { initializer-list , }
 */
class Initializer final : public Node {
  using Variant = std::variant<AssignExpr, box<InitializerList>>;
  Variant variant_;

public:
  Initializer(TokIter begin, Variant &&variant)
      : Node(begin), variant_(MV_(variant)) {}

  [[nodiscard]] const Variant &getVariant() const { return variant_; }
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
  using Identifier = std::string_view;
  using Designator = std::variant<ConstantExpr, Identifier>;
  using DesignatorList = std::vector<Designator>;
  using Designation = DesignatorList;
  using InitializerPair = std::pair<std::optional<Designation>, Initializer>;

private:
  std::vector<InitializerPair> initializerPairs_;

public:
  InitializerList(TokIter begin,
                  std::vector<InitializerPair> &&initializerPairs)
      : Node(begin), initializerPairs_(MV_(initializerPairs)) {}

  const std::vector<InitializerPair> &getInitializerList() const {
    return initializerPairs_;
  }
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
    TokIter beginLoc_;
    box<Declarator> declarator_;
    std::optional<Initializer> optionalInitializer_;
  };

private:
  DeclSpec declarationSpecifiers_;
  std::vector<InitDeclarator> initDeclarators_;

public:
  Declaration(TokIter begin, DeclSpec &&declarationSpecifiers,
              std::vector<InitDeclarator> &&initDeclarators)
      : Node(begin), declarationSpecifiers_(MV_(declarationSpecifiers)),
        initDeclarators_(MV_(initDeclarators)) {}
  [[nodiscard]] const DeclSpec &getDeclarationSpecifiers() const {
    return declarationSpecifiers_;
  }
  [[nodiscard]] const std::vector<InitDeclarator> &getInitDeclarators() const {
    return initDeclarators_;
  }
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
  std::vector<BlockItem> blockItems_;

public:
  BlockStmt(TokIter begin, std::vector<BlockItem> &&blockItems)
      : Node(begin), blockItems_(MV_(blockItems)) {}
  [[nodiscard]] const std::vector<BlockItem> &getBlockItems() const {
    return blockItems_;
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
    std::variant<box<DirectAbstractDeclaratorParentheses>,
                 box<DirectAbstractDeclaratorAssignExpr>,
                 box<DirectAbstractDeclaratorAsterisk>,
                 box<DirectAbstractDeclaratorParamTypeList>>;

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
    std::variant<box<DirectDeclaratorIdent>, box<DirectDeclaratorParentheses>,
                 box<DirectDeclaratorAssignExpr>, box<DirectDeclaratorAsterisk>,
                 box<DirectDeclaratorParamTypeList>>;

/**
 * pointer:
 *  type-qualifier-list{opt}
 *  type-qualifier-list{opt} pointer
 */
class Pointer final : public Node {
  std::vector<TypeQualifier> typeQualifiers_;

public:
  Pointer(TokIter begin, std::vector<TypeQualifier> &&typeQualifiers)
      : Node(begin), typeQualifiers_(MV_(typeQualifiers)) {}

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const {
    return typeQualifiers_;
  }
};

/**
 * abstract-declarator:
 *  pointer
 *  pointer{opt} direct-abstract-declarator
 */
class AbstractDeclarator final : public Node {
  std::vector<Pointer> pointers_;
  std::optional<DirectAbstractDeclarator> directAbstractDeclarator_;

public:
  AbstractDeclarator(TokIter begin, std::vector<Pointer> &&pointers,
                     std::optional<DirectAbstractDeclarator>
                         &&directAbstractDeclarator = {std::nullopt})
      : Node(begin), pointers_(MV_(pointers)),
        directAbstractDeclarator_(MV_(directAbstractDeclarator)) {}

  [[nodiscard]] const std::vector<Pointer> &getPointers() const {
    return pointers_;
  }

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    if (directAbstractDeclarator_) {
      return &directAbstractDeclarator_.value();
    }
    return nullptr;
  }
};

/**
 * declarator:
 *  pointer{opt} direct-declarator
 */
class Declarator final : public Node {
  std::vector<Pointer> pointers_;
  DirectDeclarator directDeclarator_;

public:
  Declarator(TokIter begin, std::vector<Pointer> &&pointers,
             DirectDeclarator &&directDeclarator)
      : Node(begin), pointers_(MV_(pointers)),
        directDeclarator_(MV_(directDeclarator)) {}

  [[nodiscard]] const std::vector<Pointer> &getPointers() const {
    return pointers_;
  }

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const {
    return directDeclarator_;
  }
};

/**
 * parameter-declaration:
 *      declaration-specifiers declarator
 *      declaration-specifiers abstract-declarator{opt}
 */
class ParameterDeclaration final : public Node {
public:
  DeclSpec declSpec_;
  using Variant = std::variant<Declarator, std::optional<AbstractDeclarator>>;
  Variant declaratorKind_;

public:
  ParameterDeclaration(TokIter begin, DeclSpec &&declSpec,
                       Variant &&variant = {std::nullopt})
      : Node(begin), declSpec_(MV_(declSpec)), declaratorKind_(MV_(variant)) {}
  [[nodiscard]] const DeclSpec &getDeclSpec() const { return declSpec_; }
};

/**
 * parameter-list:
 *  parameter-declaration
 *  parameter-list , parameter-declaration
 */
class ParamList final : public Node {
private:
  std::vector<ParameterDeclaration> parameterList_;

public:
  ParamList(TokIter begin, std::vector<ParameterDeclaration> &&parameterList)
      : Node(begin), parameterList_(MV_(parameterList)) {}

  [[nodiscard]] const std::vector<ParameterDeclaration> &
  getParameterDeclarations() const {
    return parameterList_;
  }
};

/**
 * parameter-type-list:
 *  parameter-list
 *  parameter-list , ...
 */
class ParamTypeList final : public Node {
  ParamList parameterList_;
  bool hasEllipse_;

public:
  ParamTypeList(TokIter begin, ParamList &&parameterList, bool hasEllipse)
      : Node(begin), parameterList_(MV_(parameterList)),
        hasEllipse_(hasEllipse) {}

  [[nodiscard]] const ParamList &getParameterList() const {
    return parameterList_;
  }

  [[nodiscard]] bool hasEllipse() const { return hasEllipse_; }
};

/**
 * direct-abstract-declarator:
 *      ( abstract-declarator )
 */
class DirectAbstractDeclaratorParentheses final : public Node {
  AbstractDeclarator abstractDeclarator_;

public:
  DirectAbstractDeclaratorParentheses(TokIter begin,
                                      AbstractDeclarator &&abstractDeclarator)
      : Node(begin), abstractDeclarator_(MV_(abstractDeclarator)) {}

  [[nodiscard]] const AbstractDeclarator &getAbstractDeclarator() const {
    return abstractDeclarator_;
  }
};

/**
 * direct-abstract-declarator:
 *      direct-abstract-declarator{opt} [ type-qualifier-list{opt}
 *                                                  assignment-expression{opt} ]
 *      direct-abstract-declarator{opt} [ static
 *                              type-qualifier-list{opt} assignment-expression ]
 *      direct-abstract-declarator{opt} [ type-qualifier-list static
 * assignment-expression ]
 */
class DirectAbstractDeclaratorAssignExpr final : public Node {
  std::optional<DirectAbstractDeclarator> optionalDirectAbstractDeclarator_;
  std::vector<TypeQualifier> typeQualifiers_;
  std::optional<AssignExpr> optionalAssignExpr_;
  bool hasStatic_{false};

public:
  DirectAbstractDeclaratorAssignExpr(
      TokIter begin,
      std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::vector<TypeQualifier> &&typeQualifiers,
      std::optional<AssignExpr> &&assignExpr, bool hasStatic)
      : Node(begin),
        optionalDirectAbstractDeclarator_(MV_(directAbstractDeclarator)),
        typeQualifiers_(MV_(typeQualifiers)),
        optionalAssignExpr_(MV_(assignExpr)), hasStatic_(hasStatic) {}

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    if (optionalDirectAbstractDeclarator_) {
      return &optionalDirectAbstractDeclarator_.value();
    }
    return nullptr;
  }

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifiers() const {
    return typeQualifiers_;
  }

  [[nodiscard]] const AssignExpr *getAssignmentExpression() const {
    if (optionalAssignExpr_) {
      return &optionalAssignExpr_.value();
    }
    return nullptr;
  }

  [[nodiscard]] bool hasStatic() const { return hasStatic_; }
};

/// direct-abstract-declarator{opt} [*]
class DirectAbstractDeclaratorAsterisk final : public Node {
  std::optional<DirectAbstractDeclarator> optionalDirectAbstractDeclarator_;

public:
  DirectAbstractDeclaratorAsterisk(
      TokIter begin,
      std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator)
      : Node(begin),
        optionalDirectAbstractDeclarator_(MV_(directAbstractDeclarator)) {}

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    if (optionalDirectAbstractDeclarator_) {
      return &optionalDirectAbstractDeclarator_.value();
    }
    return nullptr;
  }
};

/**
 * direct-abstract-declarator:
 *  direct-abstract-declarator{opt} ( parameter-type-list{opt} )
 */
class DirectAbstractDeclaratorParamTypeList final : public Node {
  std::optional<DirectAbstractDeclarator> optionalDirectAbstractDeclarator_;
  std::optional<ParamTypeList> optionalParamTypeList_;

public:
  DirectAbstractDeclaratorParamTypeList(
      TokIter begin,
      std::optional<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::optional<ParamTypeList> &&paramTypeList)
      : Node(begin),
        optionalDirectAbstractDeclarator_(MV_(directAbstractDeclarator)),
        optionalParamTypeList_(MV_(paramTypeList)) {}

  [[nodiscard]] const DirectAbstractDeclarator *
  getDirectAbstractDeclarator() const {
    if (optionalDirectAbstractDeclarator_) {
      return &optionalDirectAbstractDeclarator_.value();
    }
    return nullptr;
  }

  [[nodiscard]] const ParamTypeList *getParameterTypeList() const {
    if (optionalParamTypeList_) {
      return &optionalParamTypeList_.value();
    }
    return nullptr;
  }
};

/**
 * direct-declarator:
 *  identifier
 */
class DirectDeclaratorIdent final : public Node {
  std::string_view mIdent;

public:
  DirectDeclaratorIdent(TokIter begin, std::string_view ident)
      : Node(begin), mIdent(ident) {}

  [[nodiscard]] const std::string_view &getIdent() const { return mIdent; }
};

/**
 * direct-declarator:
 *  ( declarator )
 */
class DirectDeclaratorParentheses final : public Node {
  Declarator declarator_;

public:
  DirectDeclaratorParentheses(TokIter begin, Declarator &&declarator)
      : Node(begin), declarator_(MV_(declarator)) {}

  [[nodiscard]] const Declarator &getDeclarator() const { return declarator_; }
};

/**
 * direct-declarator:
 *  direct-declarator ( parameter-type-list )
 */
class DirectDeclaratorParamTypeList final : public Node {
  DirectDeclarator directDeclarator_;
  ParamTypeList paramTypeList_;

public:
  DirectDeclaratorParamTypeList(TokIter begin,
                                DirectDeclarator &&directDeclarator,
                                ParamTypeList &&paramTypeList)
      : Node(begin), directDeclarator_(MV_(directDeclarator)),
        paramTypeList_(MV_(paramTypeList)) {}

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const {
    return directDeclarator_;
  }

  [[nodiscard]] const ParamTypeList &getParamTypeList() const {
    return paramTypeList_;
  }
};

/**
 * direct-declarator:
 *  direct-declarator [ type-qualifier-list{opt} assignment-expression{opt} ]
 *  direct-declarator [ static type-qualifier-list{opt} assignment-expression ]
 *  direct-declarator [ type-qualifier-list static assignment-expression ]
 */
class DirectDeclaratorAssignExpr final : public Node {
  DirectDeclarator directDeclarator_;
  std::optional<AssignExpr> optionalAssignExpr_;
  std::vector<TypeQualifier> typeQualifierList_;
  bool hasStatic_{false};

public:
  DirectDeclaratorAssignExpr(
      TokIter begin, DirectDeclarator &&directDeclarator,
      std::vector<TypeQualifier> &&typeQualifierList,
      std::optional<AssignExpr> &&assignExpr = {std::nullopt},
      bool hasStatic = false)
      : Node(begin), directDeclarator_(MV_(directDeclarator)),
        optionalAssignExpr_(MV_(assignExpr)),
        typeQualifierList_(MV_(typeQualifierList)), hasStatic_(hasStatic) {}

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const {
    return directDeclarator_;
  }

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifierList() const {
    return typeQualifierList_;
  }

  [[nodiscard]] const AssignExpr *getAssignmentExpression() const {
    if (optionalAssignExpr_) {
      return &optionalAssignExpr_.value();
    }
    return nullptr;
  }

  [[nodiscard]] bool hasStatic() const { return hasStatic_; }
};

/**
 * direct-declarator:
 *   direct-declarator [ type-qualifier-list{opt} * ]
 */
class DirectDeclaratorAsterisk final : public Node {
  DirectDeclarator directDeclarator_;
  std::vector<TypeQualifier> typeQualifierList_;

public:
  DirectDeclaratorAsterisk(TokIter begin, DirectDeclarator &&directDeclarator,
                           std::vector<TypeQualifier> &&typeQualifierList)
      : Node(begin), directDeclarator_(MV_(directDeclarator)),
        typeQualifierList_(MV_(typeQualifierList)) {}

  [[nodiscard]] const DirectDeclarator &getDirectDeclarator() const {
    return directDeclarator_;
  }

  [[nodiscard]] const std::vector<TypeQualifier> &getTypeQualifierList() const {
    return typeQualifierList_;
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
class StructOrUnionSpec final : public Node {
public:
  struct StructDeclarator {
    TokIter beginLoc_;
    std::optional<Declarator> optionalDeclarator_;
    std::optional<ConstantExpr> optionalBitfield_;
  };
  struct StructDeclaration {
    TokIter beginLoc_;
    DeclSpec specifierQualifiers_;
    std::vector<StructDeclarator> structDeclarators_;
  };

private:
  std::string_view name_;
  bool isUnion_;
  std::vector<StructDeclaration> structDeclarations_;

public:
  StructOrUnionSpec(TokIter begin, bool isUnion, std::string_view identifier,
                    std::vector<StructDeclaration> &&structDeclarations)
      : Node(begin), name_(identifier), isUnion_(isUnion),
        structDeclarations_(MV_(structDeclarations)) {}

  [[nodiscard]] bool isUnion() const { return isUnion_; }

  [[nodiscard]] std::string_view getTag() const { return name_; }

  [[nodiscard]] const std::vector<StructDeclaration> &
  getStructDeclarations() const {
    return structDeclarations_;
  }
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
    TokIter beginLoc_;
    std::string_view name_;
    std::optional<ConstantExpr> optionalConstantExpr_{std::nullopt};
  };

private:
  std::string_view tagName_;
  std::vector<Enumerator> enumerators_;

public:
  EnumSpecifier(TokIter begin, std::string_view tagName,
                std::vector<Enumerator> &&enumerators)
      : Node(begin), tagName_(tagName), enumerators_(MV_(enumerators)) {}

  [[nodiscard]] std::string_view getName() const { return tagName_; }
  [[nodiscard]] const std::vector<Enumerator> &getEnumerators() const {
    return enumerators_;
  }
};

/**
 * function-definition:
 *  declaration-specifiers declarator declaration-list{opt} compound-statement
 */
class FunctionDefinition final : public Node {
  DeclSpec declarationSpecifiers_;
  Declarator declarator_;
  BlockStmt compoundStmt_;

public:
  FunctionDefinition(TokIter begin, DeclSpec &&declarationSpecifiers,
                     Declarator &&declarator, BlockStmt &&compoundStmt)
      : Node(begin), declarationSpecifiers_(MV_(declarationSpecifiers)),
        declarator_(MV_(declarator)), compoundStmt_(MV_(compoundStmt)) {}

  [[nodiscard]] const DeclSpec &getDeclarationSpecifiers() const {
    return declarationSpecifiers_;
  }

  [[nodiscard]] const Declarator &getDeclarator() const { return declarator_; }

  [[nodiscard]] const BlockStmt &getCompoundStatement() const {
    return compoundStmt_;
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
  explicit TranslationUnit(TokIter begin,
                           std::vector<ExternalDeclaration> &&globals) noexcept
      : mGlobals(MV_(globals)) {}

  [[nodiscard]] const std::vector<ExternalDeclaration> &getGlobals() const {
    return mGlobals;
  }
};
} // namespace lcc::Syntax

#endif // LCC_SYNTAX_H
