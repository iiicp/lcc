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
#include "Lexer.h"
#include "SourceInterface.h"
#include "Token.h"
#include <string>
#include <variant>
#include <vector>
namespace lcc::Syntax {
class PrimaryExprIdentifier;
class PrimaryExprConstant;
class PrimaryExprParent;
class PrimaryExpr;
class PostFixExprSubscript;
class PostFixExprIncrement;
class PostFixExprDecrement;
class PostFixExprDot;
class PostFixExprArrow;
class PostFixExprFuncCall;
class PostFixExprPrimary;
class PostFixExpr;
class UnaryExprPostFixExpr;
class UnaryExprUnaryOperator;
class UnaryExprSizeOf;
class UnaryExpr;
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
class Declaration;
class ForDeclarationStmt;
class DoWhileStmt;
class WhileStmt;
class BreakStmt;
class ContinueStmt;
class GotoStmt;
class LabelStmt;
class BlockItem;
class Stmt;
class InitializerList;
class Initializer;

class FunctionDefinition;
class ExternalDeclaration;
class TranslationUnit;
class TypeName;
class Declarator;
class EnumSpecifier;
class EnumDeclaration;
class StructOrUnionSpecifier;
class TypeSpecifier;
class DirectDeclarator;
class DirectDeclaratorSquare;
class DirectDeclaratorParentParameters;
class AbstractDeclarator;
class DirectAbstractDeclarator;
class DirectAbstractDeclaratorParameterTypeList;
class DirectAbstractDeclaratorAssignmentExpression;
class Pointer;
class ParameterTypeList;
class ParameterList;

class Node {
public:
  Node(){};
  virtual ~Node() = default;
  Node(const Node &) = default;
  Node &operator=(const Node &) = default;
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
  Expr(std::vector<AssignExpr> assignExpressions);
  [[nodiscard]] const std::vector<AssignExpr> &getAssignExpressions() const;
};

class PrimaryExprIdentifier final : public Node {
private:
  std::string mIdentifier;

public:
  PrimaryExprIdentifier(std::string identifier);
  [[nodiscard]] const std::string &getIdentifier() const;
};

class PrimaryExprConstant final : public Node {
public:
  using Variant = std::variant<std::int32_t, std::uint32_t, std::int64_t,
                               std::uint64_t, float, double, std::string>;

private:
  Variant mVariant;

public:
  PrimaryExprConstant(Variant variant);
  [[nodiscard]] const Variant &getValue() const;
};

class PrimaryExprParent final : public Node {
private:
  Expr mExpr;

public:
  PrimaryExprParent(Expr &&expr);
  [[nodiscard]] const Expr &getExpr() const;
};

class PrimaryExpr final : public Node {
private:
  using Variant = std::variant<PrimaryExprConstant, PrimaryExprIdentifier,
                               PrimaryExprParent>;
  Variant mVariant;

public:
  PrimaryExpr(Variant &&variant);

  [[nodiscard]] const Variant &getVariant() const;
};

class PostFixExprPrimary final : public Node {
private:
  PrimaryExpr mPrimaryExpr;

public:
  PostFixExprPrimary(PrimaryExpr &&primaryExpr);

  [[nodiscard]] const PrimaryExpr &getPrimaryExpr() const;
};

class PostFixExprSubscript final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  Expr mExpr;

public:
  PostFixExprSubscript(std::unique_ptr<PostFixExpr> &&postFixExpr, Expr &&expr);

  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
  [[nodiscard]] const Expr &getExpr() const;
};

class PostFixExprDot final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string mIdentifier;

public:
  PostFixExprDot(std::unique_ptr<PostFixExpr> &&postFixExpr,
                 std::string identifier);

  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
  [[nodiscard]] const std::string &getIdentifier() const;
};

class PostFixExprArrow final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::string mIdentifier;

public:
  PostFixExprArrow(std::unique_ptr<PostFixExpr> &&postFixExpr,
                   std::string identifier);
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
  [[nodiscard]] const std::string &getIdentifier() const;
};

class PostFixExprFuncCall final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;
  std::vector<AssignExpr> mOptParams;

public:
  PostFixExprFuncCall(std::unique_ptr<PostFixExpr> &&postFixExpr,
                      std::vector<AssignExpr> &&optParams);

  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
  [[nodiscard]] const std::vector<AssignExpr> &
  getOptionalAssignExpressions() const;
};

class PostFixExprIncrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprIncrement(std::unique_ptr<PostFixExpr> &&postFixExpr);
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
};

class PostFixExprDecrement final : public Node {
private:
  std::unique_ptr<PostFixExpr> mPostFixExpr;

public:
  PostFixExprDecrement(std::unique_ptr<PostFixExpr> &&postFixExpr);
  [[nodiscard]] const PostFixExpr &getPostFixExpr() const;
};

class PostFixExpr final : public Node {
  using Variant =
      std::variant<PostFixExprPrimary, PostFixExprSubscript, PostFixExprDot,
                   PostFixExprArrow, PostFixExprFuncCall, PostFixExprIncrement,
                   PostFixExprDecrement>;
  Variant mVariant;

public:
  PostFixExpr(Variant &&variant);
  [[nodiscard]] const Variant &getVariant() const;
};

class UnaryExprUnaryOperator final : public Node {
private:
  tok::TokenKind mTok;
  std::unique_ptr<UnaryExpr> mUnaryExpr;

public:
  UnaryExprUnaryOperator(tok::TokenKind tokenKind,
                         std::unique_ptr<UnaryExpr> &&unaryExpr);

  [[nodiscard]] tok::TokenKind getOperator() const;
  [[nodiscard]] const UnaryExpr &getUnaryExpr() const;
};

class UnaryExprPostFixExpr final : public Node {
private:
  PostFixExpr mPostExpr;

public:
  UnaryExprPostFixExpr(PostFixExpr &&postExpr);

  [[nodiscard]] const PostFixExpr &getPostExpr() const;
};

class UnaryExprSizeOf final : public Node {
  using Variant =
      std::variant<std::unique_ptr<UnaryExpr>, std::unique_ptr<TypeName>>;
  Variant mValue;

public:
  UnaryExprSizeOf(Variant &&variant);

  [[nodiscard]] const Variant &getVariant() const;
};

class UnaryExpr final : public Node {
  using Variant = std::variant<UnaryExprPostFixExpr, UnaryExprUnaryOperator,
                               UnaryExprSizeOf>;
  Variant mVariant;

public:
  UnaryExpr(Variant &&variant);

  const Variant &getVariant() const;
};

class AssignExprAssign final : public Node {
private:
  UnaryExpr mUnaryExpr;
  tok::TokenKind mOperator;
  std::unique_ptr<AssignExpr> mAssignExpr;

public:
  AssignExprAssign(UnaryExpr &&unaryExpr,
                   tok::TokenKind tokenKind,
                   std::unique_ptr<AssignExpr> &&assignExpr);

  [[nodiscard]] const UnaryExpr &getUnaryExpr() const;
  [[nodiscard]] const AssignExpr &getAssignExpr() const;
  [[nodiscard]] const tok::TokenKind getOperator() const;
};

enum class TypeQualifier { Const, Restrict, Volatile };

using SpecifierQualifier = std::variant<TypeQualifier, TypeSpecifier>;

class TypeName final : public Node {
private:
  std::vector<SpecifierQualifier> mSpecifierQualifiers;
  std::unique_ptr<AbstractDeclarator> mAbstractDeclarator;

public:
  TypeName(std::vector<SpecifierQualifier> &&specifierQualifiers,
           std::unique_ptr<AbstractDeclarator> &&abstractDeclarator);

  const std::vector<SpecifierQualifier> &getSpecifierQualifiers() const;

  const AbstractDeclarator *getAbstractDeclarator() const;
};

class CastExpr final : public Node {
  using Variant =
      std::variant<UnaryExpr, std::pair<TypeName, std::unique_ptr<CastExpr>>>;

public:
  Variant mVariant;

public:
  CastExpr(Variant &&unaryOrCast);

  const Variant &getVariant() const;
};

class MultiExpr final : public Node {
private:
  CastExpr mCastExpr;
  std::vector<std::pair<tok::TokenKind, CastExpr>> mOptCastExps;

public:
  explicit MultiExpr(
      CastExpr &&castExpr,
      std::vector<std::pair<tok::TokenKind, CastExpr>> &&optCastExps);
  const CastExpr &getCastExpr() const;
  const std::vector<std::pair<tok::TokenKind, CastExpr>> &
  getOptionalCastExpr() const;
};

class AdditiveExpr final : public Node {
private:
  MultiExpr mMultiExpr;
  std::vector<std::pair<tok::TokenKind, MultiExpr>> mOptionalMultiExpr;

public:
  AdditiveExpr(
      MultiExpr &&multiExpr,
      std::vector<std::pair<tok::TokenKind, MultiExpr>> &&optionalMultiExps);
  const MultiExpr &getMultiExpr() const;
  const std::vector<std::pair<tok::TokenKind, MultiExpr>> &
  getOptionalMultiExpr() const;
};

class ShiftExpr final : public Node {
private:
  AdditiveExpr mAdditiveExpr;
  std::vector<std::pair<tok::TokenKind, AdditiveExpr>> mOptAdditiveExps;

public:
  ShiftExpr(
      AdditiveExpr &&additiveExpr,
      std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &&optAdditiveExps);
  const AdditiveExpr &getAdditiveExpr() const;
  const std::vector<std::pair<tok::TokenKind, AdditiveExpr>> &
  getOptAdditiveExps() const;
};

class RelationalExpr final : public Node {
private:
  ShiftExpr mShiftExpr;
  std::vector<std::pair<tok::TokenKind, ShiftExpr>> mOptShiftExps;

public:
  RelationalExpr(
      ShiftExpr &&shiftExpr,
      std::vector<std::pair<tok::TokenKind, ShiftExpr>> &&optShiftExps);
  const ShiftExpr &getShiftExpr() const;
  const std::vector<std::pair<tok::TokenKind, ShiftExpr>> &
  getOptionalShiftExpressions() const;
};

class EqualExpr final : public Node {
private:
  RelationalExpr mRelationalExpr;
  std::vector<std::pair<tok::TokenKind, RelationalExpr>> mOptRelationExps;

public:
  EqualExpr(RelationalExpr &&relationalExpr,
            std::vector<std::pair<tok::TokenKind, RelationalExpr>>
                &&optRelationalExps);
  const RelationalExpr &getRelationalExpr() const;

  const std::vector<std::pair<tok::TokenKind, RelationalExpr>> &
  getOptionalRelationalExpr() const;
};

class BitAndExpr final : public Node {
private:
  EqualExpr mEqualExpr;
  std::vector<EqualExpr> mOptEqualExps;

public:
  BitAndExpr(EqualExpr &&equalExpr, std::vector<EqualExpr> &&optEqualExps);
  const EqualExpr &getEqualExpr() const;

  const std::vector<EqualExpr> &getOptionalEqualExpr() const;
};

class BitXorExpr final : public Node {
private:
  BitAndExpr mBitAndExpr;
  std::vector<BitAndExpr> mOptBitAndExps;

public:
  BitXorExpr(BitAndExpr &&bitAndExpr, std::vector<BitAndExpr> &&optBitAndExps);
  const BitAndExpr &getBitAndExpr() const;
  const std::vector<BitAndExpr> &getOptionalBitAndExpressions() const;
};

class BitOrExpr final : public Node {
private:
  BitXorExpr mBitXorExpr;
  std::vector<BitXorExpr> mOptBitXorExps;

public:
  BitOrExpr(BitXorExpr &&bitXorExpr, std::vector<BitXorExpr> &&optBitXorExps);
  const BitXorExpr &getBitXorExpression() const;

  const std::vector<BitXorExpr> &getOptionalBitXorExpressions() const;
};

class LogAndExpr final : public Node {
private:
  BitOrExpr mBitOrExpr;
  std::vector<BitOrExpr> mOptBitOrExps;

public:
  LogAndExpr(BitOrExpr &&bitOrExpr, std::vector<BitOrExpr> &&optBitOrExps);
  const BitOrExpr &getBitOrExpression() const;
  const std::vector<BitOrExpr> &getOptionalBitOrExpressions() const;
};

class LogOrExpr final : public Node {
private:
  LogAndExpr mLogAndExpr;
  std::vector<LogAndExpr> mOptLogAndExps;

public:
  LogOrExpr(LogAndExpr &&logAndExpr, std::vector<LogAndExpr> &&optLogAndExps);
  const LogAndExpr &getAndExpression() const;

  const std::vector<LogAndExpr> &getOptionalAndExpressions() const;
};

class ConditionalExpr final : public Node {
private:
  LogOrExpr mLogOrExpr;
  std::unique_ptr<Expr> mOptExpr;
  std::unique_ptr<ConditionalExpr> mOptCondExpr;

public:
  explicit ConditionalExpr(
      LogOrExpr &&logOrExpr, std::unique_ptr<Expr> &&optExpr = nullptr,
      std::unique_ptr<ConditionalExpr> &&optCondExpr = nullptr);
  const LogOrExpr &getLogicalOrExpression() const;
  const Expr *getOptionalExpression() const;
  const ConditionalExpr *getOptionalConditionalExpression() const;
};

class AssignExpr final : public Node {
private:
  std::variant<AssignExprAssign, ConditionalExpr> mVariant;
  std::unique_ptr<ConditionalExpr> mCondExpr;
  tok::TokenKind mTokType;
  std::unique_ptr<AssignExpr> mAssignExpr;

public:
  AssignExpr(std::variant<AssignExprAssign, ConditionalExpr> &&variant);

  const std::variant<AssignExprAssign, ConditionalExpr> &getVariant() const;
};

class ExprStmt final : public Node {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  ExprStmt(std::unique_ptr<Expr> &&optExpr = nullptr);
  const Expr *getOptionalExpression() const;
  std::unique_ptr<Expr> moveOptionalExpr();
};

class IfStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mThenStmt;
  std::unique_ptr<Stmt> mOptElseStmt;

public:
  IfStmt(Expr &&expr, std::unique_ptr<Stmt> &&thenStmt,
         std::unique_ptr<Stmt> &&optElseStmt = nullptr);

  const Expr &getExpression() const;

  const Stmt &getThenStmt() const;

  const Stmt *getElseStmt() const;
};

class SwitchStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  SwitchStmt(Expr &&expression, std::unique_ptr<Stmt> &&statement);

  const Expr &getExpression() const;

  const Stmt &getStatement() const;
};

class DefaultStmt final : public Node {
private:
  std::unique_ptr<Stmt> mStmt;

public:
  DefaultStmt(std::unique_ptr<Stmt> &&statement);
  const Stmt &getStatement() const;
};

class CaseStmt final : public Node {
private:
  using constantVariant = std::variant<int32_t, uint32_t, int64_t, uint64_t, float, double, void *>;
  constantVariant mConstant;
  std::unique_ptr<Stmt> mStatement;

public:
  CaseStmt(const constantVariant &constant, std::unique_ptr<Stmt> &&statement);

  const constantVariant &getConstant() const;

  const Stmt *getStatement() const;
};

class LabelStmt final : public Node {
private:
  std::string mIdentifier;

public:
  LabelStmt(std::string identifier);
  const std::string &getIdentifier() const;
};

class GotoStmt final : public Node {
private:
  std::string mIdentifier;

public:
  GotoStmt(std::string identifier);
  const std::string &getIdentifier() const;
};

class DoWhileStmt final : public Node {
private:
  std::unique_ptr<Stmt> mStmt;
  Expr mExpr;

public:
  DoWhileStmt(std::unique_ptr<Stmt> &&stmt, Expr &&expr);
  const Stmt &getStatement() const;

  const Expr &getExpression() const;
};

class WhileStmt final : public Node {
private:
  Expr mExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  WhileStmt(Expr &&expr, std::unique_ptr<Stmt> &&stmt);
  const Expr &getExpression() const;
  const Stmt &getStatement() const;
};

class ForStmt final : public Node {
private:
  std::unique_ptr<Expr> mInitExpr;
  std::unique_ptr<Expr> mControlExpr;
  std::unique_ptr<Expr> mPostExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  ForStmt(std::unique_ptr<Stmt> &&stmt,
          std::unique_ptr<Expr> &&initExpr = nullptr,
          std::unique_ptr<Expr> &&controlExpr = nullptr,
          std::unique_ptr<Expr> &&postExpr = nullptr);
  const Stmt &getStatement() const;

  const Expr *getInitial() const;

  const Expr *getControlling() const;

  const Expr *getPost() const;
};

enum class StorageClassSpecifier { Typedef, Extern, Static, Auto, Register };

struct FunctionSpecifier {};

using DeclarationSpecifier = std::variant<StorageClassSpecifier, TypeSpecifier,
                                          TypeQualifier, FunctionSpecifier>;

class Declaration final : public Node {
private:
  std::vector<DeclarationSpecifier> mDeclarationSpecifiers;
  std::vector<
      std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>>
      mInitDeclarators;

public:
  Declaration(

      std::vector<DeclarationSpecifier> &&declarationSpecifiers,
      std::vector<std::pair<std::unique_ptr<Declarator>,
                            std::unique_ptr<Initializer>>> &&initDeclarators);
  const std::vector<DeclarationSpecifier> &getDeclarationSpecifiers() const;
  const std::vector<
      std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>> &
  getInitDeclarators() const;
};

class ForDeclarationStmt final : public Node {
private:
  Declaration mInitDecl;
  std::unique_ptr<Expr> mControlExpr;
  std::unique_ptr<Expr> mPostExpr;
  std::unique_ptr<Stmt> mStmt;

public:
  ForDeclarationStmt(std::unique_ptr<Stmt> &&stmt, Declaration &&initDecl,
                     std::unique_ptr<Expr> &&controlExpr = nullptr,
                     std::unique_ptr<Expr> &&postExpr = nullptr);
  const Stmt &getStatement() const;

  const Declaration &getInitial() const;

  const Expr *getControlling() const;

  const Expr *getPost() const;
};

class BreakStmt final : public Node {
public:
  BreakStmt();
};

class ContinueStmt final : public Node {
public:
  ContinueStmt();
};

class ReturnStmt final : public Node {
private:
  std::unique_ptr<Expr> mOptExpr;

public:
  ReturnStmt(std::unique_ptr<Expr> &&optExpr = nullptr);
  const Expr *getExpression() const;
};

class BlockStmt final : public Node {
private:
  std::vector<BlockItem> mBlockItems;

public:
  BlockStmt(std::vector<BlockItem> &&blockItems);
  const std::vector<BlockItem> &getBlockItems() const;
};

class Stmt final : public Node {
private:
  using variant = std::variant<ReturnStmt, ExprStmt, IfStmt, BlockStmt, ForStmt,
                               ForDeclarationStmt, WhileStmt, DoWhileStmt,
                               BreakStmt, ContinueStmt, SwitchStmt, DefaultStmt,
                               CaseStmt, GotoStmt, LabelStmt>;
  variant mVariant;

public:
  Stmt(variant &&variant);
  const variant &getVariant() const;
  variant &getVariant();
};

class BlockItem final : public Node {
private:
  using variant = std::variant<Stmt, Declaration>;
  variant mVariant;

public:
  BlockItem(variant &&variant);

  const variant &getVariant() const;

  variant &getVariant();
};

class DirectAbstractDeclaratorAssignmentExpression final : public Node {
  std::unique_ptr<DirectAbstractDeclarator> mDirectAbstractDeclarator;
  std::unique_ptr<AssignExpr> mAssignmentExpression;

public:
  DirectAbstractDeclaratorAssignmentExpression(

      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::unique_ptr<AssignExpr> &&assignmentExpression);

  const DirectAbstractDeclarator *getDirectAbstractDeclarator() const;

  const AssignExpr *getAssignmentExpression() const;
};

class DirectAbstractDeclaratorParameterTypeList final : public Node {
  std::unique_ptr<DirectAbstractDeclarator> mDirectAbstractDeclarator;
  std::unique_ptr<ParameterTypeList> mParameterTypeList;

public:
  DirectAbstractDeclaratorParameterTypeList(

      std::unique_ptr<DirectAbstractDeclarator> &&directAbstractDeclarator,
      std::unique_ptr<ParameterTypeList> &&parameterTypeList);

  const DirectAbstractDeclarator *getDirectAbstractDeclarator() const;

  const ParameterTypeList *getParameterTypeList() const;
};

class DirectAbstractDeclarator final : public Node {
  using variant = std::variant<std::unique_ptr<AbstractDeclarator>,
                               DirectAbstractDeclaratorAssignmentExpression,
                               std::unique_ptr<DirectAbstractDeclarator>,
                               DirectAbstractDeclaratorParameterTypeList>;

  variant mVariant;

public:
  DirectAbstractDeclarator(variant &&variant);

  const variant &getVariant() const;
};

class AbstractDeclarator final : public Node {
  std::vector<Pointer> mPointers;
  DirectAbstractDeclarator mDirectAbstractDeclarator;

public:
  AbstractDeclarator(std::vector<Pointer> &&pointers,
                     DirectAbstractDeclarator &&directAbstractDeclarator);

  const std::vector<Pointer> &getPointers() const;

  const DirectAbstractDeclarator &getDirectAbstractDeclarator() const;
};

using ParameterDeclaration =
    std::pair<std::vector<DeclarationSpecifier>,
              std::variant<std::unique_ptr<Declarator>,
                           std::unique_ptr<AbstractDeclarator>>>;

class ParameterList final : public Node {
private:
  std::vector<ParameterDeclaration> mParameterList;

public:
  ParameterList(std::vector<ParameterDeclaration> &&parameterList);

  const std::vector<ParameterDeclaration> &getParameterDeclarations() const;
};

class ParameterTypeList final : public Node {
  ParameterList mParameterList;
  bool mHasEllipse;

public:
  ParameterTypeList(ParameterList &&parameterList, bool hasEllipse);

  const ParameterList &getParameterList() const;

  bool hasEllipse() const;
};

class DirectDeclaratorParentParameters final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  ParameterTypeList mParameterTypeList;

public:
  DirectDeclaratorParentParameters(DirectDeclarator &&directDeclarator,
                                   ParameterTypeList &&parameterTypeList);

  const DirectDeclarator &getDirectDeclarator() const;

  const ParameterTypeList &getParameterTypeList() const;
};

class DirectDeclaratorSquare final : public Node {
  std::unique_ptr<DirectDeclarator> mDirectDeclarator;
  std::vector<TypeQualifier> mTypeQualifiers;
  std::unique_ptr<AssignExpr> mAssignmentExpression;

public:
  DirectDeclaratorSquare(
      std::unique_ptr<DirectDeclarator> &&directDeclarator,
      std::vector<TypeQualifier> &&typeQualifiers,
      std::unique_ptr<AssignExpr> &&assignmentExpression);

  const DirectDeclarator &getDirectDeclarator() const;

  const std::vector<TypeQualifier> &getTypeQualifiers() const;

  const std::unique_ptr<AssignExpr> &getAssignmentExpression() const;
};

class DirectDeclarator final : public Node {
  using variant =
      std::variant<std::string, std::unique_ptr<Declarator>,
                   DirectDeclaratorSquare, DirectDeclaratorParentParameters>;

  variant mVariant;

public:
  DirectDeclarator(variant &&variant);

  const variant &getVariant() const;
};

class Declarator final : public Node {
  std::vector<Pointer> mPointers;
  DirectDeclarator mDirectDeclarator;

public:
  Declarator(std::vector<Pointer> &&pointers,
             DirectDeclarator &&directDeclarator);

  const std::vector<Pointer> &getPointers() const;

  const DirectDeclarator &getDirectDeclarator() const;
};

class StructOrUnionSpecifier final : public Node {
  bool mIsUnion;
  std::string mIdentifier;

public:
  struct StructDeclaration {
    std::vector<SpecifierQualifier> specifierQualifiers;
    std::vector<std::unique_ptr<Declarator>> structDeclarators;
  };

private:
  std::vector<StructDeclaration> mStructDeclarations;

public:
  StructOrUnionSpecifier(bool isUnion, std::string identifier,
                         std::vector<StructDeclaration> &&structDeclarations);

  bool isUnion() const;

  const std::string &getIdentifier() const;

  const std::vector<StructDeclaration> &getStructDeclarations() const;
};

class EnumDeclaration final : public Node {
  std::string mName;
  std::vector<std::pair<std::string, std::int32_t>> mValues;

public:
  EnumDeclaration(std::string name,
                  std::vector<std::pair<std::string, std::int32_t>> values);

  const std::string &getName() const;

  const std::vector<std::pair<std::string, std::int32_t>> &getValues() const;
};

class EnumSpecifier final : public Node {
  using variant = std::variant<EnumDeclaration, std::string>;

  variant mVariant;

public:
  EnumSpecifier(variant &&variant);

  const variant &getVariant() const;
};

class TypeSpecifier final : public Node {
public:
  enum class PrimitiveTypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
  };

private:
  using variant = std::variant<PrimitiveTypeSpecifier,
                               std::unique_ptr<StructOrUnionSpecifier>,
                               std::unique_ptr<EnumSpecifier>, std::string>;

  variant mVariant;

public:
  TypeSpecifier(variant &&variant);

  const variant &getVariant() const;
};

class Pointer final : public Node {
  std::vector<TypeQualifier> mTypeQualifiers;

public:
  Pointer(std::vector<TypeQualifier> &&typeQualifiers);

  const std::vector<TypeQualifier> &getTypeQualifiers() const;
};

class InitializerList final : public Node {
public:
  using Designator = std::variant<std::size_t, std::string>;

  using DesignatorList = std::vector<Designator>;

  using vector = std::vector<std::pair<Initializer, DesignatorList>>;

private:
  vector mNonCommaExpressionsAndBlocks;

public:
  InitializerList(vector &&nonCommaExpressionsAndBlocks);

  const vector &getNonCommaExpressionsAndBlocks() const;
};

class Initializer final : public Node {
  using variant = std::variant<AssignExpr, InitializerList>;
  variant mVariant;

public:
  Initializer(variant &&variant);

  const variant &getVariant() const;
};

class FunctionDefinition final : public Node {
  std::vector<DeclarationSpecifier> mDeclarationSpecifiers;
  Declarator mDeclarator;
  BlockStmt mCompoundStatement;

public:
  FunctionDefinition(std::vector<DeclarationSpecifier> &&declarationSpecifiers,
                     Declarator &&declarator,
                     BlockStmt &&compoundStatement);

  const std::vector<DeclarationSpecifier> &getDeclarationSpecifiers() const;

  const Declarator &getDeclarator() const;

  const BlockStmt &getCompoundStatement() const;
};

class ExternalDeclaration final : public Node {
  using variant = std::variant<Declaration, FunctionDefinition>;
  variant mVariant;

public:
  ExternalDeclaration(variant &&variant);

  const variant &getVariant() const;
};

class TranslationUnit final {
  std::vector<ExternalDeclaration> mGlobals;

public:
  explicit TranslationUnit(std::vector<ExternalDeclaration> &&globals) noexcept;

  const std::vector<ExternalDeclaration> &getGlobals() const;
};
} // namespace lcc::Syntax

#endif // LCC_SYNTAX_H
