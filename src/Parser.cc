/***********************************
 * File:     Parser.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/17
 ***********************************/

#include "Parser.h"
#include <stack>

namespace lcc {

std::unique_ptr<Program> Parser::ParseProgram() {
  std::vector<std::unique_ptr<ExternalDeclaration>> decls;
  while (mTokCursor != mTokEnd) {
    if (IsFunction()) {
      decls.push_back(std::move(ParseFunction()));
    } else {
      decls.push_back(std::move(ParseGlobalDecl()));
    }
  }
  return std::make_unique<Program>(std::move(decls));
}
std::unique_ptr<Function> Parser::ParseFunction() {
  assert(IsTypeName());
  auto retType = ParseType();
  assert(Expect(tok::identifier));
  std::string funcName = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(tok::identifier);
  assert(Match(tok::l_paren));
  std::vector<std::pair<std::unique_ptr<Type>, std::string>> params;
  while (mTokCursor->GetTokenType() != tok::r_paren) {
    auto ty = ParseType();
    std::string name;
    if (Match(tok::comma)) {
      name = "";
    } else {
      assert(mTokCursor->GetTokenType() == tok::identifier);
      name = std::get<std::string>(mTokCursor->GetTokenValue());
      Consume(tok::identifier);
      if (Peek(tok::comma))
        Consume(tok::comma);
    }
    params.push_back({std::move(ty), name});
  }
  Consume(tok::r_paren);
  if (mTokCursor->GetTokenType() == tok::semi) {
    Consume(tok::semi);
    return std::make_unique<Function>(std::move(retType), funcName,
                                      std::move(params));
  } else {
    return std::make_unique<Function>(std::move(retType), funcName,
                                      std::move(params), ParseBlockStmt());
  }
}
std::unique_ptr<GlobalDecl> Parser::ParseGlobalDecl() {
  assert(IsTypeName());
  auto ty = ParseType();
  assert(Expect(tok::identifier));
  std::string varName = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(tok::identifier);
  std::unique_ptr<GlobalDecl> globalDecl;
  if (Match(tok::equal)) {
    globalDecl = std::make_unique<GlobalDecl>(std::move(ty), varName, ParseConstantExpr());
  } else {
    globalDecl = std::make_unique<GlobalDecl>(std::move(ty), varName);
  }
  assert(Match(tok::semi));
  return std::move(globalDecl);
}

std::unique_ptr<ConstantExpr> Parser::ParseConstantExpr() {
  switch (mTokCursor->GetTokenType()) {
  case tok::char_constant: {
    ConstantExpr::ConstantValue val =
        std::get<int32_t>(mTokCursor->GetTokenValue());
    ConsumeAny();
    return std::make_unique<ConstantExpr>(val);
  }
  case tok::string_literal: {
    ConstantExpr::ConstantValue val =
        std::get<std::string>(mTokCursor->GetTokenValue());
    ConsumeAny();
    return std::make_unique<ConstantExpr>(val);
  }
  case tok::numeric_constant: {
    auto val = std::visit(
        [](auto &&value) -> ConstantExpr::ConstantValue {
          using T = std::decay_t<decltype(value)>;
          if constexpr (!std::is_same_v<T, std::monostate>) {
            return value;
          } else {
            return "";
          }
        },
        mTokCursor->GetTokenValue());
    ConsumeAny();
    return std::make_unique<ConstantExpr>(val);
  }
  default:
    assert(0);
    break;
  }
}

std::unique_ptr<Type> Parser::ParseType() {
  std::vector<tok::TokenKind> typeKinds;
  while (IsTypeName()) {
    typeKinds.push_back(mTokCursor->GetTokenType());
    ++mTokCursor;
  }
  assert(!typeKinds.empty());
  auto baseType = std::make_unique<PrimaryType>(std::move(typeKinds));
  return ParseType(std::move(baseType));
}

std::unique_ptr<Type> Parser::ParseType(std::unique_ptr<Type> &&baseType) {
  if (Match(tok::star)) {
    return std::make_unique<PointerType>(ParseType(std::move(baseType)));
  } else {
    return std::move(baseType);
  }
}

std::unique_ptr<Stmt> Parser::ParseStmt() {
  std::unique_ptr<Stmt> stmt;
  if (Peek(tok::kw_if)) {
    stmt = ParseIfStmt();
  }else if (Peek(tok::kw_do)) {
    stmt = ParseDoWhileStmt();
  }else if (Peek(tok::kw_while)) {
    stmt = ParseWhileStmt();
  }else if (Peek(tok::kw_for)) {
    TokIter start = mTokCursor;
    Consume(tok::kw_for);
    Match(tok::l_paren);
    if (IsTypeName()) {
      mTokCursor = start;
      stmt = ParseForDeclStmt();
    }else {
      mTokCursor = start;
      stmt = ParseForStmt();
    }
  }else if (IsTypeName()) {
    stmt = ParseDeclStmt();
  }else if (Peek(tok::kw_break)) {
    stmt = ParseBreakStmt();
  }else if (Peek(tok::kw_continue)) {
    stmt = ParseContinueStmt();
  }else if (Peek(tok::kw_return)) {
    stmt = ParseReturnStmt();
  }else if (Peek(tok::l_brace)) {
    stmt = ParseBlockStmt();
  }else{
    //Expect(tok::identifier);
    stmt = ParseExprStmt();
  }
  return stmt;
}

std::unique_ptr<BlockStmt> Parser::ParseBlockStmt() {
    assert(Match(tok::l_brace));
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (mTokCursor->GetTokenType() != tok::r_brace) {
      stmts.push_back(ParseStmt());
    }
    Consume(tok::r_brace);
    return std::make_unique<BlockStmt>(std::move(stmts));
}

std::unique_ptr<IfStmt> Parser::ParseIfStmt() {
  Consume(tok::kw_if);
  assert(Match(tok::l_paren));
  std::unique_ptr<Expr> expr = ParseExpr();
  assert(Match(tok::r_paren));
  std::unique_ptr<Stmt> thenStmt = ParseStmt();
  if (Match(tok::kw_else)) {
    return std::make_unique<IfStmt>(std::move(expr), std::move(thenStmt), ParseStmt());
  }else {
    return std::make_unique<IfStmt>(std::move(expr), std::move(thenStmt));
  }
}

std::unique_ptr<WhileStmt> Parser::ParseWhileStmt() {
  Consume(tok::kw_while);
  assert(Match(tok::l_paren));
  auto expr = ParseExpr();
  assert(Match(tok::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<WhileStmt>(std::move(expr), std::move(stmt));
}

std::unique_ptr<DoWhileStmt> Parser::ParseDoWhileStmt() {
  Consume(tok::kw_do);
  auto stmt = ParseStmt();
  assert(Match(tok::kw_while));
  assert(Match(tok::l_paren));
  auto expr = ParseExpr();
  assert(Match(tok::r_paren));
  assert(Match(tok::semi));
  return std::make_unique<DoWhileStmt>(std::move(stmt), std::move(expr));
}

std::unique_ptr<ForStmt> Parser::ParseForStmt() {
  Consume(tok::kw_for);
  assert(Match(tok::l_paren));
  std::unique_ptr<Expr> initExpr=nullptr, controlExpr=nullptr, postExpr=nullptr;
  if (!Peek(tok::semi)) {
    initExpr = ParseExpr();
  }
  assert(Match(tok::semi));

  if (!Peek(tok::semi)) {
    controlExpr = ParseExpr();
  }
  assert(Match(tok::semi));

  if (!Peek(tok::r_paren)) {
    postExpr = ParseExpr();
  }
  assert(Match(tok::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<ForStmt>(std::move(initExpr),
                                   std::move(controlExpr),
                                   std::move(postExpr), std::move(stmt));
}

std::unique_ptr<ForDeclarationStmt> Parser::ParseForDeclStmt() {
  Consume(tok::kw_for);
  assert(Match(tok::l_paren));
  assert(IsTypeName());
  std::unique_ptr<Declaration> initDecl = ParseDeclStmt();
  std::unique_ptr<Expr> controlExpr = nullptr, postExpr = nullptr;
  if (!Peek(tok::semi)) {
    controlExpr = ParseExpr();
  }
  assert(Match(tok::semi));
  if (!Peek(tok::r_paren)) {
    postExpr = ParseExpr();
  }
  assert(Match(tok::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<ForDeclarationStmt>(std::move(initDecl),
                                   std::move(controlExpr),
                                   std::move(postExpr), std::move(stmt));
}

std::unique_ptr<Declaration> Parser::ParseDeclStmt() {
  assert(IsTypeName());
  auto ty = ParseType();
  Expect(tok::identifier);
  std::string name = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(tok::identifier);
  if (Peek(tok::equal)) {
    Consume(tok::equal);
    auto expr = ParseExpr();
    assert(Match(tok::semi));
    return std::make_unique<Declaration>(std::move(ty), name, std::move(expr));
  }else {
    assert(Match(tok::semi));
    return std::make_unique<Declaration>(std::move(ty), name);
  }
}

std::unique_ptr<BreakStmt> Parser::ParseBreakStmt() {
    assert(Match(tok::kw_break));
    assert(Match(tok::semi));
    return std::make_unique<BreakStmt>();
}

std::unique_ptr<ContinueStmt> Parser::ParseContinueStmt() {
  assert(Match(tok::kw_continue));
  assert(Match(tok::semi));
  return std::make_unique<ContinueStmt>();
}

std::unique_ptr<ReturnStmt> Parser::ParseReturnStmt() {
  assert(Match(tok::kw_return));
  if (Peek(tok::semi)) {
    return std::make_unique<ReturnStmt>();
  }

  auto expr = ParseExpr();
  assert(Match(tok::semi));
  return std::make_unique<ReturnStmt>(std::move(expr));
}

std::unique_ptr<ExprStmt> Parser::ParseExprStmt() {
  if (Peek(tok::semi)) {
    return std::make_unique<ExprStmt>();
  }
  auto expr = ParseExpr();
  assert(Match(tok::semi));
  return std::make_unique<ExprStmt>(std::move(expr));
}

std::unique_ptr<Expr> Parser::ParseExpr() {
  auto assign = ParseAssignExpr();
  std::vector<std::unique_ptr<AssignExpr>> assignExps;
  while (Peek(tok::comma)) {
    Consume(tok::comma);
    assignExps.push_back(ParseAssignExpr());
  }
  return std::make_unique<Expr>(std::move(assign), std::move(assignExps));
}

std::unique_ptr<AssignExpr> Parser::ParseAssignExpr() {
  auto conditionExpr = ParseConditionalExpr();
  tok::TokenKind tokenType = tok::unknown;
  switch (mTokCursor->GetTokenType()) {
  case tok::equal:
  case tok::plus_equal:
  case tok::star_equal:
  case tok::minus_equal:
  case tok::slash_equal:
  case tok::percent_equal:
  case tok::less_less_equal:
  case tok::greater_greater_equal:
  case tok::pipe_equal:
  case tok::amp_equal:
  case tok::caret_equal: {
    tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    return std::make_unique<AssignExpr>(std::move(conditionExpr), tokenType, ParseAssignExpr());
  }
  default:
    return std::make_unique<AssignExpr>(std::move(conditionExpr));
  }
}

std::unique_ptr<ConditionalExpr> Parser::ParseConditionalExpr() {
  auto logOrExpr = ParseLogOrExpr();
  if (Peek(tok::question)) {
    Consume(tok::question);
    auto expr = ParseExpr();
    assert(Match(tok::colon));
    auto conditionalExpr = ParseConditionalExpr();
    return std::make_unique<ConditionalExpr>(std::move(logOrExpr), std::move(expr), std::move(conditionalExpr));
  }else {
    return std::make_unique<ConditionalExpr>(std::move(logOrExpr));
  }
}

std::unique_ptr<LogOrExpr> Parser::ParseLogOrExpr() {
  auto expr = ParseLogAndExpr();
  std::vector<std::unique_ptr<LogAndExpr>> logAndExprArr;
  while (Peek(tok::pipe_pipe)) {
    Consume(tok::pipe_pipe);
    logAndExprArr.push_back(ParseLogAndExpr());
  }
  return std::make_unique<LogOrExpr>(std::move(expr), std::move(logAndExprArr));
}

std::unique_ptr<LogAndExpr> Parser::ParseLogAndExpr() {
  auto expr = ParseBitOrExpr();
  std::vector<std::unique_ptr<BitOrExpr>> bitOrExprArr;
  while (Peek(tok::amp_amp)) {
    Consume(tok::amp_amp);
    bitOrExprArr.push_back(ParseBitOrExpr());
  }
  return std::make_unique<LogAndExpr>(std::move(expr), std::move(bitOrExprArr));
}

std::unique_ptr<BitOrExpr> Parser::ParseBitOrExpr() {
  auto expr = ParseBitXorExpr();
  std::vector<std::unique_ptr<BitXorExpr>> bitXorExprArr;
  while (Peek(tok::pipe)) {
    Consume(tok::pipe);
    bitXorExprArr.push_back(ParseBitXorExpr());
  }
  return std::make_unique<BitOrExpr>(std::move(expr), std::move(bitXorExprArr));
}

std::unique_ptr<BitXorExpr> Parser::ParseBitXorExpr() {
  auto expr = ParseBitAndExpr();
  std::vector<std::unique_ptr<BitAndExpr>> bitAndExprArr;
  while (Peek(tok::caret)) {
    Consume(tok::caret);
    bitAndExprArr.push_back(ParseBitAndExpr());
  }
  return std::make_unique<BitXorExpr>(std::move(expr), std::move(bitAndExprArr));
}

std::unique_ptr<BitAndExpr> Parser::ParseBitAndExpr() {
  auto expr = ParseEqualExpr();
  std::vector<std::unique_ptr<EqualExpr>> equalExprArr;
  while (Peek(tok::amp)) {
    Consume(tok::amp);
    equalExprArr.push_back(ParseEqualExpr());
  }
  return std::make_unique<BitAndExpr>(std::move(expr), std::move(equalExprArr));
}

std::unique_ptr<EqualExpr> Parser::ParseEqualExpr() {
  auto expr = ParseRelationalExpr();
  std::vector<std::pair<tok::TokenKind, std::unique_ptr<RelationalExpr>>> relationalExprArr;
  while(mTokCursor->GetTokenType() == tok::equal_equal
         ||mTokCursor->GetTokenType() == tok::exclaim_equal) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    relationalExprArr.push_back({tokenType, ParseRelationalExpr()});
  }
  return std::make_unique<EqualExpr>(std::move(expr), std::move(relationalExprArr));
}

std::unique_ptr<RelationalExpr>  Parser::ParseRelationalExpr() {
  auto expr = ParseShiftExpr();
  std::vector<std::pair<tok::TokenKind, std::unique_ptr<ShiftExpr>>> relationalExprArr;
  while (mTokCursor->GetTokenType() == tok::less || mTokCursor->GetTokenType() == tok::less_equal
         || mTokCursor->GetTokenType() == tok::greater || mTokCursor->GetTokenType() == tok::greater_equal) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    relationalExprArr.push_back({tokenType, ParseShiftExpr()});
  }
  return std::make_unique<RelationalExpr>(std::move(expr), std::move(relationalExprArr));
}

std::unique_ptr<ShiftExpr> Parser::ParseShiftExpr() {
  auto expr = ParseAdditiveExpr();
  std::vector<std::pair<tok::TokenKind, std::unique_ptr<AdditiveExpr>>> additiveExprArr;
  while (mTokCursor->GetTokenType() == tok::less_less || mTokCursor->GetTokenType() == tok::greater_greater) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    additiveExprArr.push_back({tokenType, ParseAdditiveExpr()});
  }
  return std::make_unique<ShiftExpr>(std::move(expr), std::move(additiveExprArr));
}

std::unique_ptr<AdditiveExpr> Parser::ParseAdditiveExpr() {
  auto expr = ParseMultiExpr();
  std::vector<std::pair<tok::TokenKind, std::unique_ptr<MultiExpr>>> multiExprArr;
  while (mTokCursor->GetTokenType() == tok::plus || mTokCursor->GetTokenType() == tok::minus) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    multiExprArr.push_back({tokenType, ParseMultiExpr()});
  }
  return std::make_unique<AdditiveExpr>(std::move(expr), std::move(multiExprArr));
}

std::unique_ptr<MultiExpr> Parser::ParseMultiExpr() {
  auto expr = ParseCastExpr();
  std::vector<std::pair<tok::TokenKind, std::unique_ptr<CastExpr>>> castExprArr;
  while (mTokCursor->GetTokenType() == tok::star || mTokCursor->GetTokenType() == tok::slash || mTokCursor->GetTokenType() == tok::percent) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    castExprArr.push_back({tokenType, ParseCastExpr()});
  }
  return std::make_unique<MultiExpr>(std::move(expr), std::move(castExprArr));
}

std::unique_ptr<CastExpr> Parser::ParseCastExpr() {
  TokIter start = mTokCursor;
  if (Peek(tok::l_paren)) {
    ConsumeAny();
    if(IsTypeName()) {
      auto ty = ParseType();
      assert(Match(tok::r_paren));
      return std::make_unique<CastExpr>(
          std::pair<std::unique_ptr<Type>, std::unique_ptr<CastExpr>>(
              std::move(ty), ParseCastExpr()));
    }else {
      mTokCursor = start;
      return std::make_unique<CastExpr>(ParseUnaryExpr());
    }
  } else {
    return std::make_unique<CastExpr>(ParseUnaryExpr());
  }
}

std::unique_ptr<UnaryExpr> Parser::ParseUnaryExpr() {
  if (IsUnaryOp(mTokCursor->GetTokenType())) {
    tok::TokenKind tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    auto expr = std::make_unique<UnaryExprUnaryOperator>(tokenType, ParseUnaryExpr());
    return std::make_unique<UnaryExpr>(std::move(expr));
  }else if (Peek(tok::kw_sizeof)) {
    Consume(tok::kw_sizeof);
    if (Peek(tok::l_paren)) {
      Consume(tok::l_paren);
      auto type = ParseType();
      Expect(tok::r_paren);
      ConsumeAny();
      auto expr = std::make_unique<UnaryExprSizeOf>(std::move(type));
      return std::make_unique<UnaryExpr>(std::move(expr));
    }else {
      auto expr = std::make_unique<UnaryExprSizeOf>(ParseUnaryExpr());
      return std::make_unique<UnaryExpr>(std::move(expr));
    }
  }else {
    auto expr = std::make_unique<UnaryExprPostFixExpr>(ParsePostFixExpr());
    return std::make_unique<UnaryExpr>(std::move(expr));
  }
}

std::unique_ptr<PostFixExpr> Parser::ParsePostFixExpr()
{
  std::stack<std::unique_ptr<PostFixExpr>> stack;
  stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprPrimary>(ParsePrimaryExpr())));
  while (IsPostFixExpr(mTokCursor->GetTokenType())) {
    auto tokType = mTokCursor->GetTokenType();
    if (tokType == tok::l_square) {
      assert(!stack.empty());
      Consume(tok::l_square);
      auto expr = ParseExpr();
      assert(Match(tok::r_square));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprSubscript>(std::move(postfixExpr), std::move(expr))));
    }else if (tokType == tok::l_paren) {
      Consume(tok::l_paren);
      std::vector<std::unique_ptr<AssignExpr>> params;
      if (!Peek(tok::r_paren)) {
        params.push_back(ParseAssignExpr());
      }
      while (mTokCursor->GetTokenType() != tok::r_paren) {
        assert(Match(tok::comma));
        params.push_back(ParseAssignExpr());
      }
      Consume(tok::r_paren);
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprFuncCall>(std::move(postfixExpr), std::move(params))));
    }else if (tokType == tok::period) {
      Consume(tok::period);
      Expect(tok::identifier);
      std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
      assert(Match(tok::identifier));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprDot>(std::move(postfixExpr), identifier)));
    }else if (tokType == tok::arrow) {
      Consume(tok::arrow);
      Expect(tok::identifier);
      std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
      assert(Match(tok::identifier));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprArrow>(std::move(postfixExpr), identifier)));
    }else if (tokType == tok::plus_plus) {
      Consume(tok::plus_plus);
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprIncrement>(std::move(postfixExpr))));
    }else if (tokType == tok::minus_minus) {
      Consume(tok::minus_minus);
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprDecrement>(std::move(postfixExpr))));
    }
  }
  assert(stack.size() == 1);
  auto ret = std::move(stack.top());
  stack.pop();
  return ret;
}

std::unique_ptr<PrimaryExpr> Parser::ParsePrimaryExpr() {
  if (Peek(tok::identifier)) {
    std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
    Consume(tok::identifier);
    auto expr = std::make_unique<PrimaryExprIdentifier>(identifier);
    return std::make_unique<PrimaryExpr>(std::move(expr));
  }else if (Peek(tok::char_constant) || Peek(tok::numeric_constant) || Peek(tok::string_literal)) {
    auto expr = std::make_unique<PrimaryExprConstant>(std::visit(
        [](auto &&val) -> std::variant<int32_t, uint32_t, int64_t, uint64_t,
                                       float, double, std::string> {
          using T = std::decay_t<decltype(val)>;
          if constexpr (std::is_same_v<T, std::monostate>) {
            assert(0);
          } else {
            return val;
          }
        },
        mTokCursor->GetTokenValue()));
    ConsumeAny();
    return std::make_unique<PrimaryExpr>(std::move(expr));
  }else {
    Expect(tok::l_paren);
    Consume(tok::l_paren);
    auto expr = ParseExpr();
    assert(Match(tok::r_paren));
    auto primaryExprParent = std::make_unique<PrimaryExprParent>(std::move(expr));
    return std::make_unique<PrimaryExpr>(std::move(primaryExprParent));
  }
}

bool Parser::IsFunction() {
  TokIter start = mTokCursor;
  assert(IsTypeName());

  while (IsTypeName()) {
    ++mTokCursor;
  }
  assert(Match(tok::identifier));

  bool isFunc = false;
  if (Match(tok::l_paren)) {
    isFunc = true;
  }
  mTokCursor = start;
  return isFunc;
}

bool Parser::IsTypeName() {
  tok::TokenKind type = mTokCursor->GetTokenType();
  return type == tok::kw_void | type == tok::kw_auto |
         type == tok::kw_char | type == tok::kw_short |
         type == tok::kw_int | type == tok::kw_long |
         type == tok::kw_float | type == tok::kw_double |
         type == tok::kw_signed | type == tok::kw_unsigned |
         type == tok::kw_const;
}

bool Parser::Match(tok::TokenKind tokenType) {
  if (mTokCursor->GetTokenType() == tokenType) {
    ++mTokCursor;
    return true;
  }
  return false;
}

bool Parser::Expect(tok::TokenKind tokenType) {
  if (mTokCursor->GetTokenType() == tokenType)
    return true;
  assert(0);
  return false;
}

bool Parser::Consume(tok::TokenKind tokenType) {
  if (mTokCursor->GetTokenType() == tokenType) {
    ++mTokCursor;
    return true;
  } else {
    assert(0);
    return false;
  }
}
bool Parser::ConsumeAny() {
  ++mTokCursor;
  return true;
}
bool Parser::Peek(tok::TokenKind tokenType) {
  return mTokCursor->GetTokenType() == tokenType;
}

bool Parser::IsUnaryOp(tok::TokenKind tokenType) {
  if (tokenType == tok::amp || tokenType == tok::star || tokenType == tok::plus ||
      tokenType == tok::minus || tokenType == tok::tilde || tokenType == tok::exclaim
      || tokenType == tok::plus_plus || tokenType == tok::minus_minus) {
    return true;
  }
  return false;
}

bool Parser::IsPostFixExpr(tok::TokenKind tokenType) {
  return ( tokenType == tok::l_paren || tokenType == tok::l_square
      || tokenType == tok::period || tokenType == tok::arrow
      || tokenType == tok::plus_plus || tokenType == tok::minus_minus
      || tokenType == tok::identifier || tokenType == tok::char_constant
          || tokenType == tok::numeric_constant);
}
} // namespace lcc::parser