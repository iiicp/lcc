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

namespace lcc::parser {

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
  assert(Expect(lexer::identifier));
  std::string funcName = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(lexer::identifier);
  assert(Match(lexer::l_paren));
  std::vector<std::pair<std::unique_ptr<Type>, std::string>> params;
  while (mTokCursor->GetTokenType() != lexer::r_paren) {
    auto ty = ParseType();
    std::string name;
    if (Match(lexer::comma)) {
      name = "";
    } else {
      assert(mTokCursor->GetTokenType() == lexer::identifier);
      name = std::get<std::string>(mTokCursor->GetTokenValue());
      Consume(lexer::identifier);
      if (Peek(lexer::comma))
        Consume(lexer::comma);
    }
    params.push_back({std::move(ty), name});
  }
  Consume(lexer::r_paren);
  if (mTokCursor->GetTokenType() == lexer::semi) {
    Consume(lexer::semi);
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
  assert(Expect(lexer::identifier));
  std::string varName = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(lexer::identifier);
  std::unique_ptr<GlobalDecl> globalDecl;
  if (Match(lexer::equal)) {
    globalDecl = std::make_unique<GlobalDecl>(std::move(ty), varName, ParseConstantExpr());
  } else {
    globalDecl = std::make_unique<GlobalDecl>(std::move(ty), varName);
  }
  assert(Match(lexer::semi));
  return std::move(globalDecl);
}

std::unique_ptr<ConstantExpr> Parser::ParseConstantExpr() {
  switch (mTokCursor->GetTokenType()) {
  case lexer::char_constant: {
    ConstantExpr::ConstantValue val =
        std::get<int32_t>(mTokCursor->GetTokenValue());
    ConsumeAny();
    return std::make_unique<ConstantExpr>(val);
  }
  case lexer::string_literal: {
    ConstantExpr::ConstantValue val =
        std::get<std::string>(mTokCursor->GetTokenValue());
    ConsumeAny();
    return std::make_unique<ConstantExpr>(val);
  }
  case lexer::numeric_constant: {
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
  std::vector<lexer::TokenType> typeKinds;
  while (IsTypeName()) {
    typeKinds.push_back(mTokCursor->GetTokenType());
    ++mTokCursor;
  }
  assert(!typeKinds.empty());
  auto baseType = std::make_unique<PrimaryType>(std::move(typeKinds));
  return ParseType(std::move(baseType));
}

std::unique_ptr<Type> Parser::ParseType(std::unique_ptr<Type> &&baseType) {
  if (Match(lexer::star)) {
    return std::make_unique<PointerType>(ParseType(std::move(baseType)));
  } else {
    return std::move(baseType);
  }
}

std::unique_ptr<Stmt> Parser::ParseStmt() {
  std::unique_ptr<Stmt> stmt;
  if (Peek(lexer::kw_if)) {
    stmt = ParseIfStmt();
  }else if (Peek(lexer::kw_do)) {
    stmt = ParseDoWhileStmt();
  }else if (Peek(lexer::kw_while)) {
    stmt = ParseWhileStmt();
  }else if (Peek(lexer::kw_for)) {
    TokIter start = mTokCursor;
    Consume(lexer::kw_for);
    Match(lexer::l_paren);
    if (IsTypeName()) {
      mTokCursor = start;
      stmt = ParseForDeclStmt();
    }else {
      mTokCursor = start;
      stmt = ParseForStmt();
    }
  }else if (IsTypeName()) {
    stmt = ParseDeclStmt();
  }else if (Peek(lexer::kw_break)) {
    stmt = ParseBreakStmt();
  }else if (Peek(lexer::kw_continue)) {
    stmt = ParseContinueStmt();
  }else if (Peek(lexer::kw_return)) {
    stmt = ParseReturnStmt();
  }else if (Peek(lexer::l_brace)) {
    stmt = ParseBlockStmt();
  }else{
    //Expect(lexer::identifier);
    stmt = ParseExprStmt();
  }
  return stmt;
}

std::unique_ptr<BlockStmt> Parser::ParseBlockStmt() {
    assert(Match(lexer::l_brace));
    std::vector<std::unique_ptr<Stmt>> stmts;
    while (mTokCursor->GetTokenType() != lexer::r_brace) {
      stmts.push_back(ParseStmt());
    }
    Consume(lexer::r_brace);
    return std::make_unique<BlockStmt>(std::move(stmts));
}

std::unique_ptr<IfStmt> Parser::ParseIfStmt() {
  Consume(lexer::kw_if);
  assert(Match(lexer::l_paren));
  std::unique_ptr<Expr> expr = ParseExpr();
  assert(Match(lexer::r_paren));
  std::unique_ptr<Stmt> thenStmt = ParseStmt();
  if (Match(lexer::kw_else)) {
    return std::make_unique<IfStmt>(std::move(expr), std::move(thenStmt), ParseStmt());
  }else {
    return std::make_unique<IfStmt>(std::move(expr), std::move(thenStmt));
  }
}

std::unique_ptr<WhileStmt> Parser::ParseWhileStmt() {
  Consume(lexer::kw_while);
  assert(Match(lexer::l_paren));
  auto expr = ParseExpr();
  assert(Match(lexer::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<WhileStmt>(std::move(expr), std::move(stmt));
}

std::unique_ptr<DoWhileStmt> Parser::ParseDoWhileStmt() {
  Consume(lexer::kw_do);
  auto stmt = ParseStmt();
  assert(Match(lexer::kw_while));
  assert(Match(lexer::l_paren));
  auto expr = ParseExpr();
  assert(Match(lexer::r_paren));
  assert(Match(lexer::semi));
  return std::make_unique<DoWhileStmt>(std::move(stmt), std::move(expr));
}

std::unique_ptr<ForStmt> Parser::ParseForStmt() {
  Consume(lexer::kw_for);
  assert(Match(lexer::l_paren));
  std::unique_ptr<Expr> initExpr=nullptr, controlExpr=nullptr, postExpr=nullptr;
  if (!Peek(lexer::semi)) {
    initExpr = ParseExpr();
  }
  assert(Match(lexer::semi));

  if (!Peek(lexer::semi)) {
    controlExpr = ParseExpr();
  }
  assert(Match(lexer::semi));

  if (!Peek(lexer::r_paren)) {
    postExpr = ParseExpr();
  }
  assert(Match(lexer::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<ForStmt>(std::move(initExpr),
                                   std::move(controlExpr),
                                   std::move(postExpr), std::move(stmt));
}

std::unique_ptr<ForDeclarationStmt> Parser::ParseForDeclStmt() {
  Consume(lexer::kw_for);
  assert(Match(lexer::l_paren));
  assert(IsTypeName());
  std::unique_ptr<Declaration> initDecl = ParseDeclStmt();
  std::unique_ptr<Expr> controlExpr = nullptr, postExpr = nullptr;
  if (!Peek(lexer::semi)) {
    controlExpr = ParseExpr();
  }
  assert(Match(lexer::semi));
  if (!Peek(lexer::r_paren)) {
    postExpr = ParseExpr();
  }
  assert(Match(lexer::r_paren));
  auto stmt = ParseStmt();
  return std::make_unique<ForDeclarationStmt>(std::move(initDecl),
                                   std::move(controlExpr),
                                   std::move(postExpr), std::move(stmt));
}

std::unique_ptr<Declaration> Parser::ParseDeclStmt() {
  assert(IsTypeName());
  auto ty = ParseType();
  Expect(lexer::identifier);
  std::string name = std::get<std::string>(mTokCursor->GetTokenValue());
  Consume(lexer::identifier);
  if (Peek(lexer::equal)) {
    Consume(lexer::equal);
    auto expr = ParseExpr();
    assert(Match(lexer::semi));
    return std::make_unique<Declaration>(std::move(ty), name, std::move(expr));
  }else {
    assert(Match(lexer::semi));
    return std::make_unique<Declaration>(std::move(ty), name);
  }
}

std::unique_ptr<BreakStmt> Parser::ParseBreakStmt() {
    assert(Match(lexer::kw_break));
    assert(Match(lexer::semi));
    return std::make_unique<BreakStmt>();
}

std::unique_ptr<ContinueStmt> Parser::ParseContinueStmt() {
  assert(Match(lexer::kw_continue));
  assert(Match(lexer::semi));
  return std::make_unique<ContinueStmt>();
}

std::unique_ptr<ReturnStmt> Parser::ParseReturnStmt() {
  assert(Match(lexer::kw_return));
  if (Peek(lexer::semi)) {
    return std::make_unique<ReturnStmt>();
  }

  auto expr = ParseExpr();
  assert(Match(lexer::semi));
  return std::make_unique<ReturnStmt>(std::move(expr));
}

std::unique_ptr<ExprStmt> Parser::ParseExprStmt() {
  if (Peek(lexer::semi)) {
    return std::make_unique<ExprStmt>();
  }
  auto expr = ParseExpr();
  assert(Match(lexer::semi));
  return std::make_unique<ExprStmt>(std::move(expr));
}

std::unique_ptr<Expr> Parser::ParseExpr() {
  auto assign = ParseAssignExpr();
  std::vector<std::unique_ptr<AssignExpr>> assignExps;
  while (Peek(lexer::comma)) {
    Consume(lexer::comma);
    assignExps.push_back(ParseAssignExpr());
  }
  return std::make_unique<Expr>(std::move(assign), std::move(assignExps));
}

std::unique_ptr<AssignExpr> Parser::ParseAssignExpr() {
  auto conditionExpr = ParseConditionalExpr();
  lexer::TokenType tokenType = lexer::unknown;
  switch (mTokCursor->GetTokenType()) {
  case lexer::equal:
  case lexer::plus_equal:
  case lexer::star_equal:
  case lexer::minus_equal:
  case lexer::slash_equal:
  case lexer::percent_equal:
  case lexer::less_less_equal:
  case lexer::greater_greater_equal:
  case lexer::pipe_equal:
  case lexer::amp_equal:
  case lexer::caret_equal: {
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
  if (Peek(lexer::question)) {
    Consume(lexer::question);
    auto expr = ParseExpr();
    assert(Match(lexer::colon));
    auto conditionalExpr = ParseConditionalExpr();
    return std::make_unique<ConditionalExpr>(std::move(logOrExpr), std::move(expr), std::move(conditionalExpr));
  }else {
    return std::make_unique<ConditionalExpr>(std::move(logOrExpr));
  }
}

std::unique_ptr<LogOrExpr> Parser::ParseLogOrExpr() {
  auto expr = ParseLogAndExpr();
  std::vector<std::unique_ptr<LogAndExpr>> logAndExprArr;
  while (Peek(lexer::pipe_pipe)) {
    Consume(lexer::pipe_pipe);
    logAndExprArr.push_back(ParseLogAndExpr());
  }
  return std::make_unique<LogOrExpr>(std::move(expr), std::move(logAndExprArr));
}

std::unique_ptr<LogAndExpr> Parser::ParseLogAndExpr() {
  auto expr = ParseBitOrExpr();
  std::vector<std::unique_ptr<BitOrExpr>> bitOrExprArr;
  while (Peek(lexer::amp_amp)) {
    Consume(lexer::amp_amp);
    bitOrExprArr.push_back(ParseBitOrExpr());
  }
  return std::make_unique<LogAndExpr>(std::move(expr), std::move(bitOrExprArr));
}

std::unique_ptr<BitOrExpr> Parser::ParseBitOrExpr() {
  auto expr = ParseBitXorExpr();
  std::vector<std::unique_ptr<BitXorExpr>> bitXorExprArr;
  while (Peek(lexer::pipe)) {
    Consume(lexer::pipe);
    bitXorExprArr.push_back(ParseBitXorExpr());
  }
  return std::make_unique<BitOrExpr>(std::move(expr), std::move(bitXorExprArr));
}

std::unique_ptr<BitXorExpr> Parser::ParseBitXorExpr() {
  auto expr = ParseBitAndExpr();
  std::vector<std::unique_ptr<BitAndExpr>> bitAndExprArr;
  while (Peek(lexer::caret)) {
    Consume(lexer::caret);
    bitAndExprArr.push_back(ParseBitAndExpr());
  }
  return std::make_unique<BitXorExpr>(std::move(expr), std::move(bitAndExprArr));
}

std::unique_ptr<BitAndExpr> Parser::ParseBitAndExpr() {
  auto expr = ParseEqualExpr();
  std::vector<std::unique_ptr<EqualExpr>> equalExprArr;
  while (Peek(lexer::amp)) {
    Consume(lexer::amp);
    equalExprArr.push_back(ParseEqualExpr());
  }
  return std::make_unique<BitAndExpr>(std::move(expr), std::move(equalExprArr));
}

std::unique_ptr<EqualExpr> Parser::ParseEqualExpr() {
  auto expr = ParseRelationalExpr();
  std::vector<std::pair<lexer::TokenType, std::unique_ptr<RelationalExpr>>> relationalExprArr;
  while(mTokCursor->GetTokenType() == lexer::equal_equal
         ||mTokCursor->GetTokenType() == lexer::exclaim_equal) {
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    relationalExprArr.push_back({tokenType, ParseRelationalExpr()});
  }
  return std::make_unique<EqualExpr>(std::move(expr), std::move(relationalExprArr));
}

std::unique_ptr<RelationalExpr>  Parser::ParseRelationalExpr() {
  auto expr = ParseShiftExpr();
  std::vector<std::pair<lexer::TokenType, std::unique_ptr<ShiftExpr>>> relationalExprArr;
  while (mTokCursor->GetTokenType() == lexer::less || mTokCursor->GetTokenType() == lexer::less_equal
         || mTokCursor->GetTokenType() == lexer::greater || mTokCursor->GetTokenType() == lexer::greater_equal) {
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    relationalExprArr.push_back({tokenType, ParseShiftExpr()});
  }
  return std::make_unique<RelationalExpr>(std::move(expr), std::move(relationalExprArr));
}

std::unique_ptr<ShiftExpr> Parser::ParseShiftExpr() {
  auto expr = ParseAdditiveExpr();
  std::vector<std::pair<lexer::TokenType, std::unique_ptr<AdditiveExpr>>> additiveExprArr;
  while (mTokCursor->GetTokenType() == lexer::less_less || mTokCursor->GetTokenType() == lexer::greater_greater) {
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    additiveExprArr.push_back({tokenType, ParseAdditiveExpr()});
  }
  return std::make_unique<ShiftExpr>(std::move(expr), std::move(additiveExprArr));
}

std::unique_ptr<AdditiveExpr> Parser::ParseAdditiveExpr() {
  auto expr = ParseMultiExpr();
  std::vector<std::pair<lexer::TokenType, std::unique_ptr<MultiExpr>>> multiExprArr;
  while (mTokCursor->GetTokenType() == lexer::plus || mTokCursor->GetTokenType() == lexer::minus) {
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    multiExprArr.push_back({tokenType, ParseMultiExpr()});
  }
  return std::make_unique<AdditiveExpr>(std::move(expr), std::move(multiExprArr));
}

std::unique_ptr<MultiExpr> Parser::ParseMultiExpr() {
  auto expr = ParseCastExpr();
  std::vector<std::pair<lexer::TokenType, std::unique_ptr<CastExpr>>> castExprArr;
  while (mTokCursor->GetTokenType() == lexer::star || mTokCursor->GetTokenType() == lexer::slash || mTokCursor->GetTokenType() == lexer::percent) {
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    castExprArr.push_back({tokenType, ParseCastExpr()});
  }
  return std::make_unique<MultiExpr>(std::move(expr), std::move(castExprArr));
}

std::unique_ptr<CastExpr> Parser::ParseCastExpr() {
  TokIter start = mTokCursor;
  if (Peek(lexer::l_paren)) {
    ConsumeAny();
    if(IsTypeName()) {
      auto ty = ParseType();
      assert(Match(lexer::r_paren));
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
    lexer::TokenType tokenType = mTokCursor->GetTokenType();
    ConsumeAny();
    auto expr = std::make_unique<UnaryExprUnaryOperator>(tokenType, ParseUnaryExpr());
    return std::make_unique<UnaryExpr>(std::move(expr));
  }else if (Peek(lexer::kw_sizeof)) {
    Consume(lexer::kw_sizeof);
    if (Peek(lexer::l_paren)) {
      Consume(lexer::l_paren);
      auto type = ParseType();
      Expect(lexer::r_paren);
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
    if (tokType == lexer::l_square) {
      assert(!stack.empty());
      Consume(lexer::l_square);
      auto expr = ParseExpr();
      assert(Match(lexer::r_square));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprSubscript>(std::move(postfixExpr), std::move(expr))));
    }else if (tokType == lexer::l_paren) {
      Consume(lexer::l_paren);
      std::vector<std::unique_ptr<AssignExpr>> params;
      if (!Peek(lexer::r_paren)) {
        params.push_back(ParseAssignExpr());
      }
      while (mTokCursor->GetTokenType() != lexer::r_paren) {
        assert(Match(lexer::comma));
        params.push_back(ParseAssignExpr());
      }
      Consume(lexer::r_paren);
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprFuncCall>(std::move(postfixExpr), std::move(params))));
    }else if (tokType == lexer::period) {
      Consume(lexer::period);
      Expect(lexer::identifier);
      std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
      assert(Match(lexer::identifier));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprDot>(std::move(postfixExpr), identifier)));
    }else if (tokType == lexer::arrow) {
      Consume(lexer::arrow);
      Expect(lexer::identifier);
      std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
      assert(Match(lexer::identifier));
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprArrow>(std::move(postfixExpr), identifier)));
    }else if (tokType == lexer::plus_plus) {
      Consume(lexer::plus_plus);
      auto postfixExpr = std::move(stack.top());
      stack.pop();
      stack.push(std::make_unique<PostFixExpr>(std::make_unique<PostFixExprIncrement>(std::move(postfixExpr))));
    }else if (tokType == lexer::minus_minus) {
      Consume(lexer::minus_minus);
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
  if (Peek(lexer::identifier)) {
    std::string identifier = std::get<std::string>(mTokCursor->GetTokenValue());
    Consume(lexer::identifier);
    auto expr = std::make_unique<PrimaryExprIdentifier>(identifier);
    return std::make_unique<PrimaryExpr>(std::move(expr));
  }else if (Peek(lexer::char_constant) || Peek(lexer::numeric_constant) || Peek(lexer::string_literal)) {
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
    Expect(lexer::l_paren);
    Consume(lexer::l_paren);
    auto expr = ParseExpr();
    assert(Match(lexer::r_paren));
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
  assert(Match(lexer::identifier));

  bool isFunc = false;
  if (Match(lexer::l_paren)) {
    isFunc = true;
  }
  mTokCursor = start;
  return isFunc;
}

bool Parser::IsTypeName() {
  lexer::TokenType type = mTokCursor->GetTokenType();
  return type == lexer::kw_void | type == lexer::kw_auto |
         type == lexer::kw_char | type == lexer::kw_short |
         type == lexer::kw_int | type == lexer::kw_long |
         type == lexer::kw_float | type == lexer::kw_double |
         type == lexer::kw_signed | type == lexer::kw_unsigned |
         type == lexer::kw_const;
}

bool Parser::Match(lexer::TokenType tokenType) {
  if (mTokCursor->GetTokenType() == tokenType) {
    ++mTokCursor;
    return true;
  }
  return false;
}

bool Parser::Expect(lexer::TokenType tokenType) {
  if (mTokCursor->GetTokenType() == tokenType)
    return true;
  assert(0);
  return false;
}

bool Parser::Consume(lexer::TokenType tokenType) {
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
bool Parser::Peek(lexer::TokenType tokenType) {
  return mTokCursor->GetTokenType() == tokenType;
}

bool Parser::IsUnaryOp(lexer::TokenType tokenType) {
  if (tokenType == lexer::amp || tokenType == lexer::star || tokenType == lexer::plus ||
      tokenType == lexer::minus || tokenType == lexer::tilde || tokenType == lexer::exclaim
      || tokenType == lexer::plus_plus || tokenType == lexer::minus_minus) {
    return true;
  }
  return false;
}

bool Parser::IsPostFixExpr(lexer::TokenType tokenType) {
  return ( tokenType == lexer::l_paren || tokenType == lexer::l_square
      || tokenType == lexer::period || tokenType == lexer::arrow
      || tokenType == lexer::plus_plus || tokenType == lexer::minus_minus
      || tokenType == lexer::identifier || tokenType == lexer::char_constant
          || tokenType == lexer::numeric_constant);
}
} // namespace lcc::parser