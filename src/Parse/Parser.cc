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
#include <algorithm>
#include "Utilities.h"
#include <set>
#include <iostream>

namespace lcc {
using namespace Syntax;

Parser::Parser(const std::vector<Token> & tokens, DiagnosticEngine &diag)
    : mTokens(tokens), mTokCursor(mTokens.cbegin()),
      mTokEnd(mTokens.cend()), Diag(diag) {

  FirstDeclaration = FormTokenKinds(tok::kw_auto, tok::kw_extern, tok::kw_static,
     tok::kw_register, tok::kw_typedef, tok::kw_const, tok::kw_restrict,
     tok::kw_volatile, tok::kw_signed, tok::kw_unsigned, tok::kw_short,
     tok::kw_long, tok::kw_char, tok::kw_int, tok::kw__Bool, tok::kw_float,
     tok::kw_double, tok::kw_enum, tok::kw_struct, tok::kw_union, tok::kw_void,
     tok::kw_inline, tok::identifier);

  FirstExpression = FormTokenKinds(tok::identifier, tok::char_constant,
     tok::string_literal, tok::numeric_constant, tok::l_paren, tok::plus_plus,
     tok::minus_minus, tok::kw_sizeof, tok::amp, tok::star, tok::plus,
     tok::minus, tok::tilde, tok::exclaim);

  FirstStatement = FormTokenKinds(tok::l_brace, tok::kw_if, tok::kw_while,
     tok::kw_do, tok::kw_for, tok::kw_break, tok::kw_continue, tok::kw_goto,
     tok::identifier, tok::kw_switch, tok::kw_return, tok::kw_case,
     tok::kw_default, tok::semi) | FirstExpression;

  FirstStructDeclaration = FirstDeclaration | FormTokenKinds(tok::semi);
  FirstExternalDeclaration = FirstDeclaration | FormTokenKinds(tok::semi);
}

TranslationUnit Parser::ParseTranslationUnit() {
  std::vector<ExternalDeclaration> decls;
  auto begin = mTokCursor;
  while (mTokCursor != mTokEnd) {
    /// ; is a external declaration
    if (Peek(tok::semi)) {
      ConsumeAny();
      continue;
    }
    auto result = ParseExternalDeclaration();
    if (result) {
      decls.push_back(std::move(*result));
    }
    SkipTo(FirstExternalDeclaration, diag::err_parse_skip_to_first_external_declaration);
  }
  return TranslationUnit(begin, decls);
}

DeclSpec Parser::ParseDeclarationSpecifiers() {
  auto begin = mTokCursor;
  DeclSpec decSpec(begin);
  bool seeTy = false;
next_specifier:
  switch (mTokCursor->getTokenKind()) {
  case tok::kw_auto: {
    decSpec.addStorageClassSpecifiers(
        StorageClsSpec(mTokCursor, StorageClsSpec::Auto));
    ConsumeAny();
    break;
  }
  case tok::kw_register: {
    decSpec.addStorageClassSpecifiers(
        StorageClsSpec(mTokCursor, StorageClsSpec::Register));
    ConsumeAny();
    break;
  }
  case tok::kw_static: {
    decSpec.addStorageClassSpecifiers(
        StorageClsSpec(mTokCursor, StorageClsSpec::Static));
    ConsumeAny();
    break;
  }
  case tok::kw_extern: {
    decSpec.addStorageClassSpecifiers(
        StorageClsSpec(mTokCursor, StorageClsSpec::Extern));
    ConsumeAny();
    break;
  }
  case tok::kw_typedef: {
    decSpec.addStorageClassSpecifiers(
        StorageClsSpec(mTokCursor, StorageClsSpec::Typedef));
    ConsumeAny();
    break;
  }
  case tok::kw_volatile: {
    decSpec.addTypeQualifiers(
        TypeQualifier(mTokCursor, TypeQualifier::Volatile));
    ConsumeAny();
    break;
  }
  case tok::kw_const: {
    decSpec.addTypeQualifiers(TypeQualifier(mTokCursor, TypeQualifier::Const));
    ConsumeAny();
    break;
  }
  case tok::kw_restrict: {
    decSpec.addTypeQualifiers(
        TypeQualifier(mTokCursor, TypeQualifier::Restrict));
    ConsumeAny();
    break;
  }
  case tok::kw_void: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Void));
    ConsumeAny();
    break;
  }
  case tok::kw_char: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Char));
    ConsumeAny();
    break;
  }
  case tok::kw_short: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Short));
    ConsumeAny();
    break;
  }
  case tok::kw_int: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Int));
    ConsumeAny();
    break;
  }
  case tok::kw_long: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Long));
    ConsumeAny();
    break;
  }
  case tok::kw_float: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Float));
    ConsumeAny();
    break;
  }
  case tok::kw_double: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Double));
    ConsumeAny();
    break;
  }
  case tok::kw_signed: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Signed));
    ConsumeAny();
    break;
  }
  case tok::kw_unsigned: {
    seeTy = true;
    decSpec.addTypeSpec(TypeSpec(mTokCursor, TypeSpec::Unsigned));
    ConsumeAny();
    break;
  }
  case tok::kw_union:
  case tok::kw_struct: {
    auto expected = ParseStructOrUnionSpecifier();
    if (expected) {
      decSpec.addTypeSpec(TypeSpec(mTokCursor, *expected));
    }
    seeTy = true;
    break;
  }
  case tok::kw_enum: {
    auto expected = ParseEnumSpecifier();
    if (expected) {
      decSpec.addTypeSpec(TypeSpec(mTokCursor, *expected));
    }
    seeTy = true;
    break;
  }
  case tok::identifier: {
    auto name = mTokCursor->getRepresentation();
    if (!seeTy && mScope.isTypedefInScope(name)) {
      ConsumeAny();
      decSpec.addTypeSpec(TypeSpec(mTokCursor, name));
      seeTy = true;
      break;
    }
    return decSpec;
  }
  default:
    return decSpec;
  }
  goto next_specifier;
}

std::optional<Declaration> Parser::ParseDeclarationSuffix(
    DeclSpec declSpec, std::optional<Declarator> alreadyParsedDeclarator) {
  bool hasTypedef =
      std::any_of(declSpec.getStorageClassSpecifiers().begin(),
                  declSpec.getStorageClassSpecifiers().end(),
                  [](const StorageClsSpec &storage) {
                    return storage.getSpecifier() == StorageClsSpec::Typedef;
                  });
  std::vector<Declaration::InitDeclarator> initDeclarators;
  if (alreadyParsedDeclarator) {
    if (!hasTypedef) {
      auto name = getDeclaratorName(*alreadyParsedDeclarator);
      mScope.addToScope(name);
    }
    if (!Peek(tok::equal)) {
      initDeclarators.push_back({(*alreadyParsedDeclarator).getBeginLoc(),
                                 *alreadyParsedDeclarator, std::nullopt});
    }else {
      Expect(tok::equal);
      auto initializer = ParseInitializer();
      if (initializer) {
        initDeclarators.push_back({(*alreadyParsedDeclarator).getBeginLoc(),
                                   *alreadyParsedDeclarator, *initializer});
      }
    }
  }

  bool first = true;
  if (Peek(tok::semi))
    goto End;

  if(Peek(tok::comma)) {
    ConsumeAny();
  }

  do{
    if (first) {
      first = false;
    }else {
      Expect(tok::comma);
    }
    /// handle first declarator
    auto begin = mTokCursor;
    auto declarator = ParseDeclarator();
    if (!hasTypedef && declarator) {
      auto name = getDeclaratorName(*declarator);
      mScope.addToScope(name);
    }
    if (!Peek(tok::equal) && declarator) {
      initDeclarators.push_back({begin, *declarator, std::nullopt});
    } else {
      Expect(tok::equal);
      auto initializer = ParseInitializer();
      if (initializer && declarator) {
        initDeclarators.push_back({begin, *declarator, *initializer});
      }
    }
  }while (Peek(tok::comma));

End:
  Expect(tok::semi);
  if (hasTypedef) {
    for (auto& iter : initDeclarators) {
      auto name = getDeclaratorName(*iter.declarator_);
      mScope.addTypedef(name);
    }
  }
  return Declaration(declSpec.getBeginLoc(), declSpec, initDeclarators);
}

std::optional<ExternalDeclaration> Parser::ParseExternalDeclaration() {
  auto begin = mTokCursor;
  auto declSpecs = ParseDeclarationSpecifiers();
  if (declSpecs.isEmpty()) {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Declaration(begin, declSpecs, {});
  }
  auto declarator = ParseDeclarator();
  const DirectDeclaratorParamTypeList *parameters = nullptr;
  if (!declarator) {
    goto end;
  }
  parameters = getFuncDeclarator(*declarator);
  if (!parameters) {
    goto end;
  }
  if (declSpecs.getStorageClassSpecifiers().size() > 0 &&
      std::any_of(declSpecs.getStorageClassSpecifiers().begin(),
                  declSpecs.getStorageClassSpecifiers().end(),
                  [](const StorageClsSpec &specifier) {
                    return specifier.getSpecifier() == StorageClsSpec::Typedef;
                  })) {
    goto end;
  }

  if (Peek(tok::semi) && PeekN(1, tok::l_brace)) {
    DiagReport(Diag, mTokCursor->getSMLoc(),
               diag::err_parse_accidently_add_semi);
    goto end;
  }
  if (!Peek(tok::l_brace)) {
    goto end;
  }
  {
    /// function define
    /// func param and block stmt share a scope
    mScope.pushScope();
    auto &parameterDeclarations = parameters->getParamTypeList()
                                      .getParameterList()
                                      .getParameterDeclarations();
    for (auto &iter : parameterDeclarations) {
      /// check is void
      /// [declSpecifiers, parameterDeclarator]
      auto &declSpecifiers = iter.declarationSpecifiers_;
      auto &parameterDeclarator = iter.declaratorKind_;
      auto isOnlyOneTypeSpecifier =
          [&parameterDeclarations](const DeclSpec &declSpec) {
            return declSpec.getStorageClassSpecifiers().size() == 0 &&
                   declSpec.getTypeQualifiers().size() == 0 &&
                   declSpec.getFunctionSpecifier().size() == 0 &&
                   declSpec.getTypeSpecs().size() == 1 &&
                   (parameterDeclarations.size() == 1);
          };
      if (isOnlyOneTypeSpecifier(declSpecifiers)) {
        auto *primitive = std::get_if<TypeSpec::PrimTypeKind>(
            &declSpecifiers.getTypeSpecs()[0].getVariant());
        if (primitive && *primitive == TypeSpec::PrimTypeKind::Void) {
          break;
        }
      }
      if (std::holds_alternative<std::optional<AbstractDeclarator>>(
              parameterDeclarator)) {
        DiagReport(Diag, declSpecifiers.getBeginLoc()->getSMLoc(), diag::err_parse_func_param_declaration_miss_name);
        continue;
      }
      auto &decl = std::get<Declarator>(parameterDeclarator);
      mScope.addToScope(getDeclaratorName(decl));
    }
    auto compoundStmt = ParseBlockStmt();
    mScope.popScope();
    mScope.addToScope(getDeclaratorName(*declarator));
    if (compoundStmt) {
      return FunctionDefinition(begin, declSpecs, *declarator, *compoundStmt);
    }
    return std::nullopt;
  }
end:
  /// is global declaration
  return ParseDeclarationSuffix(declSpecs, declarator);
}

/// declaration: declaration-specifiers init-declarator-list{opt} ;
std::optional<Declaration> Parser::ParseDeclaration() {
  auto begin = mTokCursor;
  auto declSpecs = ParseDeclarationSpecifiers();
  if (declSpecs.isEmpty()) {
    DiagReport(Diag, mTokCursor->getSMLoc(),
               diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Declaration(begin, declSpecs, {});
  }
  return ParseDeclarationSuffix(declSpecs);
}

std::optional<StructOrUnionSpec> Parser::ParseStructOrUnionSpecifier() {
  auto begin = mTokCursor;
  bool isUnion = false;
  if (Peek(tok::kw_union)) {
    isUnion = true;
  }
  ConsumeAny();
  std::string_view tagName;
  auto start = mTokCursor;
  switch (mTokCursor->getTokenKind()) {
  case tok::identifier: {
    tagName = mTokCursor->getRepresentation();
    ConsumeAny();
    if (Peek(tok::l_brace)) {
      goto lbrace;
    }
    return StructOrUnionSpec(begin, isUnion, tagName, {});
  }
  case tok::l_brace: {
  lbrace:
    ConsumeAny();
    mScope.pushScope();
    std::vector<StructOrUnionSpec::StructDeclaration> structDeclarations;
    while (IsCurrentIn(FirstStructDeclaration)) {
      auto decl = ParseStructDeclaration();
      if (decl) {
        structDeclarations.push_back(std::move(*decl));
      }
      SkipTo(FirstStructDeclaration | FormTokenKinds(tok::semi, tok::r_brace),
             diag::err_parse_skip_to_first_struct_declaration);
    }
    mScope.popScope();
    Expect(tok::r_brace);
    return StructOrUnionSpec(begin, isUnion, tagName, structDeclarations);
  }
  default:
    DiagReport(Diag, start->getSMLoc(), diag::err_parse_expect_n, "identifier or { after struct/union");
    return std::nullopt;
  }
}

std::optional<StructOrUnionSpec::StructDeclaration>
Parser::ParseStructDeclaration() {
  TokIter begin = mTokCursor;

  // to support struct {;},	empty struct/union declaration
  if (Peek(tok::semi)) {
    ConsumeAny();
    return std::nullopt;
  }

  auto specs = ParseDeclarationSpecifiers();
  if (specs.getStorageClassSpecifiers().size() > 0) {
    DiagReport(Diag, begin->getSMLoc(),
               diag::err_parse_struct_declaration_appear_storage_class);
  }
  if (specs.getTypeSpecs().size() == 0 &&
      specs.getTypeQualifiers().size() == 0) {
    DiagReport(Diag, begin->getSMLoc(),
               diag::err_parse_expect_type_specifier_or_qualifier);
  }
  std::vector<StructOrUnionSpec::StructDeclarator> declarators;
  auto stDec = ParseStructDeclarator();
  if (stDec) {
    declarators.push_back(*stDec);
  }
  while (Peek(tok::comma)) {
    ConsumeAny();
    auto stDec2 = ParseStructDeclarator();
    if (stDec2) {
      declarators.push_back(*stDec2);
    }
  }
  Expect(tok::semi);
  return StructOrUnionSpec::StructDeclaration{begin, specs, declarators};
}

std::optional<StructOrUnionSpec::StructDeclarator>
Parser::ParseStructDeclarator() {
  auto begin = mTokCursor;
  SetCheckTypedefType(false);
  auto declarator = ParseDeclarator();
  SetCheckTypedefType(true);
  if (declarator)
    mScope.addToScope(getDeclaratorName(*declarator));
  if (Peek(tok::colon) && declarator) {
    ConsumeAny();
    auto constant = ParseConditionalExpr();
    return StructOrUnionSpec::StructDeclarator{begin, *declarator, *constant};
  } else if (declarator){
    return StructOrUnionSpec::StructDeclarator{begin, *declarator,
                                               std::nullopt};
  }else {
    return std::nullopt;
  }
}

/// declarator: pointer{opt} direct-declarator
std::optional<Declarator> Parser::ParseDeclarator() {
  std::vector<Pointer> pointers;
  auto begin = mTokCursor;
  while (Peek(tok::star)) {
    pointers.push_back(ParsePointer());
  }
  auto directDeclarator = ParseDirectDeclarator();
  if (!directDeclarator) {
    return std::nullopt;
  }
  return Declarator(begin, pointers, *directDeclarator);
}

/**
direct-declarator:
    identifier
    ( declarator )
    direct-declarator [ type-qualifier-list{opt} assignment-expression{opt} ]
    direct-declarator [ static type-qualifier-list{opt} assignment-expression]
    direct-declarator [type-qualifier-list static assignment-expression]
    direct-declarator [type-qualifier-list{opt} *]
    direct-declarator ( parameter-type-list )
    direct-declarator ( identifier-list{opt} )
 */
void Parser::ParseDirectDeclaratorSuffix(TokIter beginTokLoc, DirectDeclarator &directDeclarator) {
  while (Peek(tok::l_paren) || Peek(tok::l_square)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::l_paren: {
      ConsumeAny();
      if (IsFirstInDeclarationSpecifier()) {
        SetCheckTypedefType(false);
        auto parameterTypeList = ParseParameterTypeList();
        SetCheckTypedefType(true);
        if (parameterTypeList) {
          directDeclarator = DirectDeclaratorParamTypeList(
              beginTokLoc, directDeclarator, *parameterTypeList);
        }
      }else {
        directDeclarator = DirectDeclaratorParamTypeList(
            beginTokLoc, directDeclarator,
            ParamTypeList(beginTokLoc, ParamList(beginTokLoc, {}), false));
      }
      Expect(tok::r_paren);
      break;
    }
    case tok::l_square: {
      ConsumeAny();
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        std::vector<TypeQualifier> typeQualifiers;
        while (Peek(tok::kw_const) || Peek(tok::kw_volatile)
               || Peek(tok::kw_restrict)) {
          switch (mTokCursor->getTokenKind()) {
          case tok::kw_const: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Const));
            break;
          }
          case tok::kw_volatile: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Volatile));
            break;
          }
          case tok::kw_restrict: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Restrict));
            break;
          }
          default:
            break;
          }
          ConsumeAny();
        }
        auto assignment = ParseAssignExpr();
        if (assignment) {
          directDeclarator = DirectDeclaratorAssignExpr(
              beginTokLoc, directDeclarator, typeQualifiers, *assignment, true);
        }
        Expect(tok::r_square);
        break;
      }

      std::vector<TypeQualifier> typeQualifiers;
      while (Peek(tok::kw_const) || Peek(tok::kw_volatile)
             || Peek(tok::kw_restrict)) {
        switch (mTokCursor->getTokenKind()) {
        case tok::kw_const: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Const));
          break;
        }
        case tok::kw_volatile: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Volatile));
          break;
        }
        case tok::kw_restrict: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Restrict));
          break;
        }
        default:
          break;
        }
        ConsumeAny();
      }
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        auto assignment = ParseAssignExpr();
        if (assignment) {
          directDeclarator = DirectDeclaratorAssignExpr(
              beginTokLoc, directDeclarator, typeQualifiers, *assignment, true);
        }
        Expect(tok::r_square);
      }else if (Peek(tok::star)) {
        ConsumeAny();
        directDeclarator =
            DirectDeclaratorAsterisk(beginTokLoc, directDeclarator, typeQualifiers);
        Expect(tok::r_square);
      }else if (IsFirstInAssignmentExpr()) {
        auto assignment = ParseAssignExpr();
        if (assignment) {
          directDeclarator = DirectDeclaratorAssignExpr(
              beginTokLoc, directDeclarator, typeQualifiers, *assignment, false);
        }
        Expect(tok::r_square);
      }else {
        directDeclarator = DirectDeclaratorAssignExpr(
            beginTokLoc, directDeclarator, typeQualifiers, std::nullopt, false);
        Expect(tok::r_square);
      }
      break;
    }
    default:
      break;
    }
  }
}

/**
direct-declarator:
    identifier
    ( declarator )
    direct-declarator [ type-qualifier-list{opt} assignment-expression{opt} ]
    direct-declarator [ static type-qualifier-list{opt} assignment-expression]
    direct-declarator [type-qualifier-list static assignment-expression]
    direct-declarator [type-qualifier-list{opt} *]
    direct-declarator ( parameter-type-list )
    direct-declarator ( identifier-list{opt} )
 */
std::optional<DirectDeclarator> Parser::ParseDirectDeclarator() {
  std::optional<DirectDeclarator> directDeclarator{std::nullopt};
  auto begin = mTokCursor;
  if (Peek(tok::identifier)) {
    auto name = mTokCursor->getRepresentation();
    if (IsCheckTypedefType()) {
      if (mScope.checkIsTypedefInCurrentScope(name)) {
        DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_n, "identifier, but get a typedef type");
      }
    }
    ConsumeAny();
    directDeclarator = DirectDeclaratorIdent(begin, name);
  }else if (Peek(tok::l_paren)) {
    ConsumeAny();
    auto declarator = ParseDeclarator();
    if (declarator) {
      directDeclarator = DirectDeclaratorParentheses(begin, *declarator);
    }
    Expect(tok::r_paren);
  }else {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_n, "identifier or (");
    return std::nullopt;
  }

  ParseDirectDeclaratorSuffix(begin, *directDeclarator);
  return directDeclarator;
}

/*
parameter-type-list:
  parameter-list
  parameter-list , ...
 */
std::optional<ParamTypeList> Parser::ParseParameterTypeList() {
  auto begin = mTokCursor;
  auto parameterList = ParseParameterList();
  bool hasEllipse = false;
  if (Peek(tok::comma)) {
    ConsumeAny();
    Expect(tok::ellipsis);
    hasEllipse = true;
  }
  if (parameterList) {
    return ParamTypeList(begin, *parameterList, hasEllipse);
  }
  return std::nullopt;
}

/**
parameter-list:
    parameter-declaration
    parameter-list , parameter-declaration
 */
std::optional<ParamList> Parser::ParseParameterList() {
  std::vector<ParameterDeclaration> paramDecls;
  auto begin = mTokCursor;
  auto declaration = ParseParameterDeclaration();
  if (declaration) {
    paramDecls.push_back(*declaration);
  }
  /// fix parse tok::ellipsis
  while (Peek(tok::comma) && !PeekN(1, tok::ellipsis)) {
    ConsumeAny();
    auto decl = ParseParameterDeclaration();
    if (decl) {
      paramDecls.push_back(*decl);
    }
  }
  return ParamList(begin, paramDecls);
}
std::optional<ParameterDeclaration>
Parser::ParseParameterDeclarationSuffix(DeclSpec &declSpec) {
  auto begin = mTokCursor;
  auto peekIsDeclarator = [this]()->bool{
    /// consume pointer
    while (Peek(tok::star)) {
      ParsePointer();
    }
    if (!Peek(tok::identifier) && !Peek(tok::l_paren) && !Peek(tok::l_square)) {
      /// ε  mean abstract declarator
      return false;
    }
    /**
      first direct dec set
          tok::identifier, tok::l_paren
      first direct abstract dec set
          tok::l_paren, tok::l_square
     */
    if (Peek(tok::identifier)) {
      return true;
    }else if (Peek(tok::l_square)) {
      return false;
    }else {
      while (Peek(tok::l_paren)) {
        ConsumeAny();
        switch (mTokCursor->getTokenKind()) {
        case tok::identifier:
          return true;
        case tok::l_square:
          return false;
        default: {
          break;
        }
        }
      }
    }
    return false;
  };
  if (peekIsDeclarator()) {
    auto dec = ParseDeclarator();
    if (dec) {
      return ParameterDeclaration(begin, declSpec, *dec);
    }
  }else {
    auto absDec = ParseAbstractDeclarator();
    if (absDec) {
      return ParameterDeclaration(begin, declSpec, *absDec);
    }
  }
  return std::nullopt;
}
/**
parameter-declaration:
    declaration-specifiers declarator
    declaration-specifiers abstract-declarator{opt}

declarator:
    pointer{opt} direct-declarator

abstract-declarator:
    pointer
    pointer{opt} direct-abstract-declarator
*/
std::optional<ParameterDeclaration> Parser::ParseParameterDeclaration() {
  auto begin = mTokCursor;
  auto specs = ParseDeclarationSpecifiers();
  if (specs.isEmpty()) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  /// abstract-declarator{opt}
  if (Peek(tok::comma) || Peek(tok::r_paren)) {
    return ParameterDeclaration(begin, specs, std::nullopt);
  }

  return ParseParameterDeclarationSuffix(specs);
}

/**
 pointer:
    * type-qualifier-list{opt}
    * type-qualifier-list{opt} pointer
 */
Pointer Parser::ParsePointer() {
  auto begin = mTokCursor;
  Expect(tok::star);
  std::vector<TypeQualifier> typeQualifier;
  while (Peek(tok::kw_const) || Peek(tok::kw_restrict) ||
         Peek(tok::kw_volatile)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::kw_const:
      typeQualifier.push_back(TypeQualifier(mTokCursor, TypeQualifier::Const));
      break;
    case tok::kw_restrict:
      typeQualifier.push_back(
          TypeQualifier(mTokCursor, TypeQualifier::Restrict));
      break;
    case tok::kw_volatile:
      typeQualifier.push_back(
          TypeQualifier(mTokCursor, TypeQualifier::Volatile));
      break;
    default:
      break;
    }
    ConsumeAny();
  }
  return Pointer(begin, typeQualifier);
}

/**
 abstract-declarator:
   pointer
   pointer{opt} direct-abstract-declarator
 */
std::optional<AbstractDeclarator> Parser::ParseAbstractDeclarator() {
  std::vector<Pointer> pointers;
  auto begin = mTokCursor;
  while (Peek(tok::star)) {
    auto result = ParsePointer();
    pointers.push_back(std::move(result));
  }
  if (!pointers.empty() && !IsFirstInDirectAbstractDeclarator()) {
    return AbstractDeclarator(begin, pointers);
  }
  auto result = ParseDirectAbstractDeclarator();
  return AbstractDeclarator(begin, pointers, result);
}

/**
 * direct-abstract-declarator:
 *      ( abstract-declarator )
 *      direct-abstract-declarator{opt} [ type-qualifier-list{opt} assignment-expression{opt} ]
 *      direct-abstract-declarator{opt} [ static type-qualifier-list{opt} assignment-expression ]
 *      direct-abstract-declarator{opt} [ type-qualifier-list static assignment-expression ]
 *      direct-abstract-declarator{opt} [*]
 *      direct-abstract-declarator{opt} ( parameter-type-list{opt} )
 */
std::optional<DirectAbstractDeclarator>
Parser::ParseDirectAbstractDeclaratorSuffix() {
  std::optional<DirectAbstractDeclarator> directAbstractDeclarator{
      std::nullopt};
  auto begin = mTokCursor;
  while (Peek(tok::l_paren) || Peek(tok::l_square)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::l_paren: {
      ConsumeAny();
      /// direct-abstract-declarator{opt} ( parameter-type-list{opt} )
      if (IsFirstInParameterTypeList()) {
        auto parameterTypeList = ParseParameterTypeList();
        if (parameterTypeList) {
          directAbstractDeclarator = DirectAbstractDeclaratorParamTypeList(
              begin, directAbstractDeclarator, *parameterTypeList);
        }
      }
      /// abstract-declarator first set
      /// ( abstract-declarator )
      else if (IsFirstInAbstractDeclarator()) {
        auto abstractDeclarator = ParseAbstractDeclarator();
        if (abstractDeclarator) {
          directAbstractDeclarator =
              DirectAbstractDeclaratorParentheses(begin, *abstractDeclarator);
        }
      } else {
        /// direct-abstract-declarator{opt} (  )
        directAbstractDeclarator = DirectAbstractDeclaratorParamTypeList(
            begin, directAbstractDeclarator, std::nullopt);
      }
      Expect(tok::r_paren);
      break;
    }
    case tok::l_square: {
      ConsumeAny();
      /// direct-abstract-declarator{opt} [*]
      if (Peek(tok::star)) {
        ConsumeAny();
        directAbstractDeclarator =
            DirectAbstractDeclaratorAsterisk(begin, directAbstractDeclarator);
        Expect(tok::r_square);
        break;
      }

      /// direct-abstract-declarator{opt} [ static type-qualifier-list{opt} assignment-expression ]
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        std::vector<TypeQualifier> typeQualifiers;
        while (Peek(tok::kw_const) || Peek(tok::kw_volatile) ||
               Peek(tok::kw_restrict)) {
          switch (mTokCursor->getTokenKind()) {
          case tok::kw_const: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Const));
            break;
          }
          case tok::kw_volatile: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Volatile));
            break;
          }
          case tok::kw_restrict: {
            typeQualifiers.push_back(
                TypeQualifier(mTokCursor, TypeQualifier::Restrict));
            break;
          }
          default:
            break;
          }
          ConsumeAny();
        }
        auto assignExpr = ParseAssignExpr();
        if (assignExpr) {
          directAbstractDeclarator = DirectAbstractDeclaratorAssignExpr(
              begin, directAbstractDeclarator, typeQualifiers, *assignExpr,
              true);
        }
        Expect(tok::r_square);
        break;
      }

      std::vector<TypeQualifier> typeQualifiers;
      while (Peek(tok::kw_const) || Peek(tok::kw_volatile) ||
             Peek(tok::kw_restrict)) {
        switch (mTokCursor->getTokenKind()) {
        case tok::kw_const: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Const));
          break;
        }
        case tok::kw_volatile: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Volatile));
          break;
        }
        case tok::kw_restrict: {
          typeQualifiers.push_back(
              TypeQualifier(mTokCursor, TypeQualifier::Restrict));
          break;
        }
        default:
          break;
        }
        ConsumeAny();
      }
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        auto assignExpr = ParseAssignExpr();
        if (assignExpr) {
          directAbstractDeclarator = DirectAbstractDeclaratorAssignExpr(
              begin, directAbstractDeclarator, typeQualifiers, *assignExpr,
              true);
        }
        Expect(tok::r_square);
      } else {
        if (!Peek(tok::r_square)) {
          auto assignment = ParseAssignExpr();
          if (assignment) {
            directAbstractDeclarator = DirectAbstractDeclaratorAssignExpr(
                begin, directAbstractDeclarator, typeQualifiers, *assignment,
                false);
          }
        } else {
          directAbstractDeclarator = DirectAbstractDeclaratorAssignExpr(
              begin, directAbstractDeclarator, typeQualifiers, std::nullopt,
              false);
        }
        Expect(tok::r_square);
        break;
      }
    }
    default:
      break;
    }
  }
  return directAbstractDeclarator;
}

/**
 abstract-declarator:
   pointer
   pointer{opt} direct-abstract-declarator

 direct-abstract-declarator:
    ( abstract-declarator )
    direct-abstract-declarator{opt} [ type-qualifier-list{opt} assignment-expression{opt} ]
    direct-abstract-declarator{opt} [ static type-qualifier-list{opt} assignment-expression ]
    direct-abstract-declarator{opt} [ type-qualifier-list static assignment-expression ]
    direct-abstract-declarator{opt} [ * ]
    direct-abstract-declarator{opt} ( parameter-type-list{opt} )
 */
std::optional<DirectAbstractDeclarator>
Parser::ParseDirectAbstractDeclarator() {
  return ParseDirectAbstractDeclaratorSuffix();
}

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
std::optional<EnumSpecifier> Parser::ParseEnumSpecifier() {
  auto begin = mTokCursor;
  Expect(tok::kw_enum);
  std::vector<EnumSpecifier::Enumerator> enumerators;
  std::string_view tagName;
  if (Peek(tok::identifier)) {
    tagName = mTokCursor->getRepresentation();
    ConsumeAny();
    if (Peek(tok::l_brace)) {
      goto enumerator_list;
    }
  }else if (Peek(tok::l_brace)) {
  enumerator_list:
    ConsumeAny();
    if (Peek(tok::r_brace)) {
      DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "identifier before '}' token");
    }
    enumerators.push_back(*ParseEnumerator());
    while (Peek(tok::comma)) {
      ConsumeAny();
      if (Peek(tok::r_brace))
        break;
      enumerators.push_back(*ParseEnumerator());
    }
    Expect(tok::r_brace);
  }else {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "identifier or { after enum");
  }
  return EnumSpecifier(begin, tagName, enumerators);
}

std::optional<EnumSpecifier::Enumerator> Parser::ParseEnumerator() {
  auto begin = mTokCursor;
  std::string_view enumValueName = mTokCursor->getRepresentation();
  if (mScope.checkIsTypedefInCurrentScope(enumValueName)) {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n,
               "identifier, but get a typedef type");
  }
  mScope.addToScope(enumValueName);
  Expect(tok::identifier);
  if (Peek(tok::equal)) {
    ConsumeAny();
    auto constant = ParseConditionalExpr();
    return EnumSpecifier::Enumerator{begin, enumValueName, constant};
  }else {
    return EnumSpecifier::Enumerator{begin, enumValueName};
  }
}

std::optional<BlockStmt> Parser::ParseBlockStmt() {
  auto begin = mTokCursor;
  Expect(tok::l_brace);
  std::vector<BlockItem> items;
  mScope.pushScope();
  while (IsFirstInBlockItem()) {
    auto result = ParseBlockItem();
    if (result)
      items.push_back(std::move(*result));
    if (Peek(tok::r_brace))
      break;
    SkipTo(FirstStatement | FirstDeclaration, diag::err_parse_skip_to_first_statement_or_first_declaration);
  }
  mScope.popScope();
  Expect(tok::r_brace);
  return BlockStmt(begin, items);
}

std::optional<BlockItem> Parser::ParseBlockItem() {
  auto start = mTokCursor;
  if (IsFirstInDeclarationSpecifier()) {
    auto declaration = ParseDeclaration();
    if (declaration) {
      return BlockItem(*declaration);
    }
  }
  auto statement = ParseStmt();
  if (statement) {
    return BlockItem(*statement);
  }
  return std::nullopt;
}

/**
 initializer:
    assignment-expression
    { initializer-list }
    { initializer-list , }
 */
std::optional<Initializer> Parser::ParseInitializer() {
  auto begin = mTokCursor;
  if (!Peek(tok::l_brace)) {
    auto assignment = ParseAssignExpr();
    if (assignment) {
      return Initializer(begin, *assignment);
    }
  } else {
    Expect(tok::l_brace);
    auto initializerList = ParseInitializerList();
    if (Peek(tok::comma)) {
      ConsumeAny();
    }
    Expect(tok::r_brace);
    if (initializerList) {
      return Initializer(begin, *initializerList);
    }
  }
  return std::nullopt;
}

/**
initializer-list:
    designation{opt} initializer
    initializer-list , designation{opt} initializer

designation:
    designator-list =
designator-list:
    designator
    designator-list designator
designator:
    [ constant-expression ]
    . identifier
 */
std::optional<InitializerList> Parser::ParseInitializerList() {
  auto begin = mTokCursor;
  std::vector<InitializerList::InitializerPair> initializerPairs;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      Expect(tok::comma);
    }
    InitializerList::Designation designation;
    while (Peek(tok::l_square) || Peek(tok::period)) {
      if (Peek(tok::l_square)) {
        ConsumeAny();
        auto constant = ParseConditionalExpr();
        if (constant) {
          designation.emplace_back(std::move(*constant));
        }
        Expect(tok::r_square);
      } else if (Peek(tok::period)) {
        ConsumeAny();
        designation.emplace_back(mTokCursor->getRepresentation());
        Expect(tok::identifier);
      }
    }
    if (!designation.empty()) {
      if (Peek(tok::equal)) {
        ConsumeAny();
      }
    }
    auto initializer = ParseInitializer();
    if (initializer)
      initializerPairs.push_back({designation, *initializer});
  } while (Peek(tok::comma));
  return InitializerList{begin, initializerPairs};
}

std::optional<Stmt> Parser::ParseStmt() {
  if (Peek(tok::kw_if)) {
    return ParseIfStmt();
  } else if (Peek(tok::kw_do)) {
    return ParseDoWhileStmt();
  } else if (Peek(tok::kw_while)) {
    return ParseWhileStmt();
  } else if (Peek(tok::kw_for)) {
    return ParseForStmt();
  } else if (Peek(tok::kw_break)) {
    return ParseBreakStmt();
  } else if (Peek(tok::kw_continue)) {
    return ParseContinueStmt();
  } else if (Peek(tok::kw_return)) {
    return ParseReturnStmt();
  } else if (Peek(tok::l_brace)) {
    auto s = ParseBlockStmt();
    if (!s)
      return {};
    return Stmt(std::move(*s));
  } else if (Peek(tok::kw_switch)) {
    return ParseSwitchStmt();
  } else if (Peek(tok::kw_default)) {
    return ParseDefaultStmt();
  } else if (Peek(tok::kw_case)) {
    return ParseCaseStmt();
  } else if (Peek(tok::kw_goto)) {
    return ParseGotoStmt();
  } else {
    /// identifier : stmt
    auto begin = mTokCursor;
    if (Peek(tok::identifier)) {
      ConsumeAny();
      if (Peek(tok::colon)) {
        ConsumeAny();
        return Stmt(LabelStmt(begin, begin->getRepresentation()));
      }else {
        mTokCursor = begin;
        return ParseExprStmt();
      }
    }else {
      /// expr{opt};
      return ParseExprStmt();
    }
  }
}

/// if ( expression ) statement
/// if ( expression ) statement else statement
std::optional<Stmt> Parser::ParseIfStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_if);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto thenStmt = ParseStmt();
  if (Peek(tok::kw_else)) {
    ConsumeAny();
    auto elseStmt = ParseStmt();
    if (expr && thenStmt && elseStmt) {
      return Stmt{IfStmt(begin, *expr, *thenStmt, *elseStmt)};
    }
  } else {
    if (expr && thenStmt) {
      return Stmt{IfStmt(begin, *expr, *thenStmt)};
    }
  }
  return std::nullopt;
}

/// while ( expression ) statement
std::optional<Stmt> Parser::ParseWhileStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_while);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto stmt = ParseStmt();
  if (expr && stmt) {
    return Stmt{WhileStmt(begin, *expr, *stmt)};
  }
  return std::nullopt;
}

/// do statement while ( expression ) ;
std::optional<Stmt> Parser::ParseDoWhileStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_do);
  auto stmt = ParseStmt();
  Expect(tok::kw_while);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  Expect(tok::semi);
  if (stmt && expr) {
    return Stmt{DoWhileStmt(begin, *stmt, *expr)};
  }
  return std::nullopt;
}

/// for ( expression{opt} ; expression{opt} ; expression{opt} ) statement
/// for ( declaration expression{opt} ; expression{opt} ) statement
std::optional<Stmt> Parser::ParseForStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_for);
  Expect(tok::l_paren);
  auto blockItem = ParseBlockItem();
  if (!blockItem) return std::nullopt;
  std::optional<Expr> controlExpr{std::nullopt};
  if (std::holds_alternative<Declaration>(*blockItem) || !Peek(tok::semi)) {
    auto expr = ParseExpr();
    Expect(tok::semi);
    if (expr) {
      controlExpr = *expr;
    }
  } else {
    Expect(tok::semi);
  }

  std::optional<Expr> postExpr{std::nullopt};
  if (Peek(tok::r_paren)) {
    ConsumeAny();
  } else {
    auto expr = ParseExpr();
    Expect(tok::r_paren);
    if (expr) {
      postExpr = *expr;
    }
  }

  auto stmt = ParseStmt();
  if (std::holds_alternative<Declaration>(*blockItem)) {
    return Stmt(ForStmt(begin, *stmt, std::get<Declaration>(*blockItem),
                        controlExpr, postExpr));
  } else if (std::holds_alternative<box<ExprStmt>>(
                 std::get<Stmt>(*blockItem))) {
    const auto &exprStmtBox =
        std::get<box<ExprStmt>>(std::get<Stmt>(*blockItem));
    std::optional<Expr> expr{std::nullopt};
    if (exprStmtBox->getOptionalExpression()) {
      expr = *exprStmtBox->getOptionalExpression();
    }
    return Stmt(ForStmt(begin, *stmt, expr, controlExpr, postExpr));
  }

  return std::nullopt;
}

/// break;
std::optional<Stmt> Parser::ParseBreakStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_break);
  Expect(tok::semi);
  return Stmt{BreakStmt(begin)};
}

/// continue;
std::optional<Stmt> Parser::ParseContinueStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_continue);
  Expect(tok::semi);
  return Stmt{ContinueStmt(begin)};
}

/// return expr{opt};
std::optional<Stmt> Parser::ParseReturnStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_return);
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Stmt{ReturnStmt(begin)};
  }
  auto expr = ParseExpr();
  Expect(tok::semi);
  if (!expr)
    return std::nullopt;
  return Stmt{ReturnStmt(begin, *expr)};
}

/// expr;
std::optional<Stmt> Parser::ParseExprStmt() {
  auto begin = mTokCursor;
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Stmt(ExprStmt(begin));
  }
  auto expr = ParseExpr();
  Expect(tok::semi);
  if (!expr)
    return std::nullopt;

  return Stmt(ExprStmt(begin, *expr));
}

/// switch ( expression ) statement
std::optional<Stmt> Parser::ParseSwitchStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_switch);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto stmt = ParseStmt();
  if (expr || stmt) {
    return std::nullopt;
  }
  return Stmt(SwitchStmt(begin, *expr, *stmt));
}

/// case constantExpr: stmt
std::optional<Stmt> Parser::ParseCaseStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_case);
  auto expr = ParseConditionalExpr();
  Expect(tok::colon);
  auto stmt = ParseStmt();
  if (expr || stmt)
    return std::nullopt;

  return Stmt(CaseStmt(begin, *expr, *stmt));
}

/// default: stmt
std::optional<Stmt> Parser::ParseDefaultStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_default);
  Expect(tok::colon);
  auto stmt = ParseStmt();
  if (!stmt)
    return std::nullopt;

  return Stmt(DefaultStmt(begin, *stmt));
}

/// goto identifier;
std::optional<Stmt> Parser::ParseGotoStmt() {
  auto begin = mTokCursor;
  Expect(tok::kw_goto);
  auto name = mTokCursor->getRepresentation();
  Expect(tok::identifier);
  Expect(tok::semi);
  return Stmt(GotoStmt(begin, name));
}

/**
 expression:
    assignment-expression
    expression , assignment-expression
 */
std::optional<Expr> Parser::ParseExpr() {
  std::vector<AssignExpr> assignExprs;
  auto begin = mTokCursor;

  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto assignExpr = ParseAssignExpr();
    if (assignExpr) {
      assignExprs.push_back(*assignExpr);
    }
  } while (Peek(tok::comma));

  return Expr(begin, assignExprs);
}

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
std::optional<AssignExpr> Parser::ParseAssignExpr() {
  auto begin = mTokCursor;
  auto firstCondExpr = ParseConditionalExpr();
  if (!firstCondExpr) {
    return std::nullopt;
  }
  std::vector<std::pair<AssignExpr::AssignOp, CondExpr>> list;
  while (IsAssignOp(mTokCursor->getTokenKind())) {
    auto token = mTokCursor;
    ConsumeAny();
    auto assignOp = [token]() -> AssignExpr::AssignOp {
      switch (token->getTokenKind()){
      case tok::equal:
        return AssignExpr::AssignOp::Assign;
      case tok::plus_equal:
        return AssignExpr::AssignOp::PlusAssign;
      case tok::minus_equal:
        return AssignExpr::AssignOp::MinusAssign;
      case tok::slash_equal:
        return AssignExpr::AssignOp::DivideAssign;
      case tok::star_equal:
        return AssignExpr::AssignOp::MultiplyAssign;
      case tok::percent_equal:
        return AssignExpr::AssignOp::ModuloAssign;
      case tok::less_less_equal:
        return AssignExpr::AssignOp::LeftShiftAssign;
      case tok::greater_greater_equal:
        return AssignExpr::AssignOp::RightShiftAssign;
      case tok::amp_equal:
        return AssignExpr::AssignOp::BitAndAssign;
      case tok::pipe_equal:
        return AssignExpr::AssignOp::BitOrAssign;
      case tok::caret_equal:
        return AssignExpr::AssignOp::BitXorAssign;
      default:
        return AssignExpr::AssignOp::Assign;
      }
    }();
    auto condExpr = ParseConditionalExpr();
    if (condExpr)
      list.push_back({assignOp, *condExpr});
  }
  return AssignExpr(begin, *firstCondExpr, list);
}

/**
 * conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 */
std::optional<CondExpr> Parser::ParseConditionalExpr() {
  auto begin = mTokCursor;
  auto logOrExpr = ParseLogOrExpr();
  if (!logOrExpr)
    return std::nullopt;

  if (Peek(tok::question)) {
    ConsumeAny();
    auto expr = ParseExpr();
    Expect(tok::colon);
    auto condExpr = ParseConditionalExpr();
    if (logOrExpr && expr && condExpr) {
      return CondExpr(begin, *logOrExpr, *expr, *condExpr);
    }
    return std::nullopt;
  }
  return CondExpr(begin, *logOrExpr);
}

/**
 * logical-OR-expression:
 *      logical-AND-expression
 *      logical-OR-expression || logical-AND-expression
 */
std::optional<LogOrExpr> Parser::ParseLogOrExpr() {
  std::vector<LogAndExpr> logAndExprArr;
  auto begin = mTokCursor;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto logAndExpr = ParseLogAndExpr();
    if (logAndExpr)
      logAndExprArr.push_back(*logAndExpr);
  } while (Peek(tok::pipe_pipe));

  if (logAndExprArr.empty()) {
    return std::nullopt;
  }

  return LogOrExpr(begin, logAndExprArr);
}

/**
 * logical-AND-expression:
 *      inclusive-OR-expression
 *      logical-AND-expression && inclusive-OR-expression
 */
std::optional<LogAndExpr> Parser::ParseLogAndExpr() {
  auto begin = mTokCursor;
  std::vector<BitOrExpr> bitOrExprArr;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto bitOrExpr = ParseBitOrExpr();
    if (bitOrExpr)
      bitOrExprArr.push_back(*bitOrExpr);
  } while (Peek(tok::amp_amp));

  if (bitOrExprArr.empty()) {
    return std::nullopt;
  }

  return LogAndExpr(begin, bitOrExprArr);
}

/**
 * inclusive-OR-expression:
 *      exclusive-OR-expression
 *      inclusive-OR-expression | exclusive-OR-expression
 */
std::optional<BitOrExpr> Parser::ParseBitOrExpr() {
  auto begin = mTokCursor;
  std::vector<BitXorExpr> bitXorExprArr;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto bitXorExpr = ParseBitXorExpr();
    if (bitXorExpr) {
      bitXorExprArr.push_back(*bitXorExpr);
    }
  } while (Peek(tok::pipe));

  if (bitXorExprArr.empty())
    return std::nullopt;

  return BitOrExpr(begin, bitXorExprArr);
}

std::optional<BitXorExpr> Parser::ParseBitXorExpr() {
  auto begin = mTokCursor;
  std::vector<BitAndExpr> bitAndExprArr;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto newAnd = ParseBitAndExpr();
    if (newAnd) {
      bitAndExprArr.push_back(std::move(*newAnd));
    }
  } while (Peek(tok::caret));

  if (bitAndExprArr.empty())
    return std::nullopt;

  return BitXorExpr(begin, bitAndExprArr);
}

/**
 * AND-expression:
 *      equality-expression
 *      AND-expression & equality-expression
 */
std::optional<BitAndExpr> Parser::ParseBitAndExpr() {
  auto begin = mTokCursor;
  std::vector<EqualExpr> equalExprArr;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      ConsumeAny();
    }
    auto equalExpr = ParseEqualExpr();
    if (equalExpr) {
      equalExprArr.push_back(*equalExpr);
    }
  } while (Peek(tok::amp));

  if (equalExprArr.empty()) {
    return std::nullopt;
  }
  return BitAndExpr(begin, equalExprArr);
}

/**
 * equality-expression:
 *      relational-expression
 *      equality-expression == relational-expression
 *      equality-expression != relational-expression
 */
std::optional<EqualExpr> Parser::ParseEqualExpr() {
  auto begin = mTokCursor;
  auto firstRelationalExpr = ParseRelationalExpr();
  if (!firstRelationalExpr) {
    return std::nullopt;
  }
  std::vector<std::pair<EqualExpr::Op, RelationalExpr>> relationalExprs;
  while (Peek(tok::equal_equal) || Peek(tok::exclaim_equal)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    EqualExpr::Op equalOp;
    if (tokenType == tok::equal_equal) {
      equalOp = EqualExpr::Op::Equal;
    }else {
      equalOp = EqualExpr::Op::NotEqual;
    }
    ConsumeAny();
    auto relationalExpr = ParseRelationalExpr();
    if (relationalExpr) {
      relationalExprs.emplace_back(equalOp, *relationalExpr);
    }
  }
  return EqualExpr(begin, *firstRelationalExpr, relationalExprs);
}

/**
 * relational-expression:
 *      shift-expression
 *      relational-expression < shift-expression
 *      relational-expression > shift-expression
 *      relational-expression <= shift-expression
 *      relational-expression >= shift-expression
 */
std::optional<RelationalExpr> Parser::ParseRelationalExpr() {
  auto begin = mTokCursor;
  auto firstShiftExpr = ParseShiftExpr();
  if (!firstShiftExpr)
    return {std::nullopt};

  std::vector<std::pair<RelationalExpr::Op, ShiftExpr>> relationalExprArr;
  while (Peek(tok::less) || Peek(tok::less_equal) ||
         Peek(tok::greater) || Peek(tok::greater_equal)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto relationalOp = [tokenType]() -> RelationalExpr::Op {
      switch (tokenType) {
      case tok::less:
        return RelationalExpr::Op::LessThan;
      case tok::less_equal:
        return RelationalExpr::Op::LessThanOrEqual;
      case tok::greater:
        return RelationalExpr::Op::GreaterThan;
      case tok::greater_equal:
        return RelationalExpr::Op::GreaterThanOrEqual;
      default:
        LCC_UNREACHABLE;
      }
    }();
    ConsumeAny();
    auto shiftExpr = ParseShiftExpr();
    if (shiftExpr) {
      relationalExprArr.emplace_back(relationalOp, *shiftExpr);
    }
  }

  return RelationalExpr(begin, *firstShiftExpr, relationalExprArr);
}

/**
 * shift-expression:
 *      additive-expression
 *      shift-expression << additive-expression
 *      shift-expression >> additive-expression
 */
std::optional<ShiftExpr> Parser::ParseShiftExpr() {
  auto begin = mTokCursor;
  auto firstAdditiveExpr = ParseAdditiveExpr();
  if (!firstAdditiveExpr)
    return {std::nullopt};

  std::vector<std::pair<ShiftExpr::Op, AdditiveExpr>> additiveExprArr;
  while (Peek(tok::less_less) || Peek(tok::greater_greater)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    ShiftExpr::Op op;
    if (tokenType == tok::less_less) {
      op = ShiftExpr::Op::Left;
    }else {
      op = ShiftExpr::Op::Right;
    }
    ConsumeAny();
    auto additiveExpr = ParseAdditiveExpr();
    if (additiveExpr) {
      additiveExprArr.emplace_back(op, *additiveExpr);
    }
  }
  return ShiftExpr(begin, *firstAdditiveExpr, additiveExprArr);
}

/**
 * additive-expression:
 * multiplicative-expression
 * additive-expression + multiplicative-expression
 * additive-expression - multiplicative-expression
 */
std::optional<AdditiveExpr> Parser::ParseAdditiveExpr() {
  auto begin = mTokCursor;
  auto firstMultiExpr = ParseMultiExpr();
  if (!firstMultiExpr)
    return std::nullopt;

  std::vector<std::pair<AdditiveExpr::Op, MultiExpr>> multiExprArr;
  while (Peek(tok::plus) || Peek(tok::minus)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    AdditiveExpr::Op op;
    if (tokenType == tok::plus) {
      op = AdditiveExpr::Op::Plus;
    }else {
      op = AdditiveExpr::Op::Minus;
    }
    ConsumeAny();
    auto multiExpr = ParseMultiExpr();
    if (multiExpr) {
      multiExprArr.emplace_back(op, std::move(*multiExpr));
    }
  }
  return AdditiveExpr(begin, *firstMultiExpr, multiExprArr);
}

/**
 * multiplicative-expression:
 *  cast-expression
 *  multiplicative-expression * cast-expression
 *  multiplicative-expression / cast-expression
 *  multiplicative-expression % cast-expression
 */
std::optional<MultiExpr> Parser::ParseMultiExpr() {
  auto begin = mTokCursor;
  auto firstCastExpr = ParseCastExpr();
  if (!firstCastExpr) {
    return std::nullopt;
  }
  std::vector<std::pair<MultiExpr::Op, CastExpr>> castExprArr;
  while (Peek(tok::star) || Peek(tok::slash) || Peek(tok::percent)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto op = [tokenType]() -> MultiExpr::Op {
      switch (tokenType) {
      case tok::star:
        return MultiExpr::Op::Multiply;
      case tok::slash:
        return MultiExpr::Op::Divide;
      case tok::percent:
        return MultiExpr::Op::Modulo;
      default:
        LCC_UNREACHABLE;
      }
    }();
    ConsumeAny();
    auto newCast = ParseCastExpr();
    if (newCast) {
      castExprArr.emplace_back(op, std::move(*newCast));
    }
  }
  return MultiExpr(begin, *firstCastExpr, castExprArr);
}

/**
 * type-name:
 *  specifier-qualifier-list abstract-declarator{opt}
 */
std::optional<TypeName> Parser::ParseTypeName() {
  auto begin = mTokCursor;
  auto specs = ParseDeclarationSpecifiers();
  if (specs.getStorageClassSpecifiers().size() > 0) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_type_name_appear_storage_class);
  }
  if (specs.getTypeSpecs().size() == 0 &&
      specs.getTypeQualifiers().size() == 0) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_type_specifier_or_qualifier);
  }

  if (IsFirstInAbstractDeclarator()) {
    auto abstractDec = ParseAbstractDeclarator();
    if (!abstractDec) {
      return std::nullopt;
    }
    return TypeName(begin, specs, *abstractDec);
  }
  return TypeName(begin, specs);
}

/**
 * cast-expression:
 *      unary-expression
 *      ( type-name ) cast-expression
 *
 * (unsigned char)(h ? h->height + 1 : 0);
 */
std::optional<CastExpr> Parser::ParseCastExpr() {
  auto begin = mTokCursor;
  // cast-expression: unary-expression
  if (!Peek(tok::l_paren)) {
    auto unary = ParseUnaryExpr();
    if (!unary) {
      return std::nullopt;
    }
    return CastExpr(begin, *unary);
  }

  Expect(tok::l_paren);

  if (!IsFirstInTypeName()) {
    // cast-expression: unary-expression
    mTokCursor = begin;
    auto unary = ParseUnaryExpr();
    if (!unary) {
      return std::nullopt;
    }
    return CastExpr(begin, *unary);
  }else {
    // cast-expression: ( type-name ) cast-expression
    auto typeName = ParseTypeName();
    Expect(tok::r_paren);
    auto cast = ParseCastExpr();
    if (typeName && cast) {
      return CastExpr(begin, CastExpr::TypeNameCast{*typeName, *cast});
    }
    return std::nullopt;
  }
}

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
std::optional<UnaryExpr> Parser::ParseUnaryExpr() {
  TokIter begin = mTokCursor;
  if (Peek(tok::kw_sizeof)) {
    ConsumeAny();
    if (Peek(tok::l_paren)) {
      ConsumeAny();
      auto type = ParseTypeName();
      Expect(tok::r_paren);
      if (type) {
        return UnaryExpr(UnaryExprSizeOf(begin, *type));
      }
    } else {
      auto unary = ParseUnaryExpr();
      if (unary) {
        return UnaryExpr(UnaryExprSizeOf(begin, *unary));
      }
    }
  } else if (IsUnaryOp(mTokCursor->getTokenKind())) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto unaryOp = [tokenType]() -> UnaryExprUnaryOperator::Op {
      switch (tokenType) {
      case tok::amp:
        return UnaryExprUnaryOperator::Op::Ampersand;
      case tok::star:
        return UnaryExprUnaryOperator::Op::Asterisk;
      case tok::plus:
        return UnaryExprUnaryOperator::Op::Plus;
      case tok::minus:
        return UnaryExprUnaryOperator::Op::Minus;
      case tok::tilde:
        return UnaryExprUnaryOperator::Op::BitNot;
      case tok::exclaim:
        return UnaryExprUnaryOperator::Op::LogicalNot;
      case tok::plus_plus:
        return UnaryExprUnaryOperator::Op::Increment;
      case tok::minus_minus:
        return UnaryExprUnaryOperator::Op::Decrement;
      default:
        LCC_UNREACHABLE;
      }
    }();
    ConsumeAny();
    auto castExpr = ParseCastExpr();
    if (!castExpr) {
      return UnaryExpr(UnaryExprUnaryOperator(begin, unaryOp, *castExpr));
    }
  } else {
    auto postFix = ParsePostFixExpr();
    if (postFix) {
      return UnaryExpr(*postFix);
    }
  }
  return std::nullopt;
}

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

void Parser::ParsePostFixExprSuffix(TokIter beginTokLoc,
                                    PostFixExpr &postFixExpr) {
  while (IsPostFixExpr(mTokCursor->getTokenKind())) {
    auto tokType = mTokCursor->getTokenKind();
    if (tokType == tok::l_paren) {
      ConsumeAny();
      std::vector<box<AssignExpr>> params;
      bool first = true;
      do {
        if (first) {
          first = false;
        } else {
          Expect(tok::comma);
        }
        auto assignExpr = ParseAssignExpr();
        if (assignExpr) {
          params.push_back(*assignExpr);
        }
      } while (!Peek(tok::r_paren));

      Expect(tok::r_paren);
      postFixExpr = PostFixExprFuncCall(beginTokLoc, postFixExpr, params);
    } else if (tokType == tok::l_square) {
      ConsumeAny();
      auto expr = ParseExpr();
      Expect(tok::r_square);
      if (expr) {
        postFixExpr = PostFixExprSubscript(beginTokLoc, postFixExpr, *expr);
      }
    } else if (tokType == tok::plus_plus) {
      ConsumeAny();
      postFixExpr = PostFixExprIncrement(beginTokLoc, postFixExpr);
    } else if (tokType == tok::minus_minus) {
      ConsumeAny();
      postFixExpr = PostFixExprDecrement(beginTokLoc, postFixExpr);
    } else if (tokType == tok::period) {
      ConsumeAny();
      auto identifier = mTokCursor->getRepresentation();
      Expect(tok::identifier);
      postFixExpr = PostFixExprDot(beginTokLoc, postFixExpr, identifier);
    } else if (tokType == tok::arrow) {
      ConsumeAny();
      auto identifier = mTokCursor->getRepresentation();
      Expect(tok::identifier);
      postFixExpr = PostFixExprArrow(beginTokLoc, postFixExpr, identifier);
    }
  }
}

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
std::optional<PostFixExpr> Parser::ParsePostFixExpr() {
  std::optional<PostFixExpr> postFixExpr{std::nullopt};
  std::optional<PrimaryExpr> primaryExpr{std::nullopt};

  TokIter beginTokLoc = mTokCursor;
  if (Peek(tok::identifier)) {
    auto name = mTokCursor->getRepresentation();
    primaryExpr = PrimaryExprIdent(beginTokLoc, name);
    ConsumeAny();
  }else if (Peek(tok::char_constant) || Peek(tok::numeric_constant) || Peek(tok::string_literal)) {
    using PrimExprConstantValueType = PrimaryExprConstant::Value;
    auto value = std::visit(
        [](auto &&value) -> PrimExprConstantValueType {
          using T = std::decay_t<decltype(value)>;
          if constexpr (std::is_constructible_v<PrimExprConstantValueType, T>) {
            return std::forward<decltype(value)>(value);
          } else {
            LCC_UNREACHABLE;
          }
        },
        mTokCursor->getValue());
    primaryExpr = PrimaryExprConstant(beginTokLoc, value);
    ConsumeAny();
  }else if (Peek(tok::l_paren)) {
    ConsumeAny();
    if (!IsFirstInTypeName()) {
      auto expr = ParseExpr();
      Expect(tok::r_paren);
      if (expr) {
        primaryExpr = PrimaryExprParentheses(beginTokLoc, *expr);
      }
    } else {
      auto type = ParseTypeName();
      Expect(tok::r_paren);
      Expect(tok::l_brace);
      auto initializer = ParseInitializerList();
      if (Peek(tok::comma)) {
        ConsumeAny();
      }
      Expect(tok::r_brace);
      if (type && initializer) {
        postFixExpr =
            PostFixExprTypeInitializer(beginTokLoc, *type, *initializer);
      }
    }
  }else {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "primary expr or ( type-name )");
  }

  if (primaryExpr) {
    postFixExpr = *primaryExpr;
  }

  if (!postFixExpr) {
    return std::nullopt;
  }

  ParsePostFixExprSuffix(beginTokLoc, *postFixExpr);

  return *postFixExpr;
}

bool Parser::IsAssignOp(tok::TokenKind type) {
  return type == tok::equal || type == tok::plus_equal ||
         type == tok::minus_equal || type == tok::star_equal ||
         type == tok::slash_equal || type == tok::percent_equal ||
         type == tok::less_less_equal || type == tok::greater_greater_equal ||
         type == tok::amp_equal || type == tok::pipe_equal ||
         type == tok::caret_equal;
}

bool Parser::Expect(tok::TokenKind tokenType) {
  if (mTokCursor->getTokenKind() == tokenType) {
    ConsumeAny();
    return true;
  }
  DiagReport(Diag, (mTokCursor-1)->getSMLoc(), diag::err_parse_expect_n_after, tok::getTokenName(tokenType));
  return false;
}

bool Parser::ConsumeAny() {
  ++mTokCursor;
  return true;
}
bool Parser::Peek(tok::TokenKind tokenType) {
  if (mTokCursor >= mTokEnd) {
    return false;
  }
  return mTokCursor->getTokenKind() == tokenType;
}

bool Parser::PeekN(int n, tok::TokenKind tokenType) {
  if (n == 0) {
    return Peek(tokenType);
  }
  if (mTokCursor + n >= mTokEnd) {
    return false;
  }
  return (mTokCursor+n)->getTokenKind() == tokenType;
}

bool Parser::IsUnaryOp(tok::TokenKind tokenType) {
  if (tokenType == tok::amp || tokenType == tok::star ||
      tokenType == tok::plus || tokenType == tok::minus ||
      tokenType == tok::tilde || tokenType == tok::exclaim ||
      tokenType == tok::plus_plus || tokenType == tok::minus_minus) {
    return true;
  }
  return false;
}

bool Parser::IsPostFixExpr(tok::TokenKind tokenType) {
  return (tokenType == tok::l_paren || tokenType == tok::l_square ||
          tokenType == tok::period || tokenType == tok::arrow ||
          tokenType == tok::plus_plus || tokenType == tok::minus_minus ||
          tokenType == tok::identifier || tokenType == tok::char_constant ||
          tokenType == tok::numeric_constant);
}

bool Parser::IsCurrentIn(TokenBitSet tokenSet) {
  return tokenSet[mTokCursor->getTokenKind()];
}

void Parser::Scope::addTypedef(std::string_view name) {
  mCurrentScope.back().emplace(name, Symbol{name, true});
}

bool Parser::Scope::isTypedefInScope(std::string_view name) const {
  for (auto iter = mCurrentScope.rbegin(); iter != mCurrentScope.rend();
       iter++) {
    if (auto result = iter->find(name); result != iter->end()) {
      return result->second.isTypedef;
    }
  }
  return false;
}

bool Parser::Scope::checkIsTypedefInCurrentScope(std::string_view name) const {
  auto iter = mCurrentScope.rbegin();
  if (auto result = iter->find(name); result != iter->end())
    return result->second.isTypedef;
  return false;
}

void Parser::Scope::addToScope(std::string_view name) {
  mCurrentScope.back().emplace(name, Symbol{name, false});
}

void Parser::Scope::pushScope() {
  mCurrentScope.emplace_back();
}

void Parser::Scope::popScope() {
  mCurrentScope.pop_back();
}

void Parser::SkipTo(TokenBitSet recoveryToken, unsigned DiagID) {
  if (mTokCursor == mTokEnd || recoveryToken[mTokCursor->getTokenKind()]) {
    return;
  }
  TokIter tok = mTokCursor;
  mTokCursor = std::find_if(mTokCursor, mTokEnd, [&recoveryToken](const Token& tok){
    return recoveryToken[tok.getTokenKind()];
  });
  DiagReport(Diag, tok->getSMLoc(), DiagID);
}

bool Parser::IsFirstInExternalDeclaration() const {
  return IsFirstInDeclaration() || IsFirstInFunctionDefinition();
}
bool Parser::IsFirstInFunctionDefinition() const {
  return IsFirstInDeclarationSpecifier();
}
bool Parser::IsFirstInDeclaration() const {
  return IsFirstInDeclarationSpecifier();
}
bool Parser::IsFirstInDeclarationSpecifier() const {
  switch (mTokCursor->getTokenKind()) {
  case tok::kw_typedef:
  case tok::kw_extern:
  case tok::kw_static:
  case tok::kw_auto:
  case tok::kw_register:
  case tok::kw_void:
  case tok::kw_char:
  case tok::kw_short:
  case tok::kw_int:
  case tok::kw_long:
  case tok::kw_float:
  case tok::kw__Bool:
  case tok::kw_double:
  case tok::kw_signed:
  case tok::kw_unsigned:
  case tok::kw_enum:
  case tok::kw_struct:
  case tok::kw_union:
  case tok::kw_const:
  case tok::kw_restrict:
  case tok::kw_volatile:
  case tok::kw_inline: return true;
  case tok::identifier:
    return mScope.isTypedefInScope(mTokCursor->getRepresentation());
  default:
    return false;
  }
}
bool Parser::IsFirstInSpecifierQualifier() const {
  switch (mTokCursor->getTokenKind()) {
  case tok::kw_void:
  case tok::kw_char:
  case tok::kw_short:
  case tok::kw_int:
  case tok::kw_long:
  case tok::kw_float:
  case tok::kw__Bool:
  case tok::kw_double:
  case tok::kw_signed:
  case tok::kw_unsigned:
  case tok::kw_enum:
  case tok::kw_struct:
  case tok::kw_union:
  case tok::kw_const:
  case tok::kw_restrict:
  case tok::kw_volatile:
  case tok::kw_inline: return true;
  case tok::identifier:
    return mScope.isTypedefInScope(mTokCursor->getRepresentation());
  default:
    return false;
  }
}
bool Parser::IsFirstInDeclarator() const {
  return IsFirstInPointer() || IsFirstInDirectDeclarator();
}
bool Parser::IsFirstInDirectDeclarator() const {
  return mTokCursor->getTokenKind() == tok::identifier ||
         mTokCursor->getTokenKind() == tok::l_paren;
}
bool Parser::IsFirstInParameterTypeList() const {
  return IsFirstInParameterList();
}
bool Parser::IsFirstInAbstractDeclarator() const {
  return IsFirstInPointer() || IsFirstInDirectAbstractDeclarator();
}
bool Parser::IsFirstInDirectAbstractDeclarator() const {
  // tok::l_paren, tok::l_square
  return mTokCursor->getTokenKind() == tok::l_paren ||
         mTokCursor->getTokenKind() == tok::l_square;
}
bool Parser::IsFirstInParameterList() const {
  return IsFirstInDeclarationSpecifier();
}
bool Parser::IsFirstInPointer() const {
  return mTokCursor->getTokenKind() == tok::star;
}
bool Parser::IsFirstInBlockItem() const {
  return IsFirstInDeclaration() || IsFirstInStatement();
}
bool Parser::IsFirstInInitializer() const {
  return IsFirstInAssignmentExpr() || mTokCursor->getTokenKind() == tok::l_brace;
}
bool Parser::IsFirstInInitializerList() const {
  // tok::l_square, tok::period
  return mTokCursor->getTokenKind() == tok::l_square ||
         mTokCursor->getTokenKind() == tok::period ||
         IsFirstInInitializer();
}
bool Parser::IsFirstInStatement() const {
  return mTokCursor->getTokenKind() == tok::kw_if ||
         mTokCursor->getTokenKind() == tok::kw_for ||
         mTokCursor->getTokenKind() == tok::l_brace ||
         mTokCursor->getTokenKind() == tok::kw_switch ||
         mTokCursor->getTokenKind() == tok::kw_continue ||
         mTokCursor->getTokenKind() == tok::kw_break ||
         mTokCursor->getTokenKind() == tok::kw_case ||
         mTokCursor->getTokenKind() == tok::kw_default ||
         mTokCursor->getTokenKind() == tok::identifier ||
         mTokCursor->getTokenKind() == tok::kw_do ||
         mTokCursor->getTokenKind() == tok::kw_while ||
         mTokCursor->getTokenKind() == tok::kw_return ||
         mTokCursor->getTokenKind() == tok::kw_goto ||
         mTokCursor->getTokenKind() == tok::semi ||
         mTokCursor->getTokenKind() == tok::l_brace ||
         IsFirstInExpr();
}
bool Parser::IsFirstInExpr() const {
  return IsFirstInAssignmentExpr();
}
bool Parser::IsFirstInAssignmentExpr() const {
  return IsFirstInConditionalExpr();
}
bool Parser::IsFirstInConditionalExpr() const {
  return IsFirstInLogicalOrExpr();
}
bool Parser::IsFirstInLogicalOrExpr() const {
  return IsFirstInLogicalAndExpr();
}
bool Parser::IsFirstInLogicalAndExpr() const {
  return IsFirstInBitOrExpr();
}
bool Parser::IsFirstInBitOrExpr() const {
  return IsFirstInBitXorExpr();
}
bool Parser::IsFirstInBitXorExpr() const {
  return IsFirstInBitAndExpr();
}
bool Parser::IsFirstInBitAndExpr() const {
  return IsFirstInEqualExpr();
}
bool Parser::IsFirstInEqualExpr() const {
  return IsFirstRelationalExpr();
}
bool Parser::IsFirstRelationalExpr() const {
  return IsFirstInShiftExpr();
}
bool Parser::IsFirstInShiftExpr() const {
  return IsFirstInAdditiveExpr();
}
bool Parser::IsFirstInAdditiveExpr() const {
  return IsFirstInMultiExpr();
}
bool Parser::IsFirstInMultiExpr() const {
 return IsFirstInCastExpr();
}
bool Parser::IsFirstInTypeName() const {
  return IsFirstInSpecifierQualifier();
}
bool Parser::IsFirstInCastExpr() const {
  return mTokCursor->getTokenKind() == tok::l_paren ||
  IsFirstInUnaryExpr();
}
bool Parser::IsFirstInUnaryExpr() const {
  return IsFirstInPostFixExpr() ||
         mTokCursor->getTokenKind() == tok::plus_plus ||
         mTokCursor->getTokenKind() == tok::minus_minus ||
         mTokCursor->getTokenKind() == tok::amp ||
         mTokCursor->getTokenKind() == tok::star ||
         mTokCursor->getTokenKind() == tok::plus ||
         mTokCursor->getTokenKind() == tok::minus ||
         mTokCursor->getTokenKind() == tok::tilde ||
         mTokCursor->getTokenKind() == tok::exclaim ||
         mTokCursor->getTokenKind() == tok::kw_sizeof;
}
bool Parser::IsFirstInPostFixExpr() const {
  return mTokCursor->getTokenKind() == tok::l_paren || IsFirstInPrimaryExpr();
}
bool Parser::IsFirstInPrimaryExpr() const {
  return mTokCursor->getTokenKind() == tok::l_paren ||
  mTokCursor->getTokenKind() == tok::identifier ||
  mTokCursor->getTokenKind() == tok::char_constant ||
  mTokCursor->getTokenKind() == tok::numeric_constant ||
  mTokCursor->getTokenKind() == tok::string_literal;
}
} // namespace lcc