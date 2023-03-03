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
Parser::Parser(std::vector<Token> && tokens, DiagnosticEngine &diag)
    : mTokens(std::move(tokens)), mTokCursor(mTokens.cbegin()),
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

Syntax::TranslationUnit Parser::ParseTranslationUnit() {
  std::vector<Syntax::ExternalDeclaration> decls;
  while (mTokCursor != mTokEnd) {
    if (Peek(tok::semi)) {
      ConsumeAny();
    }
    auto result = ParseExternalDeclaration();
    if (result) {
      decls.push_back(std::move(*result));
    }
    SkipTo(FirstExternalDeclaration, diag::err_parse_skip_to_first_external_declaration);
  }
  return Syntax::TranslationUnit(std::move(decls));
}

Syntax::DeclarationSpecifiers Parser::ParseDeclarationSpecifiers() {
  Syntax::DeclarationSpecifiers declarationSpecifiers;
  bool seeTy = false;
next_specifier:
  switch (mTokCursor->getTokenKind()) {
  case tok::kw_auto: {
    declarationSpecifiers.addStorageClassSpecifier(
        Syntax::StorageClassSpecifier(Syntax::StorageClassSpecifier::Auto));
    ConsumeAny();
    break;
  }
  case tok::kw_register: {
    declarationSpecifiers.addStorageClassSpecifier(
        Syntax::StorageClassSpecifier(Syntax::StorageClassSpecifier::Register));
    ConsumeAny();
    break;
  }
  case tok::kw_static: {
    declarationSpecifiers.addStorageClassSpecifier(
        Syntax::StorageClassSpecifier(Syntax::StorageClassSpecifier::Static));
    ConsumeAny();
    break;
  }
  case tok::kw_extern: {
    declarationSpecifiers.addStorageClassSpecifier(
        Syntax::StorageClassSpecifier(Syntax::StorageClassSpecifier::Extern));
    ConsumeAny();
    break;
  }
  case tok::kw_typedef: {
    declarationSpecifiers.addStorageClassSpecifier(
        Syntax::StorageClassSpecifier(Syntax::StorageClassSpecifier::Typedef));
    ConsumeAny();
    break;
  }
  case tok::kw_volatile: {
    declarationSpecifiers.addTypeQualifier(Syntax::TypeQualifier(Syntax::TypeQualifier::Volatile));
    ConsumeAny();
    break;
  }
  case tok::kw_const: {
    declarationSpecifiers.addTypeQualifier(Syntax::TypeQualifier(Syntax::TypeQualifier::Const));
    ConsumeAny();
    break;
  }
  case tok::kw_restrict: {
    declarationSpecifiers.addTypeQualifier(Syntax::TypeQualifier(Syntax::TypeQualifier::Restrict));
    ConsumeAny();
    break;
  }
  case tok::kw_void: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Void));
    ConsumeAny();
    break;
  }
  case tok::kw_char: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Char));
    ConsumeAny();
    break;
  }
  case tok::kw_short: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Short));
    ConsumeAny();
    break;
  }
  case tok::kw_int: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Int));
    ConsumeAny();
    break;
  }
  case tok::kw_long: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Long));
    ConsumeAny();
    break;
  }
  case tok::kw_float: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Float));
    ConsumeAny();
    break;
  }
  case tok::kw_double: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Double));
    ConsumeAny();
    break;
  }
  case tok::kw_signed: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Signed));
    ConsumeAny();
    break;
  }
  case tok::kw_unsigned: {
    seeTy = true;
    declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(Syntax::TypeSpecifier::Unsigned));
    ConsumeAny();
    break;
  }
  case tok::kw_union:
  case tok::kw_struct: {
    auto expected = ParseStructOrUnionSpecifier();
    if (expected) {
      declarationSpecifiers.addTypeSpecifier(
          Syntax::TypeSpecifier(std::make_unique<Syntax::StructOrUnionSpecifier>(std::move(*expected))));
    }
    seeTy = true;
    break;
  }
  case tok::kw_enum: {
    auto expected = ParseEnumSpecifier();
    if (expected) {
      declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(
          std::make_unique<Syntax::EnumSpecifier>(std::move(*expected))));
    }
    seeTy = true;
    break;
  }
  case tok::identifier: {
    auto name = mTokCursor->getRepresentation();
    if (!seeTy && mScope.isTypedefInScope(name)) {
      ConsumeAny();
      declarationSpecifiers.addTypeSpecifier(Syntax::TypeSpecifier(name));
      seeTy = true;
      break;
    }
    return declarationSpecifiers;
  }
  default:
    return declarationSpecifiers;
  }
  goto next_specifier;
}

std::optional<Syntax::Declaration> Parser::ParseDeclarationSuffix(
    Syntax::DeclarationSpecifiers &&declarationSpecifiers,
    std::optional<Syntax::Declarator> alreadyParsedDeclarator) {
  bool isTypedef = std::any_of(declarationSpecifiers.getStorageClassSpecifiers().begin(), declarationSpecifiers.getStorageClassSpecifiers().end(),
   [](const Syntax::StorageClassSpecifier& storage) {
     return storage.getSpecifier() == Syntax::StorageClassSpecifier::Typedef;
   });
  std::vector<Syntax::Declaration::InitDeclarator> initDeclarators;
  if (alreadyParsedDeclarator) {
    if (!isTypedef) {
      auto name = getDeclaratorName(*alreadyParsedDeclarator);
      mScope.addToScope(name);
    }
    if (!Peek(tok::equal)) {
      initDeclarators.push_back({
          std::make_unique<Syntax::Declarator>
              (std::move(*alreadyParsedDeclarator)),nullptr});
    }else {
      Expect(tok::equal);
      auto initializer = ParseInitializer();
      if (initializer) {
        initDeclarators.push_back({
            std::make_unique<Syntax::Declarator>(std::move(*alreadyParsedDeclarator)),
            std::make_unique<Syntax::Initializer>(std::move(*initializer))});
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
    auto declarator = ParseDeclarator();
    if (!isTypedef && declarator) {
      auto name = getDeclaratorName(*declarator);
      mScope.addToScope(name);
    }
    if (!Peek(tok::equal) && declarator) {
      initDeclarators.push_back(
          {std::make_unique<Syntax::Declarator>(std::move(*declarator)),
           nullptr});
    } else {
      Expect(tok::equal);
      auto initializer = ParseInitializer();
      if (initializer && declarator) {
        initDeclarators.push_back(
            {std::make_unique<Syntax::Declarator>(std::move(*declarator)),
             std::make_unique<Syntax::Initializer>(std::move(*initializer))});
      }
    }
  }while (Peek(tok::comma));

End:
  Expect(tok::semi);
  if (isTypedef) {
    for (auto& iter : initDeclarators) {
      auto name = getDeclaratorName(*iter.mDeclarator);
      mScope.addTypedef(name);
    }
  }
  return Syntax::Declaration(std::move(declarationSpecifiers), std::move(initDeclarators));
}

std::optional<Syntax::ExternalDeclaration> Parser::ParseExternalDeclaration() {
  auto declarationSpecifiers = ParseDeclarationSpecifiers();
  if (declarationSpecifiers.isEmpty()) {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Syntax::Declaration(std::move(declarationSpecifiers), {});
  }
  auto declarator = ParseDeclarator();
  /// function define
  if (Peek(tok::l_brace) && declarator) {
    const auto *parameters = getFuncDeclarator(*declarator);
    if (!parameters) {
      return {};
    }
    /// func param and block stmt share a scope
    mScope.pushScope();
    auto &parameterDeclarations = parameters->getParameterTypeList().getParameterList().getParameterDeclarations();
    /// todo check repeat param name
    for (auto &[declSpecifiers, parameterDeclarator] : parameterDeclarations) {
      /// check is void
      auto isVoidType = [&parameterDeclarations](const Syntax::DeclarationSpecifiers &declSpec) {
        return declSpec.getStorageClassSpecifiers().size() == 0 &&
               declSpec.getTypeQualifiers().size() == 0 &&
               declSpec.getFunctionSpecifiers().size() == 0 &&
               declSpec.getTypeSpecifiers().size() == 1 && (parameterDeclarations.size() == 1);
      };
      if (isVoidType(declSpecifiers)) {
        auto* primitive = std::get_if<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(
            &declSpecifiers.getTypeSpecifiers()[0].getVariant());
        if (primitive && *primitive == Syntax::TypeSpecifier::PrimitiveTypeSpecifier::Void) {
          break;
        }
      }
      if (std::holds_alternative<std::optional<std::unique_ptr<Syntax::AbstractDeclarator>>>(parameterDeclarator)) {
        continue;
      }
      auto& decl = std::get<std::unique_ptr<Syntax::Declarator>>(parameterDeclarator);
      mScope.addToScope(getDeclaratorName(*decl));
    }
    auto compoundStmt = ParseBlockStmt();
    mScope.popScope();
    mScope.addToScope(getDeclaratorName(*declarator));
    if (compoundStmt) {
      return Syntax::FunctionDefinition(std::move(declarationSpecifiers),
                                        std::move(*declarator),
                                        std::move(*compoundStmt));
    }
    return {};
  }
  /// is global declaration
  return ParseDeclarationSuffix(std::move(declarationSpecifiers),
                                std::move(declarator));
}

/// declaration: declaration-specifiers init-declarator-list{opt} ;
std::optional<Syntax::Declaration> Parser::ParseDeclaration() {
  auto declarationSpecifiers = ParseDeclarationSpecifiers();
  if (declarationSpecifiers.isEmpty()) {
    DiagReport(Diag, mTokCursor->getSMLoc(),
               diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Syntax::Declaration(std::move(declarationSpecifiers), {});
  }
  return ParseDeclarationSuffix(std::move(declarationSpecifiers));
}

std::optional<Syntax::StructOrUnionSpecifier>
Parser::ParseStructOrUnionSpecifier() {
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
    return Syntax::StructOrUnionSpecifier(isUnion, tagName, {});
  }
  case tok::l_brace: {
  lbrace:
    ConsumeAny();
    mScope.pushScope();
    std::vector<Syntax::StructOrUnionSpecifier::StructDeclaration>
        structDeclarations;
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
    return Syntax::StructOrUnionSpecifier(isUnion, tagName,
                                          std::move(structDeclarations));
  }
  default:
    DiagReport(Diag, start->getSMLoc(), diag::err_parse_expect_n, "identifier or { after struct/union");
    return {};
  }
}

std::optional<Syntax::StructOrUnionSpecifier::StructDeclaration> Parser::ParseStructDeclaration() {
    TokIter begin = mTokCursor;

    // to support struct {;},	empty struct/union declaration
    if (Peek(tok::semi)) {
      ConsumeAny();
      return {};
    }

    auto specs = ParseDeclarationSpecifiers();
    if (specs.getStorageClassSpecifiers().size() > 0) {
      DiagReport(Diag, begin->getSMLoc(), diag::err_parse_struct_declaration_appear_storage_class);
    }
    if (specs.getTypeSpecifiers().size() == 0 && specs.getTypeQualifiers().size() == 0) {
      DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_type_specifier_or_qualifier);
    }
    std::vector<
        Syntax::StructOrUnionSpecifier::StructDeclaration::StructDeclarator>
        declarators;
    auto stDec = ParseStructDeclarator();
    if (stDec) {
      declarators.push_back(std::move(*stDec));
    }
    while (Peek(tok::comma)) {
      ConsumeAny();
      auto stDec2 = ParseStructDeclarator();
      if (stDec2) {
        declarators.push_back(std::move(*stDec2));
      }
    }
    Expect(tok::semi);
    return Syntax::StructOrUnionSpecifier::StructDeclaration{std::move(specs), std::move(declarators)};
}


std::optional<Syntax::StructOrUnionSpecifier::StructDeclaration::StructDeclarator> Parser::ParseStructDeclarator() {
  SetCheckTypedefType(false);
  auto declarator = ParseDeclarator();
  SetCheckTypedefType(true);
  if (declarator)
    mScope.addToScope(getDeclaratorName(*declarator));
  if (Peek(tok::colon) && declarator) {
    ConsumeAny();
    auto constant = ParseConditionalExpr();
    return Syntax::StructOrUnionSpecifier::StructDeclaration::
        StructDeclarator{
            std::make_unique<Syntax::Declarator>(std::move(*declarator)),
            std::move(*constant)};
  } else if (declarator){
    return Syntax::StructOrUnionSpecifier::StructDeclaration::StructDeclarator{
        std::make_unique<Syntax::Declarator>(std::move(*declarator)),
         std::nullopt};
  }else {
    return {};
  }
}

/// declarator: pointer{opt} direct-declarator
std::optional<Syntax::Declarator> Parser::ParseDeclarator() {
  std::vector<Syntax::Pointer> pointers;
  while (Peek(tok::star)) {
    auto result = ParsePointer();
    pointers.push_back(std::move(result));
  }
  auto directDeclarator = ParseDirectDeclarator();
  if (!directDeclarator) {
    return {};
  }
  return Syntax::Declarator(std::move(pointers), std::move(*directDeclarator));
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
std::optional<Syntax::DirectDeclarator>
Parser::ParseDirectDeclaratorSuffix(std::unique_ptr<Syntax::DirectDeclarator>&& directDeclarator) {
  while (Peek(tok::l_paren) || Peek(tok::l_square)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::l_paren: {
      ConsumeAny();
      if (IsFirstInDeclarationSpecifier()) {
        SetCheckTypedefType(false);
        auto parameterTypeList = ParseParameterTypeList();
        SetCheckTypedefType(true);
        if (parameterTypeList) {
          directDeclarator = std::make_unique<Syntax::DirectDeclarator>(
              Syntax::DirectDeclaratorParamTypeList(std::move(directDeclarator),
                                                    std::move(*parameterTypeList))
          );
        }
      }else {
        directDeclarator = std::make_unique<Syntax::DirectDeclarator>(
    Syntax::DirectDeclaratorParamTypeList(std::move(directDeclarator),
                                                Syntax::ParamTypeList(Syntax::ParamList({}), false)));
      }
      Expect(tok::r_paren);
      break;
    }
    case tok::l_square: {
      ConsumeAny();
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        std::vector<Syntax::TypeQualifier> typeQualifiers;
        while (Peek(tok::kw_const) || Peek(tok::kw_volatile)
               || Peek(tok::kw_restrict)) {
          switch (mTokCursor->getTokenKind()) {
          case tok::kw_const: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Const));
            break;
          }
          case tok::kw_volatile: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Volatile));
            break;
          }
          case tok::kw_restrict: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Restrict));
            break;
          }
          default:
            break;
          }
          ConsumeAny();
        }
        auto assignment = ParseAssignExpr();
        if (assignment) {
          directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorAssignExpr(
              std::move(directDeclarator), std::move(typeQualifiers),
              std::make_unique<Syntax::AssignExpr>(std::move(*assignment)), true));
        }
        Expect(tok::r_square);
        break;
      }

      std::vector<Syntax::TypeQualifier> typeQualifiers;
      while (Peek(tok::kw_const) || Peek(tok::kw_volatile)
             || Peek(tok::kw_restrict)) {
        switch (mTokCursor->getTokenKind()) {
        case tok::kw_const: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Const));
          break;
        }
        case tok::kw_volatile: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Volatile));
          break;
        }
        case tok::kw_restrict: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Restrict));
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
          directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorAssignExpr(
              std::move(directDeclarator), std::move(typeQualifiers),
              std::make_unique<Syntax::AssignExpr>(std::move(*assignment)), true));
        }
        Expect(tok::r_square);
      }else if (Peek(tok::star)) {
        ConsumeAny();
        directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorAsterisk(
            std::move(directDeclarator), std::move(typeQualifiers)));
        Expect(tok::r_square);
      }else if (IsFirstInAssignmentExpr()) {
        auto assignment = ParseAssignExpr();
        if (assignment) {
          directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorAssignExpr(
              std::move(directDeclarator), std::move(typeQualifiers),
              std::make_unique<Syntax::AssignExpr>(std::move(*assignment)), false));
        }
        Expect(tok::r_square);
      }else {
        directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorAssignExpr(
            std::move(directDeclarator), std::move(typeQualifiers),
            nullptr, false));
        Expect(tok::r_square);
      }
      break;
    }
    default:
      break;
    }
  }
  if (!directDeclarator)
    return {};
  return std::move(*directDeclarator);
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
std::optional<Syntax::DirectDeclarator> Parser::ParseDirectDeclarator() {
  std::unique_ptr<Syntax::DirectDeclarator> directDeclarator;

  if (Peek(tok::identifier)) {
    auto name = mTokCursor->getRepresentation();
    if (IsCheckTypedefType()) {
      if (mScope.checkIsTypedefInCurrentScope(name)) {
        DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "identifier, but get a typedef type");
      }
    }
    ConsumeAny();
    directDeclarator = std::make_unique<Syntax::DirectDeclarator>(Syntax::DirectDeclaratorIdent(name));
  }else if (Peek(tok::l_paren)) {
    ConsumeAny();
    auto declarator = ParseDeclarator();
    if (declarator) {
      directDeclarator = std::make_unique<Syntax::DirectDeclarator>(
          Syntax::DirectDeclaratorParentheses(std::make_unique<Syntax::Declarator>(std::move(*declarator))));
    }
    Expect(tok::r_paren);
  }else {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "identifier or (");
  }

  return ParseDirectDeclaratorSuffix(std::move(directDeclarator));
}

/*
parameter-type-list:
  parameter-list
  parameter-list , ...
 */
std::optional<Syntax::ParamTypeList> Parser::ParseParameterTypeList() {
  auto parameterList = ParseParameterList();
  bool hasEllipse = false;
  if (Peek(tok::comma)) {
    ConsumeAny();
    Expect(tok::ellipsis);
    hasEllipse = true;
  }
  if (parameterList) {
    return Syntax::ParamTypeList(std::move(*parameterList), hasEllipse);
  }
  return {};
}

/**
parameter-list:
    parameter-declaration
    parameter-list , parameter-declaration
 */
std::optional<Syntax::ParamList> Parser::ParseParameterList() {
  std::vector<Syntax::ParameterDeclaration> parameterDeclarations;
  auto declaration = ParseParameterDeclaration();
  if (declaration) {
    parameterDeclarations.push_back(std::move(*declaration));
  }
  /// fix parse tok::ellipsis
  while (Peek(tok::comma) && !PeekN(1, tok::ellipsis)) {
    ConsumeAny();
    declaration = ParseParameterDeclaration();
    if (declaration) {
      parameterDeclarations.push_back(std::move(*declaration));
    }
  }
  return Syntax::ParamList(std::move(parameterDeclarations));
}
std::optional<Syntax::ParameterDeclaration>
Parser::ParseParameterDeclarationSuffix(Syntax::DeclarationSpecifiers &declarationSpecifiers) {
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
      return Syntax::ParameterDeclaration(std::move(declarationSpecifiers),
                                          std::make_unique<Syntax::Declarator>(std::move(*dec)));
    }
  }else {
    auto absDec = ParseAbstractDeclarator();
    if (absDec) {
      return Syntax::ParameterDeclaration(std::move(declarationSpecifiers),
                                          std::make_unique<Syntax::AbstractDeclarator>(std::move(*absDec)));
    }
  }
  return {};
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
std::optional<Syntax::ParameterDeclaration>
Parser::ParseParameterDeclaration() {
  auto begin = mTokCursor;
  auto specs = ParseDeclarationSpecifiers();
  if (specs.isEmpty()) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_storage_class_or_type_specifier_or_qualifier);
  }
  /// abstract-declarator{opt}
  if (Peek(tok::comma) || Peek(tok::r_paren)) {
    return Syntax::ParameterDeclaration(
        std::move(specs),
        std::nullopt);
  }

  return ParseParameterDeclarationSuffix(specs);
}

/**
 pointer:
    * type-qualifier-list{opt}
    * type-qualifier-list{opt} pointer
 */
Syntax::Pointer Parser::ParsePointer() {
  Expect(tok::star);
  std::vector<Syntax::TypeQualifier> typeQualifier;
  while (Peek(tok::kw_const) || Peek(tok::kw_restrict) ||
         Peek(tok::kw_volatile)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::kw_const:
      typeQualifier.push_back(Syntax::TypeQualifier::Const);
      break;
    case tok::kw_restrict:
      typeQualifier.push_back(Syntax::TypeQualifier::Restrict);
      break;
    case tok::kw_volatile:
      typeQualifier.push_back(Syntax::TypeQualifier::Volatile);
      break;
    default:
      break;
    }
    ConsumeAny();
  }
  return Syntax::Pointer(std::move(typeQualifier));
}

/**
 abstract-declarator:
   pointer
   pointer{opt} direct-abstract-declarator
 */
std::optional<Syntax::AbstractDeclarator> Parser::ParseAbstractDeclarator() {
  std::vector<Syntax::Pointer> pointers;
  while (Peek(tok::star)) {
    auto result = ParsePointer();
    pointers.push_back(std::move(result));
  }
  if (!pointers.empty() && !IsFirstInDirectAbstractDeclarator()) {
    return Syntax::AbstractDeclarator(std::move(pointers));
  }
  auto result = ParseDirectAbstractDeclarator();
  return Syntax::AbstractDeclarator(std::move(pointers), std::move(result));
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
std::optional<Syntax::DirectAbstractDeclarator>
Parser::ParseDirectAbstractDeclaratorSuffix(std::unique_ptr<Syntax::DirectAbstractDeclarator>&& directAbstractDeclarator) {
  while (Peek(tok::l_paren) || Peek(tok::l_square)) {
    switch (mTokCursor->getTokenKind()) {
    case tok::l_paren: {
      ConsumeAny();
      /// direct-abstract-declarator{opt} ( parameter-type-list{opt} )
      if (IsFirstInParameterTypeList()) {
        auto parameterTypeList = ParseParameterTypeList();
        if (parameterTypeList) {
          directAbstractDeclarator =
              std::make_unique<Syntax::DirectAbstractDeclarator>(
                  Syntax::DirectAbstractDeclaratorParamTypeList(
                      std::move(directAbstractDeclarator),
                      std::make_unique<Syntax::ParamTypeList>(
                          std::move(*parameterTypeList))));
        }
      }
      /// abstract-declarator first set
      /// ( abstract-declarator )
      else if (IsFirstInAbstractDeclarator()) {
        auto abstractDeclarator = ParseAbstractDeclarator();
        if (abstractDeclarator) {
          directAbstractDeclarator =
              std::make_unique<Syntax::DirectAbstractDeclarator>(
                  std::make_unique<Syntax::AbstractDeclarator>(
                      std::move(*abstractDeclarator)));
        }
      } else {
        /// direct-abstract-declarator{opt} (  )
        directAbstractDeclarator =
            std::make_unique<Syntax::DirectAbstractDeclarator>(
                Syntax::DirectAbstractDeclaratorParamTypeList(
                    std::move(directAbstractDeclarator), nullptr));
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
            std::make_unique<Syntax::DirectAbstractDeclarator>(
                Syntax::DirectAbstractDeclaratorAsterisk(
                    std::move(directAbstractDeclarator)));
        Expect(tok::r_square);
        break;
      }

      /// direct-abstract-declarator{opt} [ static type-qualifier-list{opt} assignment-expression ]
      if (Peek(tok::kw_static)) {
        ConsumeAny();
        std::vector<Syntax::TypeQualifier> typeQualifiers;
        while (Peek(tok::kw_const) || Peek(tok::kw_volatile) ||
               Peek(tok::kw_restrict)) {
          switch (mTokCursor->getTokenKind()) {
          case tok::kw_const: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Const));
            break;
          }
          case tok::kw_volatile: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Volatile));
            break;
          }
          case tok::kw_restrict: {
            typeQualifiers.push_back(
                Syntax::TypeQualifier(Syntax::TypeQualifier::Restrict));
            break;
          }
          default:
            break;
          }
          ConsumeAny();
        }
        auto assignExpr = ParseAssignExpr();
        if (assignExpr) {
          directAbstractDeclarator =
              std::make_unique<Syntax::DirectAbstractDeclarator>(
                  Syntax::DirectAbstractDeclaratorAssignExpr(
                      std::move(directAbstractDeclarator),
                      std::move(typeQualifiers),
                      std::make_unique<Syntax::AssignExpr>(
                          std::move(*assignExpr)), true));
        }
        Expect(tok::r_square);
        break;
      }

      std::vector<Syntax::TypeQualifier> typeQualifiers;
      while (Peek(tok::kw_const) || Peek(tok::kw_volatile) ||
             Peek(tok::kw_restrict)) {
        switch (mTokCursor->getTokenKind()) {
        case tok::kw_const: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Const));
          break;
        }
        case tok::kw_volatile: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Volatile));
          break;
        }
        case tok::kw_restrict: {
          typeQualifiers.push_back(
              Syntax::TypeQualifier(Syntax::TypeQualifier::Restrict));
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
          directAbstractDeclarator =
              std::make_unique<Syntax::DirectAbstractDeclarator>(
                  Syntax::DirectAbstractDeclaratorAssignExpr(
                      std::move(directAbstractDeclarator),
                      std::move(typeQualifiers),
                      std::make_unique<Syntax::AssignExpr>(
                          std::move(*assignExpr)), true));
        }
        Expect(tok::r_square);
      } else {
        if (!Peek(tok::r_square)) {
          auto assignment = ParseAssignExpr();
          if (assignment) {
            directAbstractDeclarator =
                std::make_unique<Syntax::DirectAbstractDeclarator>(
                    Syntax::DirectAbstractDeclaratorAssignExpr(
                        std::move(directAbstractDeclarator),
                        std::move(typeQualifiers),
                        std::make_unique<Syntax::AssignExpr>(
                            std::move(*assignment)),false));
          }
        } else {
          directAbstractDeclarator =
              std::make_unique<Syntax::DirectAbstractDeclarator>(
                  Syntax::DirectAbstractDeclaratorAssignExpr(
                      std::move(directAbstractDeclarator),
                      std::move(typeQualifiers), nullptr, false));
        }
        Expect(tok::r_square);
        break;
      }
    }
    default:
      break;
    }
  }
  if (!directAbstractDeclarator) {
    return {};
  }
  return std::move(*directAbstractDeclarator);
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
std::optional<Syntax::DirectAbstractDeclarator>
Parser::ParseDirectAbstractDeclarator() {
  return ParseDirectAbstractDeclaratorSuffix({});
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
std::optional<Syntax::EnumSpecifier> Parser::ParseEnumSpecifier() {
  Expect(tok::kw_enum);

  std::vector<Syntax::EnumSpecifier::Enumerator> enumerators;
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
    enumerators.push_back(std::move(*ParseEnumerator()));
    while (Peek(tok::comma)) {
      ConsumeAny();
      if (Peek(tok::r_brace))
        break;
      enumerators.push_back(std::move(*ParseEnumerator()));
    }
    Expect(tok::r_brace);
  }else {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "identifier or { after enum");
  }
  return Syntax::EnumSpecifier(tagName, std::move(enumerators));
}

std::optional<Syntax::EnumSpecifier::Enumerator> Parser::ParseEnumerator() {
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
    return Syntax::EnumSpecifier::Enumerator(enumValueName, std::move(constant));
  }else {
    return Syntax::EnumSpecifier::Enumerator(enumValueName);
  }
}

std::optional<Syntax::BlockStmt> Parser::ParseBlockStmt() {
  Expect(tok::l_brace);
  std::vector<Syntax::BlockItem> items;
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
  return Syntax::BlockStmt(std::move(items));
}

std::optional<Syntax::BlockItem> Parser::ParseBlockItem() {
  auto start = mTokCursor;
  if (IsFirstInDeclarationSpecifier()) {
    auto declaration = ParseDeclaration();
    if (declaration) {
      return Syntax::BlockItem(std::move(*declaration));
    }
  }
  auto statement = ParseStmt();
  if (statement) {
    return Syntax::BlockItem(std::move(*statement));
  }
  return {};
}

/**
 initializer:
    assignment-expression
    { initializer-list }
    { initializer-list , }
 */
std::optional<Syntax::Initializer> Parser::ParseInitializer() {
  if (!Peek(tok::l_brace)) {
    auto assignment = ParseAssignExpr();
    if (assignment) {
      return Syntax::Initializer(std::move(*assignment));
    }
  } else {
    Expect(tok::l_brace);
    auto initializerList = ParseInitializerList();
    if (Peek(tok::comma)) {
      ConsumeAny();
    }
    Expect(tok::r_brace);
    if (initializerList) {
      auto q = std::make_unique<Syntax::InitializerList>(
          std::move(*initializerList));
      return Syntax::Initializer(std::move(q));
    }
  }
  return {};
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
std::optional<Syntax::InitializerList> Parser::ParseInitializerList() {
  typename Syntax::InitializerList::vector vector;
  bool first = true;
  do {
    if (first) {
      first = false;
    } else {
      Expect(tok::comma);
    }
    Syntax::InitializerList::DesignatorList designation;
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
      vector.push_back({std::move(*initializer), std::move(designation)});
  } while (Peek(tok::comma));
  return Syntax::InitializerList{std::move(vector)};
}

std::optional<Syntax::Stmt> Parser::ParseStmt() {
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
    return Syntax::Stmt(std::move(*s));
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
    auto start = mTokCursor;
    if (Peek(tok::identifier)) {
      ConsumeAny();
      if (Peek(tok::colon)) {
        ConsumeAny();
        return Syntax::Stmt(Syntax::LabelStmt(start->getRepresentation()));
      }else {
        mTokCursor = start;
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
std::optional<Syntax::Stmt> Parser::ParseIfStmt() {
  Expect(tok::kw_if);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto thenStmt = ParseStmt();
  if (Peek(tok::kw_else)) {
    ConsumeAny();
    auto elseStmt = ParseStmt();
    if (expr && thenStmt && elseStmt) {
      return Syntax::Stmt{
          Syntax::IfStmt(std::move(*expr),
                         std::make_unique<Syntax::Stmt>(std::move(*thenStmt)),
                         std::make_unique<Syntax::Stmt>(std::move(*elseStmt)))};
    }
  } else {
    if (expr && thenStmt) {
      return Syntax::Stmt{
          Syntax::IfStmt(std::move(*expr),
                         std::make_unique<Syntax::Stmt>(std::move(*thenStmt)))};
    }
  }
  return {};
}

/// while ( expression ) statement
std::optional<Syntax::Stmt> Parser::ParseWhileStmt() {
  Expect(tok::kw_while);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto stmt = ParseStmt();
  if (expr && stmt) {
    return Syntax::Stmt{Syntax::WhileStmt(
        std::move(*expr), std::make_unique<Syntax::Stmt>(std::move(*stmt)))};
  }
  return {};
}

/// do statement while ( expression ) ;
std::optional<Syntax::Stmt> Parser::ParseDoWhileStmt() {
  Expect(tok::kw_do);
  auto stmt = ParseStmt();
  Expect(tok::kw_while);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  Expect(tok::semi);
  if (stmt && expr) {
    return Syntax::Stmt{Syntax::DoWhileStmt(
        std::make_unique<Syntax::Stmt>(std::move(*stmt)), std::move(*expr))};
  }
  return {};
}

/// for ( expression{opt} ; expression{opt} ; expression{opt} ) statement
/// for ( declaration expression{opt} ; expression{opt} ) statement
std::optional<Syntax::Stmt> Parser::ParseForStmt() {
  Expect(tok::kw_for);
  Expect(tok::l_paren);
  auto blockItem = ParseBlockItem();
  if (!blockItem) return {};
  std::unique_ptr<Syntax::Expr> control = nullptr;
  if (std::holds_alternative<Syntax::Declaration>(*blockItem) ||
      !Peek(tok::semi)) {
    auto expr = ParseExpr();
    Expect(tok::semi);
    if (expr) {
      control = std::make_unique<Syntax::Expr>(std::move(*expr));
    }
  } else {
    Expect(tok::semi);
  }

  std::unique_ptr<Syntax::Expr> post = nullptr;
  if (Peek(tok::r_paren)) {
    ConsumeAny();
  } else {
    auto expr = ParseExpr();
    Expect(tok::r_paren);
    if (expr) {
      post = std::make_unique<Syntax::Expr>(std::move(*expr));
    }
  }

  auto stmt = ParseStmt();
  if (auto declaration =
          std::get_if<Syntax::Declaration>(&*blockItem)) {
    if (stmt && declaration) {
      return Syntax::Stmt(Syntax::ForStmt(
          std::make_unique<Syntax::Stmt>(std::move(*stmt)),
          std::make_unique<Syntax::Declaration>(std::move(*declaration)),
          std::move(control), std::move(post)));
    }
  } else if (auto expr = std::get_if<Syntax::ExprStmt>(
                 &std::get<Syntax::Stmt>(*blockItem))) {
    if (stmt && expr) {
      return Syntax::Stmt(Syntax::ForStmt(
          std::make_unique<Syntax::Stmt>(std::move(*stmt)),
          expr->moveOptionalExpr(), std::move(control), std::move(post)));
    }
  }

  return {};
}

/// break;
std::optional<Syntax::Stmt> Parser::ParseBreakStmt() {
  Expect(tok::kw_break);
  Expect(tok::semi);
  return Syntax::Stmt{Syntax::BreakStmt()};
}

/// continue;
std::optional<Syntax::Stmt> Parser::ParseContinueStmt() {
  Expect(tok::kw_continue);
  Expect(tok::semi);
  return Syntax::Stmt{Syntax::ContinueStmt()};
}

/// return expr{opt};
std::optional<Syntax::Stmt> Parser::ParseReturnStmt() {
  Expect(tok::kw_return);
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Syntax::Stmt{Syntax::ReturnStmt()};
  }
  auto expr = ParseExpr();
  Expect(tok::semi);
  if (expr) {
    return Syntax::Stmt{
        Syntax::ReturnStmt(std::make_unique<Syntax::Expr>(std::move(*expr)))};
  }
  return {};
}

/// expr;
std::optional<Syntax::Stmt> Parser::ParseExprStmt() {
  if (Peek(tok::semi)) {
    ConsumeAny();
    return Syntax::Stmt(Syntax::ExprStmt());
  }
  auto expr = ParseExpr();
  Expect(tok::semi);
  if (expr) {
    return Syntax::Stmt(
        Syntax::ExprStmt(std::make_unique<Syntax::Expr>(std::move(*expr))));
  }
  return {};
}

/// switch ( expression ) statement
std::optional<Syntax::Stmt> Parser::ParseSwitchStmt() {
  Expect(tok::kw_switch);
  Expect(tok::l_paren);
  auto expr = ParseExpr();
  Expect(tok::r_paren);
  auto stmt = ParseStmt();
  if (expr && stmt) {
    return Syntax::Stmt(Syntax::SwitchStmt(
        std::move(*expr), std::make_unique<Syntax::Stmt>(std::move(*stmt))));
  }
  return {};
}

/// case constantExpr: stmt
std::optional<Syntax::Stmt> Parser::ParseCaseStmt() {
  Expect(tok::kw_case);
  auto expr = ParseConditionalExpr();
  Expect(tok::colon);
  auto stmt = ParseStmt();
  if (expr && stmt) {
    return Syntax::Stmt(Syntax::CaseStmt(
        std::move(*expr), std::make_unique<Syntax::Stmt>(std::move(*stmt))));
  }
  return {};
}

/// default: stmt
std::optional<Syntax::Stmt> Parser::ParseDefaultStmt() {
  Expect(tok::kw_default);
  Expect(tok::colon);
  auto stmt = ParseStmt();
  if (stmt) {
    return Syntax::Stmt(
        Syntax::DefaultStmt(std::make_unique<Syntax::Stmt>(std::move(*stmt))));
  }
  return {};
}

/// goto identifier;
std::optional<Syntax::Stmt> Parser::ParseGotoStmt() {
  Expect(tok::kw_goto);
  auto name = mTokCursor->getRepresentation();
  Expect(tok::identifier);
  Expect(tok::semi);
  return Syntax::Stmt(Syntax::GotoStmt(name));
}

/**
 expression:
    assignment-expression
    expression , assignment-expression
 */
std::optional<Syntax::Expr> Parser::ParseExpr() {
  std::vector<Syntax::AssignExpr> expressions;
  auto assignment = ParseAssignExpr();
  if (assignment) {
    expressions.push_back(std::move(*assignment));
  }

  while (Peek(tok::comma)) {
    ConsumeAny();
    assignment = ParseAssignExpr();
    if (assignment)
      expressions.push_back(std::move(*assignment));
  }
  return Syntax::Expr(std::move(expressions));
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
std::optional<Syntax::AssignExpr> Parser::ParseAssignExpr() {
  auto result = ParseConditionalExpr();
  std::vector<std::pair<Syntax::AssignExpr::AssignmentOperator, Syntax::ConditionalExpr>> list;
  while (IsAssignment(mTokCursor->getTokenKind())) {
    auto token = mTokCursor;
    ConsumeAny();
    auto assignmentOperator = [token]() -> Syntax::AssignExpr::AssignmentOperator{
      switch (token->getTokenKind()){
      case tok::equal:
        return Syntax::AssignExpr::AssignmentOperator::Assign;
      case tok::plus_equal:
        return Syntax::AssignExpr::AssignmentOperator::PlusAssign;
      case tok::minus_equal:
        return Syntax::AssignExpr::AssignmentOperator::MinusAssign;
      case tok::slash_equal:
        return Syntax::AssignExpr::AssignmentOperator::DivideAssign;
      case tok::star_equal:
        return Syntax::AssignExpr::AssignmentOperator::MultiplyAssign;
      case tok::percent_equal:
        return Syntax::AssignExpr::AssignmentOperator::ModuloAssign;
      case tok::less_less_equal:
        return Syntax::AssignExpr::AssignmentOperator::LeftShiftAssign;
      case tok::greater_greater_equal:
        return Syntax::AssignExpr::AssignmentOperator::RightShiftAssign;
      case tok::amp_equal:
        return Syntax::AssignExpr::AssignmentOperator::BitAndAssign;
      case tok::pipe_equal:
        return Syntax::AssignExpr::AssignmentOperator::BitOrAssign;
      case tok::caret_equal:
        return Syntax::AssignExpr::AssignmentOperator::BitXorAssign;
      default: return Syntax::AssignExpr::AssignmentOperator::Assign;
      }
    };
    auto conditional = ParseConditionalExpr();
    if (conditional)
      list.push_back({assignmentOperator(), std::move(*conditional)});
  }
  if (result) {
    return Syntax::AssignExpr(std::move(*result), std::move(list));
  }
  return {};
}

/**
 * conditional-expression:
 *      logical-OR-expression
 *      logical-OR-expression ? expression : conditional-expression
 */
std::optional<Syntax::ConditionalExpr> Parser::ParseConditionalExpr() {
  auto logOrExpr = ParseLogOrExpr();
  if (Peek(tok::question)) {
    ConsumeAny();
    auto expr = ParseExpr();
    Expect(tok::colon);
    auto optionalConditional = ParseConditionalExpr();
    if (logOrExpr && expr && optionalConditional) {
      return Syntax::ConditionalExpr(
          std::move(*logOrExpr),
          std::make_unique<Syntax::Expr>(std::move(*expr)),
          std::make_unique<Syntax::ConditionalExpr>(
              std::move(*optionalConditional)));
    }
    return {};
  }
  if (logOrExpr)
    return Syntax::ConditionalExpr(std::move(*logOrExpr));
  return {};
}

/**
 * logical-OR-expression:
 *      logical-AND-expression
 *      logical-OR-expression || logical-AND-expression
 */
std::optional<Syntax::LogOrExpr> Parser::ParseLogOrExpr() {
  auto expr = ParseLogAndExpr();
  std::vector<Syntax::LogAndExpr> logAndExprArr;
  while (Peek(tok::pipe_pipe)) {
    ConsumeAny();
    auto logAndExpr = ParseLogAndExpr();
    if (logAndExpr)
      logAndExprArr.push_back(std::move(*logAndExpr));
  }
  if (expr) {
    return Syntax::LogOrExpr(std::move(*expr), std::move(logAndExprArr));
  }
  return {};
}

/**
 * logical-AND-expression:
 *      inclusive-OR-expression
 *      logical-AND-expression && inclusive-OR-expression
 */
std::optional<Syntax::LogAndExpr> Parser::ParseLogAndExpr() {
  auto expr = ParseBitOrExpr();
  std::vector<Syntax::BitOrExpr> bitOrExprArr;
  while (Peek(tok::amp_amp)) {
    ConsumeAny();
    auto bitOrExpr = ParseBitOrExpr();
    if (bitOrExpr)
      bitOrExprArr.push_back(std::move(*bitOrExpr));
  }
  if (expr) {
    return Syntax::LogAndExpr(std::move(*expr), std::move(bitOrExprArr));
  }
  return {};
}

/**
 * inclusive-OR-expression:
 *      exclusive-OR-expression
 *      inclusive-OR-expression | exclusive-OR-expression
 */
std::optional<Syntax::BitOrExpr> Parser::ParseBitOrExpr() {
  auto expr = ParseBitXorExpr();
  std::vector<Syntax::BitXorExpr> bitXorExprArr;
  while (Peek(tok::pipe)) {
    ConsumeAny();
    auto newXor = ParseBitXorExpr();
    if (newXor) {
      bitXorExprArr.push_back(std::move(*newXor));
    }
  }
  if (expr) {
    return Syntax::BitOrExpr(std::move(*expr), std::move(bitXorExprArr));
  }
  return {};
}

std::optional<Syntax::BitXorExpr> Parser::ParseBitXorExpr() {
  auto expr = ParseBitAndExpr();
  std::vector<Syntax::BitAndExpr> bitAndExprArr;
  while (Peek(tok::caret)) {
    ConsumeAny();
    auto newAnd = ParseBitAndExpr();
    if (newAnd) {
      bitAndExprArr.push_back(std::move(*newAnd));
    }
  }
  if (expr) {
    return Syntax::BitXorExpr(std::move(*expr), std::move(bitAndExprArr));
  }
  return {};
}

/**
 * AND-expression:
 *      equality-expression
 *      AND-expression & equality-expression
 */
std::optional<Syntax::BitAndExpr> Parser::ParseBitAndExpr() {
  auto expr = ParseEqualExpr();
  std::vector<Syntax::EqualExpr> equalExprArr;
  while (Peek(tok::amp)) {
    ConsumeAny();
    auto newEqual = ParseEqualExpr();
    if (newEqual) {
      equalExprArr.push_back(std::move(*newEqual));
    }
  }
  if (expr) {
    return Syntax::BitAndExpr(std::move(*expr), std::move(equalExprArr));
  }
  return std::nullopt;
}

/**
 * equality-expression:
 *      relational-expression
 *      equality-expression == relational-expression
 *      equality-expression != relational-expression
 */
std::optional<Syntax::EqualExpr> Parser::ParseEqualExpr() {
  auto result = ParseRelationalExpr();
  std::vector<std::pair<Syntax::EqualExpr::BinaryOperator, Syntax::RelationalExpr>>
      relationalExpressions;
  while (Peek(tok::equal_equal) || Peek(tok::exclaim_equal)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    Syntax::EqualExpr::BinaryOperator binaryOperator;
    if (tokenType == tok::equal_equal) {
      binaryOperator = Syntax::EqualExpr::BinaryOperator::Equal;
    }else {
      binaryOperator = Syntax::EqualExpr::BinaryOperator::NotEqual;
    }
    ConsumeAny();
    auto newRelational = ParseRelationalExpr();
    if (newRelational) {
      relationalExpressions.emplace_back(binaryOperator, std::move(*newRelational));
    }
  }
  if (result) {
    return Syntax::EqualExpr(std::move(*result),
                             std::move(relationalExpressions));
  }
  return std::nullopt;
}

/**
 * relational-expression:
 *      shift-expression
 *      relational-expression < shift-expression
 *      relational-expression > shift-expression
 *      relational-expression <= shift-expression
 *      relational-expression >= shift-expression
 */
std::optional<Syntax::RelationalExpr> Parser::ParseRelationalExpr() {
  auto result = ParseShiftExpr();
  std::vector<std::pair<Syntax::RelationalExpr::BinaryOperator, Syntax::ShiftExpr>> relationalExprArr;
  while (Peek(tok::less) || Peek(tok::less_equal) ||
         Peek(tok::greater) || Peek(tok::greater_equal)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto binaryOperator = [tokenType]()->Syntax::RelationalExpr::BinaryOperator {
      switch (tokenType) {
        case tok::less: return Syntax::RelationalExpr::BinaryOperator::LessThan;
        case tok::less_equal: return Syntax::RelationalExpr::BinaryOperator::LessThanOrEqual;
        case tok::greater: return Syntax::RelationalExpr::BinaryOperator::GreaterThan;
        case tok::greater_equal: return Syntax::RelationalExpr::BinaryOperator::GreaterThanOrEqual;
        default:
          LCC_UNREACHABLE;
      }
    };
    ConsumeAny();
    auto newShift = ParseShiftExpr();
    if (newShift) {
      relationalExprArr.emplace_back(binaryOperator(), std::move(*newShift));
    }
  }
  if (result) {
    return Syntax::RelationalExpr(std::move(*result),
                                  std::move(relationalExprArr));
  }
  return {};
}


/**
 * shift-expression:
 *      additive-expression
 *      shift-expression << additive-expression
 *      shift-expression >> additive-expression
 */
std::optional<Syntax::ShiftExpr> Parser::ParseShiftExpr() {
  auto result = ParseAdditiveExpr();
  std::vector<std::pair<Syntax::ShiftExpr::BinaryOperator, Syntax::AdditiveExpr>> additiveExprArr;
  while (Peek(tok::less_less) || Peek(tok::greater_greater)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    Syntax::ShiftExpr::BinaryOperator binaryOperator;
    if (tokenType == tok::less_less) {
      binaryOperator = Syntax::ShiftExpr::BinaryOperator::Left;
    }else {
      binaryOperator = Syntax::ShiftExpr::BinaryOperator::Right;
    }
    ConsumeAny();
    auto newAdd = ParseAdditiveExpr();
    if (newAdd) {
      additiveExprArr.emplace_back(binaryOperator, std::move(*newAdd));
    }
  }
  if (result) {
    return Syntax::ShiftExpr(std::move(*result), std::move(additiveExprArr));
  }
  return {};
}


/**
 * additive-expression:
 * multiplicative-expression
 * additive-expression + multiplicative-expression
 * additive-expression - multiplicative-expression
 */
std::optional<Syntax::AdditiveExpr> Parser::ParseAdditiveExpr() {
  auto result = ParseMultiExpr();
  std::vector<std::pair<Syntax::AdditiveExpr::BinaryOperator, Syntax::MultiExpr>> multiExprArr;
  while (Peek(tok::plus) || Peek(tok::minus)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    Syntax::AdditiveExpr::BinaryOperator binaryOperator;
    if (tokenType == tok::plus) {
      binaryOperator = Syntax::AdditiveExpr::BinaryOperator::Plus;
    }else {
      binaryOperator = Syntax::AdditiveExpr::BinaryOperator::Minus;
    }
    ConsumeAny();
    auto newMul = ParseMultiExpr();
    if (newMul) {
      multiExprArr.emplace_back(binaryOperator, std::move(*newMul));
    }
  }
  if (result) {
    return Syntax::AdditiveExpr(std::move(*result), std::move(multiExprArr));
  }
  return {};
}

/**
 * multiplicative-expression:
 *  cast-expression
 *  multiplicative-expression * cast-expression
 *  multiplicative-expression / cast-expression
 *  multiplicative-expression % cast-expression
 */
std::optional<Syntax::MultiExpr> Parser::ParseMultiExpr() {
  auto result = ParseCastExpr();
  std::vector<std::pair<Syntax::MultiExpr::BinaryOperator, Syntax::CastExpr>> castExprArr;
  while (Peek(tok::star) || Peek(tok::slash) || Peek(tok::percent)) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto binaryOperator = [tokenType]()->Syntax::MultiExpr::BinaryOperator {
      switch (tokenType) {
      case tok::star: return Syntax::MultiExpr::BinaryOperator::Multiply;
      case tok::slash: return Syntax::MultiExpr::BinaryOperator::Divide;
      case tok::percent: return Syntax::MultiExpr::BinaryOperator::Modulo;
      default:
        LCC_UNREACHABLE;
      }
    };
    ConsumeAny();
    auto newCast = ParseCastExpr();
    if (newCast) {
      castExprArr.emplace_back(binaryOperator(), std::move(*newCast));
    }
  }
  if (result) {
    return Syntax::MultiExpr(std::move(*result), std::move(castExprArr));
  }
  return std::nullopt;
}

/**
 * type-name:
 *  specifier-qualifier-list abstract-declarator{opt}
 */
std::optional<Syntax::TypeName> Parser::ParseTypeName() {
  auto begin = mTokCursor;
  auto specs = ParseDeclarationSpecifiers();
  if (specs.getStorageClassSpecifiers().size() > 0) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_type_name_appear_storage_class);
  }
  if (specs.getTypeSpecifiers().size() == 0 && specs.getTypeQualifiers().size() == 0) {
    DiagReport(Diag, begin->getSMLoc(), diag::err_parse_expect_type_specifier_or_qualifier);
  }

  if (IsFirstInAbstractDeclarator()) {
    auto abstractDec = ParseAbstractDeclarator();
    if (abstractDec) {
      return Syntax::TypeName(std::move(specs),
                              std::make_unique<Syntax::AbstractDeclarator>(
                                  std::move(*abstractDec)));
    }
    return std::nullopt;
  }
  return Syntax::TypeName(std::move(specs), nullptr);
}

/**
 * cast-expression:
 *      unary-expression
 *      ( type-name ) cast-expression
 *
 * (unsigned char)(h ? h->height + 1 : 0);
 */
std::optional<Syntax::CastExpr> Parser::ParseCastExpr() {
  auto start = mTokCursor;
  // cast-expression: unary-expression
  if (!Peek(tok::l_paren)) {
    auto unary = ParseUnaryExpr();
    if (unary) {
      return Syntax::CastExpr(std::move(*unary));
    }
    return {};
  }

  Expect(tok::l_paren);

  if (!IsFirstInTypeName()) {
    // cast-expression: unary-expression
    mTokCursor = start;
    auto unary = ParseUnaryExpr();
    if (unary) {
      return Syntax::CastExpr(std::move(*unary));
    }
  }else {
    // cast-expression: ( type-name ) cast-expression
    auto typeName = ParseTypeName();
    Expect(tok::r_paren);
    auto cast = ParseCastExpr();
    if (typeName && cast) {
      return Syntax::CastExpr(
          std::pair{std::move(*typeName),
                    std::make_unique<Syntax::CastExpr>(std::move(*cast))});
    }
  }
  return std::nullopt;
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
std::optional<Syntax::UnaryExpr> Parser::ParseUnaryExpr() {
  if (Peek(tok::kw_sizeof)) {
    ConsumeAny();
    if (Peek(tok::l_paren)) {
      ConsumeAny();
      auto type = ParseTypeName();
      Expect(tok::r_paren);
      if (type) {
        return Syntax::UnaryExpr(Syntax::UnaryExprSizeOf(
            std::make_unique<Syntax::TypeName>(std::move(*type))));
      }
    } else {
      auto unary = ParseUnaryExpr();
      if (unary) {
        return Syntax::UnaryExpr(Syntax::UnaryExprSizeOf(
            std::make_unique<Syntax::UnaryExpr>(std::move(*unary))));
      }
    }
  } else if (IsUnaryOp(mTokCursor->getTokenKind())) {
    tok::TokenKind tokenType = mTokCursor->getTokenKind();
    auto unaryOperator = [tokenType]()->Syntax::UnaryExprUnaryOperator::UnaryOperator{
      switch (tokenType) {
      case tok::amp: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Ampersand;
      case tok::star: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Asterisk;
      case tok::plus: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Plus;
      case tok::minus: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Minus;
      case tok::tilde: return Syntax::UnaryExprUnaryOperator::UnaryOperator::BitNot;
      case tok::exclaim: return Syntax::UnaryExprUnaryOperator::UnaryOperator::LogicalNot;
      case tok::plus_plus: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Increment;
      case tok::minus_minus: return Syntax::UnaryExprUnaryOperator::UnaryOperator::Decrement;
      default:
        LCC_UNREACHABLE;
      }
    };
    ConsumeAny();
    auto castExpr = ParseCastExpr();
    if (!castExpr) {
      return Syntax::UnaryExpr(Syntax::UnaryExprUnaryOperator(
          unaryOperator(), std::make_unique<Syntax::CastExpr>(std::move(*castExpr))));
    }
  } else {
    auto postFix = ParsePostFixExpr();
    if (postFix) {
      return Syntax::UnaryExpr(Syntax::UnaryExprPostFixExpr(std::move(*postFix)));
    }
  }
  return {};
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

void Parser::ParsePostFixExprSuffix(std::unique_ptr<Syntax::PostFixExpr>& current) {
  while (IsPostFixExpr(mTokCursor->getTokenKind())) {
    auto tokType = mTokCursor->getTokenKind();
    if (tokType == tok::l_paren) {
      ConsumeAny();
      std::vector<std::unique_ptr<Syntax::AssignExpr>> params;
      if (!Peek(tok::r_paren)) {
        auto assignment = ParseAssignExpr();
        if (assignment) {
          params.push_back(std::make_unique<Syntax::AssignExpr>(std::move(*assignment)));
        }
      }
      while (!Peek(tok::r_paren)) {
        Expect(tok::comma);
        auto assignment = ParseAssignExpr();
        if (assignment) {
          params.push_back(std::make_unique<Syntax::AssignExpr>(std::move(*assignment)));
        }
      }
      Expect(tok::r_paren);
      if (current) {
        current = std::make_unique<Syntax::PostFixExpr>(
            Syntax::PostFixExprFuncCall(std::move(current), std::move(params)));
      }
    } else if (tokType == tok::l_square) {
      ConsumeAny();
      auto expr = ParseExpr();
      Expect(tok::r_square);
      if (current && expr) {
        current = std::make_unique<Syntax::PostFixExpr>(
            Syntax::PostFixExprSubscript(std::move(current), std::move(*expr)));
      }
    } else if (tokType == tok::plus_plus) {
      ConsumeAny();
      current = std::make_unique<Syntax::PostFixExpr>(
          Syntax::PostFixExprIncrement(std::move(current)));
    } else if (tokType == tok::minus_minus) {
      ConsumeAny();
      current = std::make_unique<Syntax::PostFixExpr>(
          Syntax::PostFixExprDecrement(std::move(current)));
    } else if (tokType == tok::period) {
      ConsumeAny();
      auto identifier = mTokCursor->getRepresentation();
      Expect(tok::identifier);
      current = std::make_unique<Syntax::PostFixExpr>(
          Syntax::PostFixExprDot(std::move(current), identifier));
    } else if (tokType == tok::arrow) {
      ConsumeAny();
      auto name = mTokCursor->getRepresentation();
      Expect(tok::identifier);
      current = std::make_unique<Syntax::PostFixExpr>(
          Syntax::PostFixExprArrow(std::move(current), name));
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
std::optional<Syntax::PostFixExpr> Parser::ParsePostFixExpr() {
  std::unique_ptr<Syntax::PostFixExpr> current = nullptr;
  std::optional<Syntax::PrimaryExpr> newPrimary;
  if (Peek(tok::identifier)) {
    auto name = mTokCursor->getRepresentation();
    ConsumeAny();
    newPrimary = Syntax::PrimaryExpr(Syntax::PrimaryExprIdent(name));
  }else if (Peek(tok::char_constant) || Peek(tok::numeric_constant) || Peek(tok::string_literal)) {
    auto value = std::visit([](auto &&value)->typename Syntax::PrimaryExprConstant::Variant {
      using T = std::decay_t<decltype(value)>;
      if constexpr (std::is_constructible_v<typename Syntax::PrimaryExprConstant::Variant, T>) {
        return std::forward<decltype(value)>(value);
      }else {
        LCC_UNREACHABLE;
      }
    }, mTokCursor->getValue());
    ConsumeAny();
    newPrimary = Syntax::PrimaryExpr(Syntax::PrimaryExprConstant(value));
  }else if (Peek(tok::l_paren)) {
    ConsumeAny();
    if (IsFirstInTypeName()) {
      auto type = ParseTypeName();
      Expect(tok::r_paren);
      Expect(tok::l_brace);
      auto initializer = ParseInitializerList();
      if (Peek(tok::comma)) {
        ConsumeAny();
      }
      Expect(tok::r_brace);
      if (type && initializer) {
        current = std::make_unique<Syntax::PostFixExpr>(
            Syntax::PostFixExprTypeInitializer(std::move(*type),
                                               std::move(*initializer)));
      }
    }else {
      auto expr = ParseExpr();
      Expect(tok::r_paren);
      if (expr) {
        newPrimary = Syntax::PrimaryExpr(
            Syntax::PrimaryExprParentheses(std::move(*expr)));
      }
    }
  }else {
    DiagReport(Diag, mTokCursor->getSMLoc(), diag::err_parse_expect_n, "primary expr or ( type-name )");
  }

  if (newPrimary) {
    current = std::make_unique<Syntax::PostFixExpr>(
        Syntax::PostFixExprPrimaryExpr(std::move(*newPrimary)));
  }
  ParsePostFixExprSuffix(current);
  if (!current) {
    return {};
  }
  return std::move(*current);
}

bool Parser::IsAssignment(tok::TokenKind type) {
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