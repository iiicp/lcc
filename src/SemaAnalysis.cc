/***********************************
 * File:     SemaAnalysis.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/9
 ***********************************/

#include "SemaAnalysis.h"
#include <bitset>
#include <unordered_map>
namespace lcc {

using Sema::PrimitiveType;

SemaAnalysis::SemaAnalysis(const std::vector<Token> &tokens, DiagnosticEngine &diag)
    : mTokens(tokens), Diag(diag) {}

Sema::TranslationUnit SemaAnalysis::Analyse(const Syntax::TranslationUnit &node) {
  return visit(node);
}

Sema::TranslationUnit SemaAnalysis::visit(const Syntax::TranslationUnit &node) {
  std::vector<Sema::TranslationUnit::Variant> globals;
  for (auto &iter : node.getGlobals()) {
    auto result = std::visit(overload{[&](const Syntax::FunctionDefinition &functionDefinition)
                     -> std::vector<Sema::TranslationUnit::Variant> {
                    return visit(functionDefinition);
                 },
                 [&](const Syntax::Declaration &declaration)
                     -> std::vector<Sema::TranslationUnit::Variant> {
                    return visit(declaration);
                 }}, iter);
    globals.insert(globals.end(), std::move_iterator(result.begin()), std::move_iterator(result.end()));
//    globals = std::move(result);
  }
  return Sema::TranslationUnit(std::move(globals));
}

std::vector<Sema::TranslationUnit::Variant> SemaAnalysis::visit(const Syntax::FunctionDefinition &node) {
  std::vector<Sema::TranslationUnit::Variant> result;
  const Syntax::StorageClassSpecifier* storageClassSpecifier = nullptr;
  for (auto &iter : node.getDeclarationSpecifiers().getStorageClassSpecifiers()) {
    if (iter.getSpecifier() != Syntax::StorageClassSpecifier::Extern &&
        iter.getSpecifier() != Syntax::StorageClassSpecifier::Static) {
      DiagReport(Diag, iter.getBegin()->getSMLoc(), diag::err_sema_only_static_or_extern_allowed_in_function_definition);
      continue;
    }
    storageClassSpecifier = &iter;
  }
  return result;
}

std::vector<Sema::TranslationUnit::Variant> SemaAnalysis::visit(const Syntax::Declaration &node) {
  return {};
}

Sema::Type SemaAnalysis::declarationSpecifierToType(const Syntax::DeclarationSpecifiers &declarationSpecifiers) {
  bool isConst = false, isVolatile = false;
  for (auto &iter : declarationSpecifiers.getTypeQualifiers()) {
    switch (iter.getQualifier()) {
    case Syntax::TypeQualifier::Const: isConst = true; break;
    case Syntax::TypeQualifier::Volatile: isVolatile = true; break;
    case Syntax::TypeQualifier::Restrict:
      /// todo
      break;
    }
  }
  if (declarationSpecifiers.getTypeSpecifiers().empty()) {
    DiagReport(Diag, declarationSpecifiers.getBegin()->getSMLoc(), diag::err_sema_at_least_one_type_specifier_required);
    return Sema::Type{};
  }

  const auto &typeSpecs = declarationSpecifiers.getTypeSpecifiers();
  if (std::holds_alternative<Syntax::TypeSpecifier::PrimitiveTypeSpecifier>(typeSpecs[0].getVariant())) {
    /// primitive type
    return primitiveTypeSpecifiersToType(isConst, isVolatile, typeSpecs);
  }else if (auto *name = std::get_if<std::string_view>(&typeSpecs[0].getVariant())) {
    if (typeSpecs.size() != 1) {
      DiagReport(Diag, typeSpecs[1].getBegin()->getSMLoc(), diag::err_sema_expected_no_further_type_specifiers_after, "typedef typename");
    }
    /// todo typedef typename
  }else if (auto *structOrUnionPtr = std::get_if<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(&typeSpecs[0].getVariant())) {
    /// todo struct type or union type
  }else {
    LCC_ASSERT(std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpecs[0].getVariant()));
    if (typeSpecs.size() != 1) {
      DiagReport(Diag, typeSpecs[1].getBegin()->getSMLoc(), diag::err_sema_expected_no_further_type_specifiers_after, "enum specifier");
    }
    /// todo enum type
  }
}

Sema::Type SemaAnalysis::declaratorToType(Sema::Type type, const Syntax::AbstractDeclarator* declarator) {

}
Sema::Type SemaAnalysis::declaratorToType(Sema::Type type, const Syntax::Declarator* declarator) {

}

Sema::Type SemaAnalysis::primitiveTypeSpecifiersToType(bool isConst, bool isVolatile, const std::vector<Syntax::TypeSpecifier> &typeSpecs) {
  using SyntaxPrimTypeSpec = Syntax::TypeSpecifier::PrimitiveTypeSpecifier;
  LCC_ASSERT(std::holds_alternative<SyntaxPrimTypeSpec>(typeSpecs[0].getVariant()));

  auto displaySpecifierError = [this](const std::string desc, const Syntax::TypeSpecifier& typeSpec) {
    if (std::holds_alternative<std::string_view>(typeSpec.getVariant())) {
      DiagReport(Diag, typeSpec.getBegin()->getSMLoc(), diag::err_sema_cannot_combine_n_with_n, desc, "typename");
    }else if (auto *structOrUnionPtr = std::get_if<std::unique_ptr<Syntax::StructOrUnionSpecifier>>(&typeSpec.getVariant())) {
      DiagReport(Diag, typeSpec.getBegin()->getSMLoc(), diag::err_sema_cannot_combine_n_with_n, desc, "struct or union");
    }else if (std::holds_alternative<std::unique_ptr<Syntax::EnumSpecifier>>(typeSpec.getVariant())) {
      DiagReport(Diag, typeSpec.getBegin()->getSMLoc(), diag::err_sema_cannot_combine_n_with_n, desc,"enum");
    }else {
      tok::TokenKind tokenKind;
      switch (std::get<SyntaxPrimTypeSpec>(typeSpec.getVariant())) {
      case SyntaxPrimTypeSpec::Void: tokenKind = tok::kw_void; break;
      case SyntaxPrimTypeSpec::Char: tokenKind = tok::kw_char; break;
      case SyntaxPrimTypeSpec::Short:tokenKind = tok::kw_short;break;
      case SyntaxPrimTypeSpec::Int:tokenKind = tok::kw_int;break;
      case SyntaxPrimTypeSpec::Long:tokenKind = tok::kw_long;break;
      case SyntaxPrimTypeSpec::Float:tokenKind = tok::kw_float;break;
      case SyntaxPrimTypeSpec::Double:tokenKind = tok::kw_double;break;
      case SyntaxPrimTypeSpec::Signed:tokenKind = tok::kw_signed;break;
      case SyntaxPrimTypeSpec::Unsigned:tokenKind = tok::kw_unsigned;break;
      case SyntaxPrimTypeSpec::Bool:tokenKind = tok::kw__Bool;break;
      }
      DiagReport(Diag, typeSpec.getBegin()->getSMLoc(), diag::err_sema_cannot_combine_n_with_n, desc, getTokenName(tokenKind));
    }
  };
  constexpr auto bitCount = [](std::size_t value) {
    std::size_t i = 0;
    for (; value != 0; value >>= 1, i++)
      ;
    return i;
  };

  using BitSet = std::bitset<bitCount(SyntaxPrimTypeSpec::Bool)>;
  static const auto hashTable = []() -> std::unordered_map<BitSet, std::pair<BitSet, PrimitiveType::Kind>> {
    using tpl = std::tuple<std::underlying_type_t<SyntaxPrimTypeSpec>, std::underlying_type_t<SyntaxPrimTypeSpec>, PrimitiveType::Kind>;
    std::vector<tpl> temp = {
        {
            SyntaxPrimTypeSpec::Void,
            0,
            PrimitiveType::Kind::Void
        },
        {
            SyntaxPrimTypeSpec::Char,
            SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::Char
        },
        {
            SyntaxPrimTypeSpec::Char + SyntaxPrimTypeSpec::Signed,
            0,
            PrimitiveType::Kind::Char
        },
        {
            SyntaxPrimTypeSpec::Char + SyntaxPrimTypeSpec::Unsigned,
            0,
            PrimitiveType::Kind::UnsignedChar
        },
        {
            SyntaxPrimTypeSpec::Float,
            0,
            PrimitiveType::Kind::Float
        },
        {
            SyntaxPrimTypeSpec::Bool,
            0,
            PrimitiveType::Kind::Bool
        },
        {
            SyntaxPrimTypeSpec::Double,
            SyntaxPrimTypeSpec::Long,
            PrimitiveType::Kind::Double
        },
        {
            SyntaxPrimTypeSpec::Double + SyntaxPrimTypeSpec::Long,
            0,
            PrimitiveType::Kind::LongDouble
        },
        {
            SyntaxPrimTypeSpec::Int,
            SyntaxPrimTypeSpec::Long | SyntaxPrimTypeSpec::Short | SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::Int
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long,
            SyntaxPrimTypeSpec::Long | SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::Long
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long,
            SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::LongLong
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Signed,
            0,
            PrimitiveType::Kind::LongLong
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Unsigned,
            0,
            PrimitiveType::Kind::UnsignedLongLong
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Long,
            PrimitiveType::Kind::Long
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Long,
            PrimitiveType::Kind::UnsignedLong
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Short,
            SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::Short
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Short + SyntaxPrimTypeSpec::Signed,
            0,
            PrimitiveType::Kind::Short
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Short + SyntaxPrimTypeSpec::Unsigned,
            0,
            PrimitiveType::Kind::UnsignedShort
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Long | SyntaxPrimTypeSpec::Short,
            PrimitiveType::Kind::Int
        },
        {
            SyntaxPrimTypeSpec::Int + SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Long | SyntaxPrimTypeSpec::Short,
            PrimitiveType::Kind::UnsignedInt
        },
        {
            SyntaxPrimTypeSpec::Short,
            SyntaxPrimTypeSpec::Int | SyntaxPrimTypeSpec::Signed | SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::Short
        },
        {
            SyntaxPrimTypeSpec::Short + SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Int,
            PrimitiveType::Kind::Short
        },
        {
            SyntaxPrimTypeSpec::Short + SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Int,
            PrimitiveType::Kind::UnsignedShort
        },
        {
            SyntaxPrimTypeSpec::Long,
            SyntaxPrimTypeSpec::Signed|SyntaxPrimTypeSpec::Unsigned| SyntaxPrimTypeSpec::Long|SyntaxPrimTypeSpec::Int|SyntaxPrimTypeSpec::Double,
            PrimitiveType::Kind::Long
        },
        {
            SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Long|SyntaxPrimTypeSpec::Int, PrimitiveType::Kind::Long
        },
        {
            SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Long|SyntaxPrimTypeSpec::Int,
            PrimitiveType::Kind::UnsignedLong
        },
        {
            SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long,
            SyntaxPrimTypeSpec::Signed|SyntaxPrimTypeSpec::Unsigned,
            PrimitiveType::Kind::LongLong
        },
        {
            SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Int,
            PrimitiveType::Kind::LongLong
        },
        {
            SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Long + SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Int,
            PrimitiveType::Kind::UnsignedLongLong
        },
        {
            SyntaxPrimTypeSpec::Signed,
            SyntaxPrimTypeSpec::Char|SyntaxPrimTypeSpec::Short|SyntaxPrimTypeSpec::Int|SyntaxPrimTypeSpec::Long,
            PrimitiveType::Kind::Int
        },
        {
            SyntaxPrimTypeSpec::Unsigned,
            SyntaxPrimTypeSpec::Char|SyntaxPrimTypeSpec::Short|SyntaxPrimTypeSpec::Int|SyntaxPrimTypeSpec::Long,
            PrimitiveType::Kind::UnsignedInt
        }
    };
    std::unordered_map<BitSet, std::pair<BitSet,Sema::PrimitiveType::Kind>> result;
    for (auto& [init, next, type] : temp)
    {
      auto [prev, inserted] = result.insert({BitSet(init),{BitSet(next),std::move(type)}});
      (void)prev;
      LCC_ASSERT(inserted);
    }
    return result;
  }();

  auto primKindToType = [isConst, isVolatile, this](PrimitiveType::Kind kind) -> Sema::Type {
    switch (kind)
    {
    case PrimitiveType::Kind::Char:
      return PrimitiveType::createChar(isConst, isVolatile);
    case PrimitiveType::Kind::UnsignedChar: return PrimitiveType::createUnsignedChar(isConst, isVolatile);
    case PrimitiveType::Kind::Bool: return PrimitiveType::createUnderlineBool(isConst, isVolatile);
    case PrimitiveType::Kind::Short:
      return PrimitiveType::createShort(isConst, isVolatile);
    case PrimitiveType::Kind::UnsignedShort:
      return PrimitiveType::createUnsignedShort(isConst, isVolatile);
    case PrimitiveType::Kind::Int:
      return PrimitiveType::createInt(isConst, isVolatile);
    case PrimitiveType::Kind::UnsignedInt:
      return PrimitiveType::createUnsignedInt(isConst, isVolatile);
    case PrimitiveType::Kind::Long:
      return PrimitiveType::createLong(isConst, isVolatile);
    case PrimitiveType::Kind::UnsignedLong:
      return PrimitiveType::createUnsignedLong(isConst, isVolatile);
    case PrimitiveType::Kind::LongLong:
      return PrimitiveType::createLongLong(isConst, isVolatile);
    case PrimitiveType::Kind::UnsignedLongLong:
      return PrimitiveType::createUnsignedLongLong(isConst, isVolatile);
    case PrimitiveType::Kind::Float: return PrimitiveType::createFloat(isConst, isVolatile);
    case PrimitiveType::Kind::Double:
      return PrimitiveType::createDouble(isConst, isVolatile);
    case PrimitiveType::Kind::LongDouble:
      return PrimitiveType::createLongDouble(isConst, isVolatile);
    case PrimitiveType::Kind::Void: return PrimitiveType::createVoid(isConst, isVolatile);
    }
    LCC_UNREACHABLE;
  };

  auto primTypeSpecToString = [](SyntaxPrimTypeSpec spec) -> std::string_view {
    switch (spec)
    {
    case Syntax::TypeSpecifier::Void: return "void";
    case Syntax::TypeSpecifier::Char: return "char";
    case Syntax::TypeSpecifier::Short: return "short";
    case Syntax::TypeSpecifier::Int: return "int";
    case Syntax::TypeSpecifier::Long: return "long";
    case Syntax::TypeSpecifier::Float: return "float";
    case Syntax::TypeSpecifier::Double: return "double";
    case Syntax::TypeSpecifier::Signed: return "signed";
    case Syntax::TypeSpecifier::Unsigned: return "unsigned";
    case Syntax::TypeSpecifier::Bool: return "_Bool";
    }
    LCC_UNREACHABLE;
  };

  std::string text = "'";
  text += primTypeSpecToString(std::get<SyntaxPrimTypeSpec>(typeSpecs[0].getVariant()));
  BitSet typeSpec(std::get<SyntaxPrimTypeSpec>(typeSpecs[0].getVariant()));
  for (int i = 1; i < typeSpecs.size(); ++i) {
    const auto &iter = typeSpecs[i];
    auto* nextTypeSpec = std::get_if<SyntaxPrimTypeSpec>(&iter.getVariant());
    if (!nextTypeSpec){
      displaySpecifierError(text + "'", iter);
      auto result = hashTable.find(typeSpec);
      LCC_ASSERT(result != hashTable.end());
      return primKindToType(result->second.second);
    }
    auto result = hashTable.find(typeSpec);
    LCC_ASSERT(result != hashTable.end());
    if ((result->second.first & BitSet(*nextTypeSpec)).any()) {
      typeSpec |= BitSet(*nextTypeSpec);
      text += " ";
      text += primTypeSpecToString(*nextTypeSpec);
      continue;
    }
    displaySpecifierError(text + "'", iter);
    return primKindToType(result->second.second);
  }
  auto result = hashTable.find(typeSpec);
  LCC_ASSERT(result != hashTable.end());
  return primKindToType(result->second.second);
}
}