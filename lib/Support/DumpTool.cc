/***********************************
 * File:     DumpTool.cc
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/2/5
 *
 * Sign:     enjoy life
 ***********************************/
#include "lcc/Support/DumpTool.h"
#include "lcc/Basic/Match.h"
#include "lcc/Basic/Util.h"
#include "lcc/Basic/ValueReset.h"
#include "llvm/Support/raw_ostream.h"
namespace lcc::dump {

static uint64_t LeftAlign = 1;

//void IncAlign() {
//  LeftAlign++;
//}
//
//void DecAlign() {
//  assert(LeftAlign > 1);
//  LeftAlign--;
//}


void Print(std::string_view content) {
  std::string ws(LeftAlign, '-');
  if (ws.length() >= 1) {
    ws[0] = '|';
  }
  llvm::outs() << ws << content << " ";
}

void Println(std::string_view content, bool color=true) {
  std::string ws(LeftAlign, '-');
  if (ws.length() >= 1) {
    ws[0] = '|';
  }
  if (color) {
    llvm::outs().changeColor(llvm::raw_ostream::GREEN) << ws << content;
    llvm::outs().resetColor() << "\n";
  }else {
    llvm::outs() << ws << content << "\n";
  }
}

void dumpTokens(const std::vector<lcc::Token> &tokens) {
  for (auto &tok : tokens) {
//    llvm::outs() << tok.getLine() << ", " << tok.getColumn() << ", " << tok.getRepresentation() << "\n";
    auto pair = tok.getLineAndColumn();
    llvm::outs() << pair.first << ", " << pair.second << ", " << tok.getRepresentation() << "\n";
  }
}

void dumpAst(const lcc::Syntax::TranslationUnit &unit) { visit(unit); }

void visit(const Syntax::TranslationUnit &unit) {
  Print("TranslationUnit");
  llvm::outs() << &unit << " " << unit.getGlobals().size() << "\n";
  for (auto &externalDecl : unit.getGlobals()) {
    match(
        externalDecl,
        [](const Syntax::Declaration &declaration) {
          ValueReset v(LeftAlign, LeftAlign + 1);
          visit(declaration);
        },
        [](const Syntax::FunctionDefinition &functionDefinition) {
          ValueReset v(LeftAlign, LeftAlign + 1);
          visit(functionDefinition);
        });
  }
}
void visit(const Syntax::Declaration &declaration) {
  Print("Declaration");
  llvm::outs() << &declaration << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(declaration.getDeclarationSpecifiers());
  for (auto &initDec : declaration.getInitDeclarators()) {
    visit(*initDec.declarator_);

    if (initDec.optionalInitializer_) {
      visit(*initDec.optionalInitializer_);
    }
  }
}
void visit(const Syntax::FunctionDefinition &functionDefinition) {
  Print("FunctionDefinition");
  llvm::outs() << &functionDefinition << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(functionDefinition.getDeclarationSpecifiers());
  visit(functionDefinition.getDeclarator());
  visit(functionDefinition.getCompoundStatement());
}
void visit(const Syntax::DeclSpec &declarationSpecifiers) {
  Print("DeclSpec");
  llvm::outs() << &declarationSpecifiers << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &storage : declarationSpecifiers.getStorageClassSpecifiers()) {
    visit(storage);
  }
  for (const auto &qualifier : declarationSpecifiers.getTypeQualifiers()) {
    visit(qualifier);
  }
  for (const auto &specifier : declarationSpecifiers.getTypeSpecs()) {
    visit(specifier);
  }
}
void visit(const Syntax::Declarator &declarator) {
  Print("Declarator");
  llvm::outs() << &declarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &point : declarator.getPointers()) {
    visit(point);
  }
  visit(declarator.getDirectDeclarator());
}

void visit(const Syntax::AbstractDeclarator &abstractDeclarator) {
  Print("AbstractDeclarator");
  llvm::outs() << &abstractDeclarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &point : abstractDeclarator.getPointers()) {
    visit(point);
  }
  if (abstractDeclarator.getDirectAbstractDeclarator()) {
    visit(*abstractDeclarator.getDirectAbstractDeclarator());
  }
}

void visit(const Syntax::Initializer &initializer) {
  Print("Initializer");
  llvm::outs() << &initializer << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      initializer.getVariant(),
      [](const Syntax::AssignExpr &assignExpr) { visit(assignExpr); },
      [](const box<Syntax::InitializerList> &initializerList) {
        visit(*initializerList);
      });
}

void visit(const Syntax::InitializerList &initializerList) {
  Print("InitializerList");
  llvm::outs() << &initializerList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &vec : initializerList.getInitializerList()) {
    if (vec.first) {
      for (const auto &designator : *vec.first) {
        match(
            designator,
            [](const Syntax::ConstantExpr &constantExpr) {
              visit(constantExpr);
            },
            [](const std::string_view &ident) {
              ValueReset v(LeftAlign, LeftAlign + 1);
              Println(ident);
            });
      }
    }
    visit(vec.second);
  }
}

void visit(const Syntax::StorageClsSpec &storageClassSpecifier) {
  Print("StorageClsSpec");
  llvm::outs() << &storageClassSpecifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  switch (storageClassSpecifier.getSpecifier()) {
  case Syntax::StorageClsSpec::Typedef:
    Println("Typedef");
    break;
  case Syntax::StorageClsSpec::Extern:
    Println("Extern");
    break;
  case Syntax::StorageClsSpec::Static:
    Println("Static");
    break;
  case Syntax::StorageClsSpec::Auto:
    Println("Auto");
    break;
  case Syntax::StorageClsSpec::Register:
    Println("Register");
    break;
  }
}
void visit(const Syntax::TypeQualifier &typeQualifier) {
  Print("TypeQualifier");
  llvm::outs() << &typeQualifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  switch (typeQualifier.getQualifier()) {
  case Syntax::TypeQualifier::Const:
    Println("Const");
    break;
  case Syntax::TypeQualifier::Volatile:
    Println("Volatile");
    break;
  case Syntax::TypeQualifier::Restrict:
    Println("Restrict");
    break;
  }
}
void visit(const Syntax::TypeSpec &typeSpecifier) {
  Print("TypeSpec");
  llvm::outs() << &typeSpecifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      typeSpecifier.getVariant(),
      [](const Syntax::TypeSpec::PrimTypeKind &primitiveTypeSpecifier) {
        Print("PrimTypeKind");
        llvm::outs() << &primitiveTypeSpecifier << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        switch (primitiveTypeSpecifier) {
        case Syntax::TypeSpec::Void: {
          Println("Void");
          break;
        }
        case Syntax::TypeSpec::Char: {
          Println("Char");
          break;
        }
        case Syntax::TypeSpec::Bool: {
          Println("Bool");
          break;
        }
        case Syntax::TypeSpec::Short: {
          Println("Short");
          break;
        }
        case Syntax::TypeSpec::Int: {
          Println("Int");
          break;
        }
        case Syntax::TypeSpec::Long: {
          Println("Long");
          break;
        }
        case Syntax::TypeSpec::Float: {
          Println("Float");
          break;
        }
        case Syntax::TypeSpec::Double: {
          Println("Double");
          break;
        }
        case Syntax::TypeSpec::Unsigned: {
          Println("Unsigned");
          break;
        }
        case Syntax::TypeSpec::Signed: {
          Println("Signed");
          break;
        }
        }
      },
      [](const box<Syntax::StructOrUnionSpec> &structOrUnionSpecifier) {
        Print("StructOrUnionSpec");
        llvm::outs() << &structOrUnionSpecifier << " "
                     << structOrUnionSpecifier->isUnion() << ", "
                     << structOrUnionSpecifier->getTag() << "\n";
        {
          ValueReset v(LeftAlign, LeftAlign + 1);
          for (const auto &structDeclaration :
               structOrUnionSpecifier->getStructDeclarations()) {
            visit(structDeclaration.specifierQualifiers_);
            for (const auto &structDeclarator :
                 structDeclaration.structDeclarators_) {
              if (structDeclarator.optionalDeclarator_) {
                visit(*structDeclarator.optionalDeclarator_);
              }
              if (structDeclarator.optionalBitfield_) {
                visit(*structDeclarator.optionalBitfield_);
              }
            }
          }
        }
      },
      [](const box<Syntax::EnumSpecifier> &enumSpecifier) {
        Print("EnumSpecifier");
        llvm::outs() << &enumSpecifier << " " << enumSpecifier->getName()
                     << "\n";
        {
          ValueReset v(LeftAlign, LeftAlign + 1);
          for (const auto &enumerator : enumSpecifier->getEnumerators()) {
            Print(enumerator.name_);
            if (enumerator.optionalConstantExpr_) {
              visit(*enumerator.optionalConstantExpr_);
            }
          }
        }
      },
      [](const Syntax::TypeSpec::TypedefName &stringView) {
        Println(stringView);
      });
}
void visit(const Syntax::FunctionSpecifier &functionSpecifier) {
  Print("FunctionSpecifier");
  llvm::outs() << &functionSpecifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println("inline");
}

void visit(const Syntax::Pointer &pointer) {
  Print("Pointer");
  llvm::outs() << &pointer << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &p : pointer.getTypeQualifiers()) {
    visit(p);
  }
}

void visit(const Syntax::DirectDeclarator &directDeclarator) {
  //  Print("DirectDeclarator");
  //  llvm::outs() << &directDeclarator << "\n";
  //  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      directDeclarator,
      [](const box<Syntax::DirectDeclaratorIdent> &ident) {
        Print("DirectDeclaratorIdent");
        llvm::outs() << &ident << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        Println(ident->getIdent());
      },
      [](const box<Syntax::DirectDeclaratorParentheses>
             &directDeclaratorParent) {
        Print("DirectDeclaratorParentheses");
        llvm::outs() << &directDeclaratorParent << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        visit(directDeclaratorParent->getDeclarator());
      },
      [](const box<Syntax::DirectDeclaratorAssignExpr>
             &directDeclaratorAssignExpr) {
        Print("DirectDeclaratorAssignExpr");
        llvm::outs() << &directDeclaratorAssignExpr << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        visit(directDeclaratorAssignExpr->getDirectDeclarator());
        if (directDeclaratorAssignExpr->hasStatic()) {
          Println("has static");
        }
        for (const auto &typeQualifier :
             directDeclaratorAssignExpr->getTypeQualifierList()) {
          visit(typeQualifier);
        }
        if (directDeclaratorAssignExpr->getAssignmentExpression()) {
          visit(*directDeclaratorAssignExpr->getAssignmentExpression());
        }
      },
      [](const box<Syntax::DirectDeclaratorParamTypeList>
             &directDeclaratorParentParamTypeList) {
        Print("DirectDeclaratorParamTypeList");
        llvm::outs() << &directDeclaratorParentParamTypeList << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        visit(directDeclaratorParentParamTypeList->getDirectDeclarator());
        visit(directDeclaratorParentParamTypeList->getParamTypeList());
      },
      [](const box<Syntax::DirectDeclaratorAsterisk>
             &directDeclaratorAsterisk) {
        Print("DirectDeclaratorAsterisk");
        llvm::outs() << &directDeclaratorAsterisk << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        visit(directDeclaratorAsterisk->getDirectDeclarator());
        for (const auto &typeQualifier :
             directDeclaratorAsterisk->getTypeQualifierList()) {
          visit(typeQualifier);
        }
      });
}

void visit(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator) {
  //  Print("DirectAbstractDeclarator");
  //  llvm::outs() << &directAbstractDeclarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      directAbstractDeclarator,
      [](const box<Syntax::DirectAbstractDeclaratorParentheses>
             &directAbstractDeclaratorParent) {
        Print("DirectAbstractDeclaratorParentheses");
        llvm::outs() << &directAbstractDeclaratorParent << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        visit(directAbstractDeclaratorParent->getAbstractDeclarator());
      },
      [](const box<Syntax::DirectAbstractDeclaratorAssignExpr>
             &directAbstractDeclaratorAssignExpr) {
        Print("DirectAbstractDeclaratorAssignExpr");
        llvm::outs() << &directAbstractDeclaratorAssignExpr << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        if (directAbstractDeclaratorAssignExpr->getDirectAbstractDeclarator()) {
          visit(*directAbstractDeclaratorAssignExpr
                     ->getDirectAbstractDeclarator());
        }
        if (directAbstractDeclaratorAssignExpr->hasStatic()) {
          Println("has static");
        }
        for (const auto &typeQualifier :
             directAbstractDeclaratorAssignExpr->getTypeQualifiers()) {
          visit(typeQualifier);
        }
        if (directAbstractDeclaratorAssignExpr->getAssignmentExpression()) {
          visit(*directAbstractDeclaratorAssignExpr->getAssignmentExpression());
        }
      },
      [](const box<Syntax::DirectAbstractDeclaratorParamTypeList>
             &directAbstractDeclaratorParamTypeList) {
        Print("DirectAbstractDeclaratorParamTypeList");
        llvm::outs() << &directAbstractDeclaratorParamTypeList << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        if (directAbstractDeclaratorParamTypeList
                ->getDirectAbstractDeclarator()) {
          visit(*directAbstractDeclaratorParamTypeList
                     ->getDirectAbstractDeclarator());
        }
        if (directAbstractDeclaratorParamTypeList->getParameterTypeList())
          visit(*directAbstractDeclaratorParamTypeList->getParameterTypeList());
      },
      [](const box<Syntax::DirectAbstractDeclaratorAsterisk>
             &directAbstractDeclaratorAsterisk) {
        Print("DirectAbstractDeclaratorAsterisk");
        llvm::outs() << &directAbstractDeclaratorAsterisk << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        if (directAbstractDeclaratorAsterisk->getDirectAbstractDeclarator()) {
          visit(
              *directAbstractDeclaratorAsterisk->getDirectAbstractDeclarator());
        }
      });
}

void visit(const Syntax::ParamTypeList &paramTypeList) {
  Print("ParamTypeList");
  llvm::outs() << &paramTypeList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(paramTypeList.getParameterList());
  if (paramTypeList.hasEllipse()) {
    Println("...");
  }
}

void visit(const Syntax::ParamList &paramList) {
  Print("ParamList");
  llvm::outs() << &paramList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &paramDecl : paramList.getParameterDeclarations()) {
    visit(paramDecl.declSpec_);
    match(
        paramDecl.declaratorKind_,
        [](const Syntax::Declarator &declarator) { visit(declarator); },
        [](const std::optional<Syntax::AbstractDeclarator>
               &abstractDeclarator) {
          if (abstractDeclarator)
            visit(*abstractDeclarator);
        });
  }
}

void visit(const Syntax::BlockStmt &blockStmt) {
  Print("BlockStmt");
  llvm::outs() << &blockStmt << "\n";
  for (const auto &blockItem : blockStmt.getBlockItems()) {
    visit(blockItem);
  }
}

void visit(const Syntax::BlockItem &blockItem) {
  //  Print("BlockItem");
  //  llvm::outs() << &blockItem << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      blockItem, [](const Syntax::Stmt &stmt) { visit(stmt); },
      [](const Syntax::Declaration &declaration) { visit(declaration); });
}

void visit(const Syntax::Stmt &stmt) {
  match(
      stmt, [](const box<Syntax::IfStmt> &ifStmt) { visit(*ifStmt); },
      [](const box<Syntax::ForStmt> &forStmt) { visit(*forStmt); },
      [](const box<Syntax::WhileStmt> &whileStmt) { visit(*whileStmt); },
      [](const box<Syntax::DoWhileStmt> &doWhileStmt) { visit(*doWhileStmt); },
      [](const box<Syntax::BreakStmt> &breakStmt) { visit(*breakStmt); },
      [](const box<Syntax::ContinueStmt> &continueStmt) {
        visit(*continueStmt);
      },
      [](const box<Syntax::SwitchStmt> &switchStmt) { visit(*switchStmt); },
      [](const box<Syntax::CaseStmt> &caseStmt) { visit(*caseStmt); },
      [](const box<Syntax::DefaultStmt> &defaultStmt) { visit(*defaultStmt); },
      [](const box<Syntax::ReturnStmt> &returnStmt) { visit(*returnStmt); },
      [](const box<Syntax::ExprStmt> &exprStmt) { visit(*exprStmt); },
      [](const box<Syntax::GotoStmt> &gotoStmt) { visit(*gotoStmt); },
      [](const box<Syntax::LabelStmt> &labelStmt) { visit(*labelStmt); },
      [](const box<Syntax::BlockStmt> &blockStmt) { visit(*blockStmt); });
}

void visit(const Syntax::IfStmt &ifStmt) {
  Print("IfStmt");
  llvm::outs() << &ifStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(ifStmt.getExpression());
  visit(ifStmt.getThenStmt());
  if (ifStmt.getElseStmt()) {
    visit(*ifStmt.getElseStmt());
  }
}
void visit(const Syntax::ForStmt &forStmt) {
  Print("ForStmt");
  llvm::outs() << &forStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      forStmt.getInitial(),
      [](const box<Syntax::Declaration> &declaration) { visit(*declaration); },
      [](const std::optional<Syntax::Expr> &expr) {
        if (expr)
          visit(*expr);
      });
  if (forStmt.getControlling())
    visit(*forStmt.getControlling());
  if (forStmt.getPost())
    visit(*forStmt.getPost());
  visit(forStmt.getStatement());
}
void visit(const Syntax::WhileStmt &whileStmt) {
  Print("WhileStmt");
  llvm::outs() << &whileStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(whileStmt.getExpression());
  visit(whileStmt.getStatement());
}
void visit(const Syntax::DoWhileStmt &doWhileStmt) {
  Print("DoWhileStmt");
  llvm::outs() << &doWhileStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(doWhileStmt.getStatement());
  visit(doWhileStmt.getExpression());
}
void visit(const Syntax::BreakStmt &breakStmt) {
  Print("BreakStmt");
  llvm::outs() << &breakStmt << "\n";
}
void visit(const Syntax::ContinueStmt &continueStmt) {
  Print("ContinueStmt");
  llvm::outs() << &continueStmt << "\n";
}
void visit(const Syntax::SwitchStmt &switchStmt) {
  Print("SwitchStmt");
  llvm::outs() << &switchStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(switchStmt.getExpression());
  visit(switchStmt.getStatement());
}
void visit(const Syntax::CaseStmt &caseStmt) {
  Print("CaseStmt");
  llvm::outs() << &caseStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(caseStmt.getConstantExpr());
  visit(caseStmt.getStatement());
}
void visit(const Syntax::DefaultStmt &defaultStmt) {
  Print("DefaultStmt");
  llvm::outs() << &defaultStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(defaultStmt.getStatement());
}
void visit(const Syntax::GotoStmt &gotoStmt) {
  Print("GotoStmt");
  llvm::outs() << &gotoStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println(gotoStmt.getIdentifier());
}
void visit(const Syntax::LabelStmt &labelStmt) {
  Print("LabelStmt");
  llvm::outs() << &labelStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println(labelStmt.getIdentifier());
}
void visit(const Syntax::ExprStmt &exprStmt) {
  Print("ExprStmt");
  llvm::outs() << &exprStmt << "\n";
  if (exprStmt.getOptionalExpression()) {
    visit(*exprStmt.getOptionalExpression());
  }
}
void visit(const Syntax::ReturnStmt &returnStmt) {
  Print("ReturnStmt");
  llvm::outs() << &returnStmt << "\n";
  if (returnStmt.getExpression()) {
    ValueReset v(LeftAlign, LeftAlign+1);
    visit(*returnStmt.getExpression());
  }
}

void visit(const Syntax::Expr &expr) {
  Print("Expr");
  llvm::outs() << &expr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &assignExpr : expr.getAssignExpressions()) {
    visit(assignExpr);
  }
}

void visit(const Syntax::AssignExpr &assignExpr) {
  Print("AssignExpr");
  llvm::outs() << &assignExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(assignExpr.getConditionalExpr());
  for (const auto &pair : assignExpr.getOptionalConditionalExpr()) {
    switch (pair.first) {
    case Syntax::AssignExpr::Assign: {
      Println("=");
      break;
    }
    case Syntax::AssignExpr::PlusAssign: {
      Println("+=");
      break;
    }
    case Syntax::AssignExpr::MinusAssign: {
      Println("-=");
      break;
    }
    case Syntax::AssignExpr::MultiplyAssign: {
      Println("*=");
      break;
    }
    case Syntax::AssignExpr::DivideAssign: {
      Println("/=");
      break;
    }
    case Syntax::AssignExpr::ModuloAssign: {
      Println("%=");
      break;
    }
    case Syntax::AssignExpr::LeftShiftAssign: {
      Println("<<=");
      break;
    }
    case Syntax::AssignExpr::RightShiftAssign: {
      Println(">>=");
      break;
    }
    case Syntax::AssignExpr::BitAndAssign: {
      Println("&=");
      break;
    }
    case Syntax::AssignExpr::BitOrAssign: {
      Println("|=");
      break;
    }
    case Syntax::AssignExpr::BitXorAssign: {
      Println("^=");
      break;
    }
    }
    visit(pair.second);
  }
}

/// conditionalExpr
void visit(const Syntax::ConstantExpr &constantExpr) {
  Print("CondExpr");
  llvm::outs() << &constantExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(constantExpr.getLogicalOrExpression());
  if (constantExpr.getOptionalExpression()) {
    visit(*constantExpr.getOptionalExpression());
  }
  if (constantExpr.getOptionalConditionalExpression()) {
    visit(*constantExpr.getOptionalConditionalExpression());
  }
}
void visit(const Syntax::LogOrExpr &logOrExpr) {
  Print("LogOrExpr");
  llvm::outs() << &logOrExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &logAndExpr : logOrExpr.getLogAndExprs()) {
    visit(logAndExpr);
  }
}
void visit(const Syntax::LogAndExpr &logAndExpr) {
  Print("LogAndExpr");
  llvm::outs() << &logAndExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitOrExpr : logAndExpr.getBitOrExprs()) {
    visit(bitOrExpr);
  }
}
void visit(const Syntax::BitOrExpr &bitOrExpr) {
  Print("BitOrExpr");
  llvm::outs() << &bitOrExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitXorExpr : bitOrExpr.getBitXorExprs()) {
    visit(bitXorExpr);
  }
}
void visit(const Syntax::BitXorExpr &bitXorExpr) {
  Print("BitXorExpr");
  llvm::outs() << &bitXorExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitAndExpr : bitXorExpr.getBitAndExprs()) {
    visit(bitAndExpr);
  }
}
void visit(const Syntax::BitAndExpr &bitAndExpr) {
  Print("BitAndExpr");
  llvm::outs() << &bitAndExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &equalExpr : bitAndExpr.getEqualExpr()) {
    visit(equalExpr);
  }
}
void visit(const Syntax::EqualExpr &equalExpr) {
  Print("EqualExpr");
  llvm::outs() << &equalExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(equalExpr.getRelationalExpr());
  for (const auto &relationExpr: equalExpr.getOptionalRelationalExpr()) {
    switch (relationExpr.first) {
    case Syntax::EqualExpr::Equal: {
      Println("==");
      break;
    }
    case Syntax::EqualExpr::NotEqual: {
      Println("!=");
      break;
    }
    }
    visit(relationExpr.second);
  }
}
void visit(const Syntax::RelationalExpr &relationalExpr) {
  Print("RelationalExpr");
  llvm::outs() << &relationalExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(relationalExpr.getShiftExpr());
  for (const auto &shiftExpr: relationalExpr.getOptionalShiftExpressions()) {
    switch (shiftExpr.first) {
    case Syntax::RelationalExpr::LessThan: {
      Println("<");
      break;
    }
    case Syntax::RelationalExpr::LessThanOrEqual: {
      Println("<=");
      break;
    }
    case Syntax::RelationalExpr::GreaterThan: {
      Println(">");
      break;
    }
    case Syntax::RelationalExpr::GreaterThanOrEqual: {
      Println(">=");
      break;
    }
    }
    visit(shiftExpr.second);
  }
}
void visit(const Syntax::ShiftExpr &shiftExpr) {
  Print("ShiftExpr");
  llvm::outs() << &shiftExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(shiftExpr.getAdditiveExpr());
  for (const auto &additiveExpr: shiftExpr.getOptAdditiveExps()) {
    switch (additiveExpr.first) {
    case Syntax::ShiftExpr::Left: {
      Println("<<");
      break;
    }
    case Syntax::ShiftExpr::Right: {
      Println(">>");
      break;
    }
    }
    visit(additiveExpr.second);
  }
}
void visit(const Syntax::AdditiveExpr &additiveExpr) {
  Print("AdditiveExpr");
  llvm::outs() << &additiveExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(additiveExpr.getMultiExpr());
  for (const auto &multiExpr: additiveExpr.getOptionalMultiExps()) {
    switch (multiExpr.first) {
    case Syntax::AdditiveExpr::Minus: {
      Println("-");
      break;
    }
    case Syntax::AdditiveExpr::Plus: {
      Println("+");
      break;
    }
    }
    visit(multiExpr.second);
  }
}
void visit(const Syntax::MultiExpr &multiExpr) {
  Print("MultiExpr");
  llvm::outs() << &multiExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(multiExpr.getCastExpr());
  for (const auto &castExpr: multiExpr.getOptionalCastExps()) {
    switch (castExpr.first) {
    case Syntax::MultiExpr::Multiply: {
      Println("*");
      break;
    }
    case Syntax::MultiExpr::Divide: {
      Println("/");
      break;
    }
    case Syntax::MultiExpr::Modulo: {
      Println("%");
      break;
    }
    }
    visit(castExpr.second);
  }
}
void visit(const Syntax::CastExpr &castExpr) {
  Print("CastExpr");
  llvm::outs() << &castExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  match(
      castExpr.getVariant(),
      [](const Syntax::UnaryExpr &unaryExpr) { visit(unaryExpr); },
      [](const Syntax::CastExpr::TypeNameCast &pair) {
        visit(pair.first);
        visit(*pair.second);
      });
}
void visit(const Syntax::UnaryExpr &unaryExpr) {
  match(
      unaryExpr,
      [](const Syntax::PostFixExpr &postFixExpr) {
        Print("UnaryExprPostFixExpr");
        llvm::outs() << &postFixExpr << "\n";
        visit(postFixExpr);
      },
      [](const box<Syntax::UnaryExprUnaryOperator> &unaryExprUnaryOperator) {
        Print("UnaryExprUnaryOperator");
        llvm::outs() << &unaryExprUnaryOperator << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        switch (unaryExprUnaryOperator->getOperator()) {
        case Syntax::UnaryExprUnaryOperator::Op::Increment: {
          Println("++");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::Decrement: {
          Println("--");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::Ampersand: {
          Println("&");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::Asterisk: {
          Println("*");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::Plus: {
          Println("+");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::Minus: {
          Println("-");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::BitNot: {
          Println("~");
          break;
        }
        case Syntax::UnaryExprUnaryOperator::Op::LogicalNot: {
          Println("!");
          break;
        }
        }
        visit(*unaryExprUnaryOperator->getCastExpr());
      },
      [](const box<Syntax::UnaryExprSizeOf> &unaryExprSizeOf) {
        Print("UnaryExprSizeOf");
        llvm::outs() << &unaryExprSizeOf << "\n";
        ValueReset v(LeftAlign, LeftAlign + 1);
        match(
            unaryExprSizeOf->getVariant(),
            [](const Syntax::UnaryExpr &unaryExpr) { visit(unaryExpr); },
            [](const Syntax::TypeNameBox &typeName) { visit(*typeName); });
      });
}
void visit(const Syntax::TypeName &typeName) {
  Print("TypeName");
  llvm::outs() << &typeName << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visit(typeName.getSpecifierQualifiers());
  if (typeName.getAbstractDeclarator())
    visit(*typeName.getAbstractDeclarator());
}

void visit(const Syntax::PostFixExpr &postFixExpr) {
  match(
      postFixExpr,
      [](const Syntax::PrimaryExpr &primaryExpr) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprPrimaryExpr");
        llvm::outs() << &primaryExpr << "\n";
        visit(primaryExpr);
      },
      [](const box<Syntax::PostFixExprSubscript> &subscript) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprSubscript");
        llvm::outs() << &subscript << "\n";
        visit(subscript->getPostFixExpr());
        visit(subscript->getExpr());
      },
      [](const box<Syntax::PostFixExprFuncCall> &funcCall) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprFuncCall");
        llvm::outs() << &funcCall << "\n";
        visit(funcCall->getPostFixExpr());
        for (const auto &assignExpr :
             funcCall->getOptionalAssignExpressions()) {
          visit(*assignExpr);
        }
      },
      [](const box<Syntax::PostFixExprDot> &dot) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprDot");
        llvm::outs() << &dot << "\n";
        visit(dot->getPostFixExpr());
        Println(dot->getIdentifier());
      },
      [](const box<Syntax::PostFixExprArrow> &arrow) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprArrow");
        llvm::outs() << &arrow << "\n";
        visit(arrow->getPostFixExpr());
        Println(arrow->getIdentifier());
      },
      [](const box<Syntax::PostFixExprIncrement> &increment) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprIncrement");
        llvm::outs() << &increment << "\n";
        visit(increment->getPostFixExpr());
      },
      [](const box<Syntax::PostFixExprDecrement> &decrement) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprDecrement");
        llvm::outs() << &decrement << "\n";
        visit(decrement->getPostFixExpr());
      },
      [](const box<Syntax::PostFixExprTypeInitializer> &typeInitializer) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PostFixExprTypeInitializer");
        llvm::outs() << &typeInitializer << "\n";
        visit(typeInitializer->getTypeName());
        visit(typeInitializer->getInitializerList());
      });
}
void visit(const Syntax::PrimaryExpr &primaryExpr) {
  match(
      primaryExpr,
      [](const Syntax::PrimaryExprIdent &ident) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PrimaryExprIdent");
        llvm::outs() << &ident << "\n";
        {
          ValueReset v(LeftAlign, LeftAlign + 1);
          Println(ident.getIdentifier());
        }
      },
      [](const Syntax::PrimaryExprConstant &constant) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PrimaryExprConstant");
        llvm::outs() << &constant << "\n";
        match(constant.getValue(), [](auto &&value) {
          ValueReset v(LeftAlign, LeftAlign + 1);
          using T = std::decay_t<decltype(value)>;
          if constexpr (std::is_same_v<T, std::string>) {
            Println(value);
          } else {
            Println(std::to_string(value));
          }
        });
      },
      [](const Syntax::PrimaryExprParentheses &parent) {
        ValueReset v(LeftAlign, LeftAlign + 1);
        Print("PrimaryExprParentheses");
        llvm::outs() << &parent << "\n";
        visit(parent.getExpr());
      });
}
}
