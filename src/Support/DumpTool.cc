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
#include "DumpTool.h"
#include "Utilities.h"
#include "llvm/Support/raw_ostream.h"
#include "ValueReset.h"
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

void dumpAst(const lcc::Syntax::TranslationUnit &unit) {
  visitor(unit);
}

void visitor(const Syntax::TranslationUnit &unit) {
  Print("TranslationUnit");
  llvm::outs() << &unit << " " << unit.getGlobals().size() << "\n";
  for (auto &externalDecl : unit.getGlobals()) {
    std::visit(overload{
                   [](const Syntax::Declaration &declaration) {
                     ValueReset v(LeftAlign, LeftAlign+1);
                     visitor(declaration);

                   },
                   [](const Syntax::FunctionDefinition &functionDefinition) {
                     ValueReset v(LeftAlign, LeftAlign+1);
                     visitor(functionDefinition);

                   }
               }, externalDecl);
  }
}
void visitor(const Syntax::Declaration &declaration) {
  Print("Declaration");
  llvm::outs() << &declaration << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(declaration.getDeclarationSpecifiers());
  for (auto &initDec : declaration.getInitDeclarators()) {
      visitor(*initDec.declarator_);

    if (initDec.optionalInitializer_) {
      visitor(*initDec.optionalInitializer_);
    }
  }

}
void visitor(const Syntax::FunctionDefinition &functionDefinition) {
  Print("FunctionDefinition");
  llvm::outs() << &functionDefinition << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(functionDefinition.getDeclarationSpecifiers());
  visitor(functionDefinition.getDeclarator());
  visitor(functionDefinition.getCompoundStatement());

}
void visitor(const Syntax::DeclSpec &declarationSpecifiers) {
  Print("DeclSpec");
  llvm::outs() << &declarationSpecifiers << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &storage : declarationSpecifiers.getStorageClassSpecifiers()) {
    visitor(storage);
  }
  for (const auto &qualifier : declarationSpecifiers.getTypeQualifiers()) {
    visitor(qualifier);
  }
  for (const auto &specifier : declarationSpecifiers.getTypeSpecs()) {
    visitor(specifier);
  }
}
void visitor(const Syntax::Declarator &declarator) {
  Print("Declarator");
  llvm::outs() << &declarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &point : declarator.getPointers()) {
    visitor(point);
  }
  visitor(declarator.getDirectDeclarator());

}

void visitor(const Syntax::AbstractDeclarator &abstractDeclarator) {
  Print("AbstractDeclarator");
  llvm::outs() << &abstractDeclarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &point : abstractDeclarator.getPointers()) {
    visitor(point);
  }
  if (abstractDeclarator.getDirectAbstractDeclarator()) {
    visitor(*abstractDeclarator.getDirectAbstractDeclarator());
  }

}

void visitor(const Syntax::Initializer &initializer) {
  Print("Initializer");
  llvm::outs() << &initializer << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
      [](const Syntax::AssignExpr &assignExpr) {
         visitor(assignExpr);
      },[](const box<Syntax::InitializerList> &initializerList) {
       visitor(*initializerList);}
  }, initializer.getVariant());
}

void visitor(const Syntax::InitializerList &initializerList) {
  Print("InitializerList");
  llvm::outs() << &initializerList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &vec : initializerList.getInitializerList()) {
    if (vec.first) {
      for (const auto &designator : *vec.first) {
        std::visit(overload{
                       [](const Syntax::ConstantExpr &constantExpr) {
                         visitor(constantExpr);
                       },
                       [](const std::string_view &ident) {
                         ValueReset v(LeftAlign, LeftAlign+1);
                         Println(ident);

                       },
                   }, designator);
      }
    }
    visitor(vec.second);
  }

}

void visitor(const Syntax::StorageClsSpec &storageClassSpecifier) {
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
  default:
    break;
  }
}
void visitor(const Syntax::TypeQualifier &typeQualifier) {
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
  default:
    break;
  }

}
void visitor(const Syntax::TypeSpec &typeSpecifier) {
  Print("TypeSpec");
  llvm::outs() << &typeSpecifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(
      overload{
          [](const Syntax::TypeSpec::PrimTypeKind &primitiveTypeSpecifier) {
            Print("PrimTypeKind");
            llvm::outs() << &primitiveTypeSpecifier << "\n";
            ValueReset v(LeftAlign, LeftAlign+1);
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
            default:
              break;
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
                visitor(structDeclaration.specifierQualifiers_);
                for (const auto &structDeclarator :
                     structDeclaration.structDeclarators_) {
                  if (structDeclarator.optionalDeclarator_) {
                    visitor(*structDeclarator.optionalDeclarator_);
                  }
                  if (structDeclarator.optionalBitfield_) {
                    visitor(*structDeclarator.optionalBitfield_);
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
              ValueReset v(LeftAlign, LeftAlign+1);
              for (const auto &enumerator : enumSpecifier->getEnumerators()) {
                Print(enumerator.name_);
                if (enumerator.optionalConstantExpr_) {
                  visitor(*enumerator.optionalConstantExpr_);
                }
              }
            }
          },
          [](const Syntax::TypeSpec::TypedefName &stringView) {
            Println(stringView);
          }},
      typeSpecifier.getVariant());
}
void visitor(const Syntax::FunctionSpecifier &functionSpecifier ) {
  Print("FunctionSpecifier");
  llvm::outs() << &functionSpecifier << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println("inline");
}


void visitor(const Syntax::Pointer &pointer) {
  Print("Pointer");
  llvm::outs() << &pointer << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &p : pointer.getTypeQualifiers()) {
    visitor(p);
  }
}

void visitor(const Syntax::DirectDeclarator &directDeclarator) {
//  Print("DirectDeclarator");
//  llvm::outs() << &directDeclarator << "\n";
//  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
      [](const box<Syntax::DirectDeclaratorIdent> &ident) {
         Print("DirectDeclaratorIdent");
         llvm::outs() << &ident << "\n";
         ValueReset v(LeftAlign, LeftAlign+1);
         Println(ident->getIdent());

      },
      [](const box<Syntax::DirectDeclaratorParentheses> &directDeclaratorParent) {
         Print("DirectDeclaratorParentheses");
         llvm::outs() << &directDeclaratorParent << "\n";
         ValueReset v(LeftAlign, LeftAlign+1);
         visitor(directDeclaratorParent->getDeclarator());

      },
      [](const box<Syntax::DirectDeclaratorAssignExpr> &directDeclaratorAssignExpr) {
         Print("DirectDeclaratorAssignExpr");
         llvm::outs() << &directDeclaratorAssignExpr << "\n";
         ValueReset v(LeftAlign, LeftAlign+1);
         visitor(directDeclaratorAssignExpr->getDirectDeclarator());
         if (directDeclaratorAssignExpr->hasStatic()) {
           Println("has static");
         }
         for (const auto &typeQualifier : directDeclaratorAssignExpr->getTypeQualifierList()) {
           visitor(typeQualifier);
         }
         if (directDeclaratorAssignExpr->getAssignmentExpression()) {
           visitor(*directDeclaratorAssignExpr->getAssignmentExpression());
         }
      },
      [](const box<Syntax::DirectDeclaratorParamTypeList>
                 &directDeclaratorParentParamTypeList) {
         Print("DirectDeclaratorParamTypeList");
         llvm::outs() << &directDeclaratorParentParamTypeList << "\n";
         ValueReset v(LeftAlign, LeftAlign+1);
         visitor(directDeclaratorParentParamTypeList->getDirectDeclarator());
         visitor(directDeclaratorParentParamTypeList->getParamTypeList());
      },
     [](const box<Syntax::DirectDeclaratorAsterisk> &directDeclaratorAsterisk) {
       Print("DirectDeclaratorAsterisk");
       llvm::outs() << &directDeclaratorAsterisk << "\n";
       ValueReset v(LeftAlign, LeftAlign+1);
       visitor(directDeclaratorAsterisk->getDirectDeclarator());
       for (const auto &typeQualifier : directDeclaratorAsterisk->getTypeQualifierList()) {
         visitor(typeQualifier);
       }
     },
  }, directDeclarator);
}

void visitor(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator) {
//  Print("DirectAbstractDeclarator");
//  llvm::outs() << &directAbstractDeclarator << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
                 [](const box<Syntax::DirectAbstractDeclaratorParentheses>
                        &directAbstractDeclaratorParent) {
                   Print("DirectAbstractDeclaratorParentheses");
                   llvm::outs() << &directAbstractDeclaratorParent << "\n";
                   ValueReset v(LeftAlign, LeftAlign+1);
                   visitor(directAbstractDeclaratorParent->getAbstractDeclarator());
                 },
                 [](const box<Syntax::DirectAbstractDeclaratorAssignExpr> &directAbstractDeclaratorAssignExpr) {
                   Print("DirectAbstractDeclaratorAssignExpr");
                   llvm::outs() << &directAbstractDeclaratorAssignExpr << "\n";
                   ValueReset v(LeftAlign, LeftAlign+1);
                   if (directAbstractDeclaratorAssignExpr->getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorAssignExpr
                                  ->getDirectAbstractDeclarator());
                   }
                   if (directAbstractDeclaratorAssignExpr->hasStatic()) {
                     Println("has static");
                   }
                   for (const auto &typeQualifier : directAbstractDeclaratorAssignExpr->getTypeQualifiers()) {
                     visitor(typeQualifier);
                   }
                   if (directAbstractDeclaratorAssignExpr->getAssignmentExpression()) {
                     visitor(*directAbstractDeclaratorAssignExpr->getAssignmentExpression());
                   }
                 },
                 [](const box<Syntax::DirectAbstractDeclaratorParamTypeList> &directAbstractDeclaratorParamTypeList) {
                   Print("DirectAbstractDeclaratorParamTypeList");
                   llvm::outs() << &directAbstractDeclaratorParamTypeList << "\n";
                   ValueReset v(LeftAlign, LeftAlign+1);
                   if (directAbstractDeclaratorParamTypeList->getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorParamTypeList->getDirectAbstractDeclarator());
                   }
                   if (directAbstractDeclaratorParamTypeList->getParameterTypeList())
                    visitor(*directAbstractDeclaratorParamTypeList->getParameterTypeList());
                 },
                 [](const box<Syntax::DirectAbstractDeclaratorAsterisk> &directAbstractDeclaratorAsterisk) {
                   Print("DirectAbstractDeclaratorAsterisk");
                   llvm::outs() << &directAbstractDeclaratorAsterisk << "\n";
                   ValueReset v(LeftAlign, LeftAlign+1);
                   if (directAbstractDeclaratorAsterisk->getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorAsterisk->getDirectAbstractDeclarator());
                   }
                 },
             }, directAbstractDeclarator);

}

void visitor(const Syntax::ParamTypeList &paramTypeList) {
  Print("ParamTypeList");
  llvm::outs() << &paramTypeList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(paramTypeList.getParameterList());
  if (paramTypeList.hasEllipse()) {
    Println("...");
  }

}

void visitor(const Syntax::ParamList &paramList) {
  Print("ParamList");
  llvm::outs() << &paramList << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &paramDecl : paramList.getParameterDeclarations()) {
    visitor(paramDecl.declarationSpecifiers_);
    std::visit(overload{
        [](const Syntax::Declarator &declarator) {
           visitor(declarator);
        },
        [](const std::optional<Syntax::AbstractDeclarator> &abstractDeclarator) {
            if (abstractDeclarator)
              visitor(*abstractDeclarator);
        },
    }, paramDecl.declaratorKind_);
  }
}

void visitor(const Syntax::BlockStmt &blockStmt) {
  Print("BlockStmt");
  llvm::outs() << &blockStmt << "\n";
  for (const auto &blockItem : blockStmt.getBlockItems()) {
    visitor(blockItem);
  }
}

void visitor(const Syntax::BlockItem &blockItem) {
//  Print("BlockItem");
//  llvm::outs() << &blockItem << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
      [](const Syntax::Stmt &stmt) {
         visitor(stmt);
      },
      [](const Syntax::Declaration &declaration) {
         visitor(declaration);
      }
  }, blockItem);
}

void visitor(const Syntax::Stmt &stmt) {
  std::visit(overload{
                 [](const box<Syntax::IfStmt> &ifStmt){ visitor(*ifStmt);},
                 [](const box<Syntax::ForStmt> &forStmt){ visitor(*forStmt);},
                 [](const box<Syntax::WhileStmt> &whileStmt){ visitor(*whileStmt);},
                 [](const box<Syntax::DoWhileStmt> &doWhileStmt){ visitor(*doWhileStmt);},
                 [](const box<Syntax::BreakStmt> &breakStmt){ visitor(*breakStmt);},
                 [](const box<Syntax::ContinueStmt> &continueStmt){ visitor(*continueStmt);},
                 [](const box<Syntax::SwitchStmt> &switchStmt){ visitor(*switchStmt);},
                 [](const box<Syntax::CaseStmt> &caseStmt){ visitor(*caseStmt);},
                 [](const box<Syntax::DefaultStmt> &defaultStmt){ visitor(*defaultStmt);},
                 [](const box<Syntax::ReturnStmt> &returnStmt){ visitor(*returnStmt);},
                 [](const box<Syntax::ExprStmt> &exprStmt){ visitor(*exprStmt);},
                 [](const box<Syntax::GotoStmt> &gotoStmt){ visitor(*gotoStmt);},
                 [](const box<Syntax::LabelStmt> &labelStmt){ visitor(*labelStmt);},
                 [](const box<Syntax::BlockStmt> &blockStmt){ visitor(*blockStmt);},
             }, stmt);
}

void visitor(const Syntax::IfStmt &ifStmt){
  Print("IfStmt");
  llvm::outs() << &ifStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(ifStmt.getExpression());
  visitor(ifStmt.getThenStmt());
  if (ifStmt.getElseStmt()) {
    visitor(*ifStmt.getElseStmt());
  }

}
void visitor(const Syntax::ForStmt &forStmt){
  Print("ForStmt");
  llvm::outs() << &forStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
      [](const box<Syntax::Declaration>& declaration) { visitor(*declaration);},
      [](const std::optional<Syntax::Expr>& expr) { if (expr) visitor(*expr);},
  }, forStmt.getInitial());
  if (forStmt.getControlling())
    visitor(*forStmt.getControlling());
  if (forStmt.getPost())
    visitor(*forStmt.getPost());
  visitor(forStmt.getStatement());
}
void visitor(const Syntax::WhileStmt &whileStmt){
  Print("WhileStmt");
  llvm::outs() << &whileStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(whileStmt.getExpression());
  visitor(whileStmt.getStatement());
}
void visitor(const Syntax::DoWhileStmt &doWhileStmt){
  Print("DoWhileStmt");
  llvm::outs() << &doWhileStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(doWhileStmt.getStatement());
  visitor(doWhileStmt.getExpression());
}
void visitor(const Syntax::BreakStmt &breakStmt){
  Print("BreakStmt");
  llvm::outs() << &breakStmt << "\n";
}
void visitor(const Syntax::ContinueStmt &continueStmt){
  Print("ContinueStmt");
  llvm::outs() << &continueStmt << "\n";
}
void visitor(const Syntax::SwitchStmt &switchStmt){
  Print("SwitchStmt");
  llvm::outs() << &switchStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(switchStmt.getExpression());
  visitor(switchStmt.getStatement());
}
void visitor(const Syntax::CaseStmt &caseStmt){
  Print("CaseStmt");
  llvm::outs() << &caseStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(caseStmt.getConstantExpr());
  visitor(caseStmt.getStatement());
}
void visitor(const Syntax::DefaultStmt &defaultStmt){
  Print("DefaultStmt");
  llvm::outs() << &defaultStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(defaultStmt.getStatement());
}
void visitor(const Syntax::GotoStmt &gotoStmt){
  Print("GotoStmt");
  llvm::outs() << &gotoStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println(gotoStmt.getIdentifier());
}
void visitor(const Syntax::LabelStmt &labelStmt){
  Print("LabelStmt");
  llvm::outs() << &labelStmt << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  Println(labelStmt.getIdentifier());

}
void visitor(const Syntax::ExprStmt &exprStmt){
  Print("ExprStmt");
  llvm::outs() << &exprStmt << "\n";
  if (exprStmt.getOptionalExpression()) {
    visitor(*exprStmt.getOptionalExpression());
  }
}
void visitor(const Syntax::ReturnStmt &returnStmt){
  Print("ReturnStmt");
  llvm::outs() << &returnStmt << "\n";
  if (returnStmt.getExpression()) {
    ValueReset v(LeftAlign, LeftAlign+1);
    visitor(*returnStmt.getExpression());
  }
}

void visitor(const Syntax::Expr &expr) {
  Print("Expr");
  llvm::outs() << &expr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &assignExpr : expr.getAssignExpressions()) {
    visitor(assignExpr);
  }
}

void visitor(const Syntax::AssignExpr &assignExpr) {
  Print("AssignExpr");
  llvm::outs() << &assignExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(assignExpr.getConditionalExpr());
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
    default:
      break;
    }
    visitor(pair.second);
  }
}

/// conditionalExpr
void visitor(const Syntax::ConstantExpr &constantExpr) {
  Print("CondExpr");
  llvm::outs() << &constantExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(constantExpr.getLogicalOrExpression());
  if (constantExpr.getOptionalExpression()) {
    visitor(*constantExpr.getOptionalExpression());
  }
  if (constantExpr.getOptionalConditionalExpression()) {
    visitor(*constantExpr.getOptionalConditionalExpression());
  }

}
void visitor(const Syntax::LogOrExpr &logOrExpr){
  Print("LogOrExpr");
  llvm::outs() << &logOrExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &logAndExpr : logOrExpr.getLogAndExprs()) {
    visitor(logAndExpr);
  }
}
void visitor(const Syntax::LogAndExpr &logAndExpr){
  Print("LogAndExpr");
  llvm::outs() << &logAndExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitOrExpr : logAndExpr.getBitOrExprs()) {
    visitor(bitOrExpr);
  }
}
void visitor(const Syntax::BitOrExpr &bitOrExpr){
  Print("BitOrExpr");
  llvm::outs() << &bitOrExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitXorExpr : bitOrExpr.getBitXorExprs()) {
    visitor(bitXorExpr);
  }
}
void visitor(const Syntax::BitXorExpr &bitXorExpr){
  Print("BitXorExpr");
  llvm::outs() << &bitXorExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &bitAndExpr : bitXorExpr.getBitAndExprs()) {
    visitor(bitAndExpr);
  }
}
void visitor(const Syntax::BitAndExpr &bitAndExpr){
  Print("BitAndExpr");
  llvm::outs() << &bitAndExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  for (const auto &equalExpr : bitAndExpr.getEqualExpr()) {
    visitor(equalExpr);
  }
}
void visitor(const Syntax::EqualExpr &equalExpr){
  Print("EqualExpr");
  llvm::outs() << &equalExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(equalExpr.getRelationalExpr());
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
    default:
      break;
    }
    visitor(relationExpr.second);
  }
}
void visitor(const Syntax::RelationalExpr &relationalExpr){
  Print("RelationalExpr");
  llvm::outs() << &relationalExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(relationalExpr.getShiftExpr());
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
    default:
      break;
    }
    visitor(shiftExpr.second);
  }
}
void visitor(const Syntax::ShiftExpr &shiftExpr){
  Print("ShiftExpr");
  llvm::outs() << &shiftExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(shiftExpr.getAdditiveExpr());
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
    default:
      break;
    }
    visitor(additiveExpr.second);
  }
}
void visitor(const Syntax::AdditiveExpr &additiveExpr){
  Print("AdditiveExpr");
  llvm::outs() << &additiveExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(additiveExpr.getMultiExpr());
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
    default:
      break;
    }
    visitor(multiExpr.second);
  }
}
void visitor(const Syntax::MultiExpr &multiExpr){
  Print("MultiExpr");
  llvm::outs() << &multiExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(multiExpr.getCastExpr());
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
    default:
      break;
    }
    visitor(castExpr.second);
  }

}
void visitor(const Syntax::CastExpr &castExpr){
  Print("CastExpr");
  llvm::outs() << &castExpr << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  std::visit(overload{
      [](const Syntax::UnaryExpr &unaryExpr) {visitor(unaryExpr);},
      [](const Syntax::CastExpr::TypeNameCast &pair) {
         visitor(pair.first);
         visitor(*pair.second);
      }
  }, castExpr.getVariant());
}
void visitor(const Syntax::UnaryExpr &unaryExpr){
  std::visit(overload{
    [](const Syntax::PostFixExpr &postFixExpr) {
       Print("UnaryExprPostFixExpr");
       llvm::outs() << &postFixExpr << "\n";
       visitor(postFixExpr);
    },
    [](const box<Syntax::UnaryExprUnaryOperator> &unaryExprUnaryOperator) {
       Print("UnaryExprUnaryOperator");
       llvm::outs() << &unaryExprUnaryOperator << "\n";
       ValueReset v(LeftAlign, LeftAlign+1);
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
       default:
         break;
       }
       visitor(*unaryExprUnaryOperator->getCastExpr());
    },
    [](const box<Syntax::UnaryExprSizeOf> &unaryExprSizeOf) {
       Print("UnaryExprSizeOf");
       llvm::outs() << &unaryExprSizeOf << "\n";
       ValueReset v(LeftAlign, LeftAlign+1);
       std::visit(overload{
         [](const Syntax::UnaryExpr& unaryExpr){visitor(unaryExpr);},
         [](const Syntax::TypeNameBox& typeName){visitor(*typeName);},
       },
       unaryExprSizeOf->getVariant());
    },
  }, unaryExpr);
}
void visitor(const Syntax::TypeName &typeName){
  Print("TypeName");
  llvm::outs() << &typeName << "\n";
  ValueReset v(LeftAlign, LeftAlign+1);
  visitor(typeName.getSpecifierQualifiers());
  if (typeName.getAbstractDeclarator())
    visitor(*typeName.getAbstractDeclarator());

}
void visitor(const Syntax::PostFixExpr &postFixExpr){
  std::visit(overload{
     [](const Syntax::PrimaryExpr &primaryExpr) {
          ValueReset v(LeftAlign, LeftAlign+1);
          Print("PostFixExprPrimaryExpr");
          llvm::outs() << &primaryExpr << "\n";
          visitor(primaryExpr);
     },
     [](const box<Syntax::PostFixExprSubscript> &subscript) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprSubscript");
         llvm::outs() << &subscript << "\n";
         visitor(subscript->getPostFixExpr());
         visitor(subscript->getExpr());

     },
     [](const box<Syntax::PostFixExprFuncCall> &funcCall) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprFuncCall");
         llvm::outs() << &funcCall << "\n";
         visitor(funcCall->getPostFixExpr());
         for (const auto &assignExpr : funcCall->getOptionalAssignExpressions()) {
           visitor(*assignExpr);
         }
     },
     [](const box<Syntax::PostFixExprDot> &dot) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprDot");
         llvm::outs() << &dot << "\n";
         visitor(dot->getPostFixExpr());
         Println(dot->getIdentifier());
       },
     [](const box<Syntax::PostFixExprArrow> &arrow) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprArrow");
         llvm::outs() << &arrow << "\n";
         visitor(arrow->getPostFixExpr());
         Println(arrow->getIdentifier());
       },
     [](const box<Syntax::PostFixExprIncrement> &increment) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprIncrement");
         llvm::outs() << &increment << "\n";
         visitor(increment->getPostFixExpr());
     },
     [](const box<Syntax::PostFixExprDecrement> &decrement) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprDecrement");
         llvm::outs() << &decrement << "\n";
         visitor(decrement->getPostFixExpr());
     },
     [](const box<Syntax::PostFixExprTypeInitializer> &typeInitializer) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PostFixExprTypeInitializer");
         llvm::outs() << &typeInitializer << "\n";
         visitor(typeInitializer->getTypeName());
         visitor(typeInitializer->getInitializerList());
     },
  }, postFixExpr);
}
void visitor(const Syntax::PrimaryExpr &primaryExpr){
  std::visit(overload{
     [](const Syntax::PrimaryExprIdent& ident) {
       ValueReset v(LeftAlign, LeftAlign+1);
       Print("PrimaryExprIdent");
       llvm::outs() << &ident << "\n";
       {
         ValueReset v(LeftAlign, LeftAlign+1);
         Println(ident.getIdentifier());
       }
     },
     [](const Syntax::PrimaryExprConstant& constant) {
       ValueReset v(LeftAlign, LeftAlign+1);
       Print("PrimaryExprConstant");
       llvm::outs() << &constant << "\n";
       std::visit([](auto &&value) {
         ValueReset v(LeftAlign, LeftAlign+1);
         using T = std::decay_t<decltype(value)>;
         if constexpr (std::is_same_v<T, std::string>) {
           Println(value);
         }else {
           Println(std::to_string(value));
         }
       }, constant.getValue());
     },
     [](const Syntax::PrimaryExprParentheses & parent) {
         ValueReset v(LeftAlign, LeftAlign+1);
         Print("PrimaryExprParentheses");
         llvm::outs() << &parent << "\n";
         visitor(parent.getExpr());
     },
  }, primaryExpr);
}
}
