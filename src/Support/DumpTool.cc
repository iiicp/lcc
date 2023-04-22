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
namespace lcc::dump {

static uint64_t LeftAlign = 1;

void IncAlign() {
  LeftAlign++;
}

void DecAlign() {
  assert(LeftAlign > 1);
  LeftAlign--;
}

//void Print(const std::string &content) {
//  std::string ws(LeftAlign, ' ');
//  llvm::outs() << ws << content << " ";
//}

//void Println(const std::string &content) {
//  std::string ws(LeftAlign, ' ');
//  llvm::outs() << ws << content << "\n";
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
                     IncAlign();
                     visitor(declaration);
                     DecAlign();
                   },
                   [](const Syntax::FunctionDefinition &functionDefinition) {
                     IncAlign();
                     visitor(functionDefinition);
                     DecAlign();
                   }
               }, externalDecl);
  }
}
void visitor(const Syntax::Declaration &declaration) {
  Print("Declaration");
  llvm::outs() << &declaration << "\n";
  IncAlign();
  visitor(declaration.getDeclarationSpecifiers());
  for (auto &initDec : declaration.getInitDeclarators()) {
    if (initDec.mDeclarator) {
      visitor(*initDec.mDeclarator);
    }
    if (initDec.mOptInitializer) {
      visitor(*initDec.mOptInitializer);
    }
  }
  DecAlign();
}
void visitor(const Syntax::FunctionDefinition &functionDefinition) {
  Print("FunctionDefinition");
  llvm::outs() << &functionDefinition << "\n";
  IncAlign();
  visitor(functionDefinition.getDeclarationSpecifiers());
  visitor(functionDefinition.getDeclarator());
  visitor(functionDefinition.getCompoundStatement());
  DecAlign();
}
void visitor(const Syntax::DeclarationSpecifiers &declarationSpecifiers) {
  Print("DeclarationSpecifiers");
  llvm::outs() << &declarationSpecifiers << "\n";
  IncAlign();
  for (const auto &storage : declarationSpecifiers.getStorageClassSpecifiers()) {
    visitor(storage);
  }
  for (const auto &qualifier : declarationSpecifiers.getTypeQualifiers()) {
    visitor(qualifier);
  }
  for (const auto &specifier : declarationSpecifiers.getTypeSpecifiers()) {
    visitor(specifier);
  }
  DecAlign();
}
void visitor(const Syntax::Declarator &declarator) {
  Print("Declarator");
  llvm::outs() << &declarator << "\n";
  IncAlign();
  for (const auto &point : declarator.getPointers()) {
    visitor(point);
  }
  visitor(declarator.getDirectDeclarator());
  DecAlign();
}

void visitor(const Syntax::AbstractDeclarator &abstractDeclarator) {
  Print("AbstractDeclarator");
  llvm::outs() << &abstractDeclarator << "\n";
  IncAlign();
  for (const auto &point : abstractDeclarator.getPointers()) {
    visitor(point);
  }
  if (abstractDeclarator.getDirectAbstractDeclarator()) {
    visitor(*abstractDeclarator.getDirectAbstractDeclarator());
  }
  DecAlign();
}

void visitor(const Syntax::Initializer &initializer) {
  Print("Initializer");
  llvm::outs() << &initializer << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::AssignExpr &assignExpr) {
         visitor(assignExpr);
      },[](const std::unique_ptr<Syntax::InitializerList> &initializerList) {
       visitor(*initializerList);}
  }, initializer.getVariant());
  DecAlign();
}

void visitor(const Syntax::InitializerList &initializerList) {
  Print("InitializerList");
  llvm::outs() << &initializerList << "\n";
  IncAlign();
  for (const auto &vec : initializerList.getInitializerList()) {
    for (const auto &designator : vec.second) {
      std::visit(overload{
          [](const Syntax::ConstantExpr &constantExpr) {
             visitor(constantExpr);
          },
          [](const std::string_view &ident) {
             IncAlign();
             Println(ident);
             DecAlign();
          },
      }, designator);
    }
    visitor(vec.first);
  }
  DecAlign();
}

void visitor(const Syntax::StorageClassSpecifier &storageClassSpecifier) {
  Print("StorageClassSpecifier");
  llvm::outs() << &storageClassSpecifier << "\n";
  IncAlign();
  switch (storageClassSpecifier.getSpecifier()) {
  case Syntax::StorageClassSpecifier::Typedef:
    Println("Typedef");
    break;
  case Syntax::StorageClassSpecifier::Extern:
    Println("Extern");
    break;
  case Syntax::StorageClassSpecifier::Static:
    Println("Static");
    break;
  case Syntax::StorageClassSpecifier::Auto:
    Println("Auto");
    break;
  case Syntax::StorageClassSpecifier::Register:
    Println("Register");
    break;
  default:
    break;
  }
  DecAlign();
}
void visitor(const Syntax::TypeQualifier &typeQualifier) {
  Print("TypeQualifier");
  llvm::outs() << &typeQualifier << "\n";
  IncAlign();
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
  DecAlign();
}
void visitor(const Syntax::TypeSpecifier &typeSpecifier) {
  Print("TypeSpecifier");
  llvm::outs() << &typeSpecifier << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::TypeSpecifier::PrimitiveTypeSpecifier & primitiveTypeSpecifier) {
            Print("PrimitiveTypeSpecifier");
            llvm::outs() << &primitiveTypeSpecifier << "\n";
            IncAlign();
            switch (primitiveTypeSpecifier) {
            case Syntax::TypeSpecifier::Void: {
              Println("Void");
              break;
            }
            case Syntax::TypeSpecifier::Char: {
              Println("Char");
              break;
            }
            case Syntax::TypeSpecifier::Bool: {
              Println("Bool");
              break;
            }
            case Syntax::TypeSpecifier::Short: {
              Println("Short");
              break;
            }
            case Syntax::TypeSpecifier::Int: {
              Println("Int");
              break;
            }
            case Syntax::TypeSpecifier::Long: {
              Println("Long");
              break;
            }
            case Syntax::TypeSpecifier::Float: {
              Println("Float");
              break;
            }
            case Syntax::TypeSpecifier::Double: {
              Println("Double");
              break;
            }
            case Syntax::TypeSpecifier::Unsigned: {
              Println("Unsigned");
              break;
            }
            case Syntax::TypeSpecifier::Signed: {
              Println("Signed");
              break;
            }
            default:
              break;
            }
            DecAlign();
          },
      [](const std::unique_ptr<Syntax::StructOrUnionSpecifier> & structOrUnionSpecifier) {
           Print("StructOrUnionSpecifier");
           llvm::outs() << &structOrUnionSpecifier << " " << structOrUnionSpecifier->isUnion() << ", "
                        << structOrUnionSpecifier->getTag() << "\n";
           {
             IncAlign();
             for (const auto &structDeclaration : structOrUnionSpecifier->getStructDeclarations()) {
               visitor(structDeclaration.specifierQualifiers);
               for (const auto &structDeclarator : structDeclaration.structDeclarators) {
                 if (structDeclarator.optionalDeclarator) {
                   visitor(*structDeclarator.optionalDeclarator);
                 }
                 if (structDeclarator.optionalBitfield) {
                   visitor(*structDeclarator.optionalBitfield);
                 }
               }
             }
             DecAlign();
           }
      },
      [](const std::unique_ptr<Syntax::EnumSpecifier> & enumSpecifier) {
           Print("EnumSpecifier");
           llvm::outs() << &enumSpecifier << " " << enumSpecifier->getName() << "\n";
           {
              IncAlign();
              for (const auto &enumerator : enumSpecifier->getEnumerators()) {
                Print(enumerator.mName);
                if (enumerator.mValue) {
                  visitor(enumerator.mValue.value());
                }
              }
              DecAlign();
           }
      },
      [](const std::string_view& stringView) {
          Println(stringView);
      }
  }, typeSpecifier.getVariant());
  DecAlign();
}
void visitor(const Syntax::FunctionSpecifier &functionSpecifier ) {
  Print("FunctionSpecifier");
  llvm::outs() << &functionSpecifier << "\n";
  IncAlign();
  Println("inline");
  DecAlign();
}

//void visitor(const Syntax::SpecifierQualifiers &specifierQualifiers) {
//  Print("SpecifierQualifiers");
//  llvm::outs() << &specifierQualifiers << "\n";
//  IncAlign();
//  for (const auto &qualifier : specifierQualifiers.getTypeQualifiers()) {
//    visitor(qualifier);
//  }
//  for (const auto &specifier : specifierQualifiers.getTypeSpecifiers()) {
//    visitor(specifier);
//  }
//  DecAlign();
//}

void visitor(const Syntax::Pointer &pointer) {
  Print("Pointer");
  llvm::outs() << &pointer << "\n";
  IncAlign();
  for (const auto &p : pointer.getTypeQualifiers()) {
    visitor(p);
  }
  DecAlign();
}

void visitor(const Syntax::DirectDeclarator &directDeclarator) {
//  Print("DirectDeclarator");
//  llvm::outs() << &directDeclarator << "\n";
//  IncAlign();
  std::visit(overload{
      [](const Syntax::DirectDeclaratorIdent &ident) {
         Print("DirectDeclaratorIdent");
         llvm::outs() << &ident << "\n";
         IncAlign();
         Println(ident.getIdent());
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorParentheses &directDeclaratorParent) {
         Print("DirectDeclaratorParentheses");
         llvm::outs() << &directDeclaratorParent << "\n";
         IncAlign();
         visitor(*directDeclaratorParent.getDeclarator());
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorAssignExpr &directDeclaratorAssignExpr) {
         Print("DirectDeclaratorAssignExpr");
         llvm::outs() << &directDeclaratorAssignExpr << "\n";
         IncAlign();
         visitor(*directDeclaratorAssignExpr.getDirectDeclarator());
         if (directDeclaratorAssignExpr.hasStatic()) {
           Println("has static");
         }
         for (const auto &typeQualifier : directDeclaratorAssignExpr.getTypeQualifierList()) {
           visitor(typeQualifier);
         }
         if (directDeclaratorAssignExpr.getAssignmentExpression()) {
           visitor(*directDeclaratorAssignExpr.getAssignmentExpression());
         }
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorParamTypeList
                 &directDeclaratorParentParamTypeList) {
         Print("DirectDeclaratorParamTypeList");
         llvm::outs() << &directDeclaratorParentParamTypeList << "\n";
         IncAlign();
         visitor(*directDeclaratorParentParamTypeList.getDirectDeclarator());
         visitor(directDeclaratorParentParamTypeList.getParameterTypeList());
         DecAlign();
      },
     [](const Syntax::DirectDeclaratorAsterisk &directDeclaratorAsterisk) {
       Print("DirectDeclaratorAsterisk");
       llvm::outs() << &directDeclaratorAsterisk << "\n";
       IncAlign();
       visitor(*directDeclaratorAsterisk.getDirectDeclarator());
       for (const auto &typeQualifier : directDeclaratorAsterisk.getTypeQualifierList()) {
         visitor(typeQualifier);
       }
       DecAlign();
     },
  }, directDeclarator);
 // DecAlign();
}

void visitor(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator) {
//  Print("DirectAbstractDeclarator");
//  llvm::outs() << &directAbstractDeclarator << "\n";
  IncAlign();
  std::visit(overload{
                 [](const Syntax::DirectAbstractDeclaratorParentheses
                        &directAbstractDeclaratorParent) {
                   Print("DirectAbstractDeclaratorParentheses");
                   llvm::outs() << &directAbstractDeclaratorParent << "\n";
                   IncAlign();
                   visitor(*directAbstractDeclaratorParent.getAbstractDeclarator());
                   DecAlign();
                 },
                 [](const Syntax::DirectAbstractDeclaratorAssignExpr &directAbstractDeclaratorAssignExpr) {
                   Print("DirectAbstractDeclaratorAssignExpr");
                   llvm::outs() << &directAbstractDeclaratorAssignExpr << "\n";
                   IncAlign();
                   if (directAbstractDeclaratorAssignExpr.getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorAssignExpr
                                  .getDirectAbstractDeclarator());
                   }
                   if (directAbstractDeclaratorAssignExpr.hasStatic()) {
                     Println("has static");
                   }
                   for (const auto &typeQualifier : directAbstractDeclaratorAssignExpr.getTypeQualifiers()) {
                     visitor(typeQualifier);
                   }
                   if (directAbstractDeclaratorAssignExpr.getAssignmentExpression()) {
                     visitor(*directAbstractDeclaratorAssignExpr
                                  .getAssignmentExpression());
                   }
                   DecAlign();
                 },
                 [](const Syntax::DirectAbstractDeclaratorParamTypeList &directAbstractDeclaratorParamTypeList) {
                   Print("DirectAbstractDeclaratorParamTypeList");
                   llvm::outs() << &directAbstractDeclaratorParamTypeList << "\n";
                   IncAlign();
                   if (directAbstractDeclaratorParamTypeList.getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorParamTypeList
                                  .getDirectAbstractDeclarator());
                   }
                   if (directAbstractDeclaratorParamTypeList.getParameterTypeList())
                    visitor(*directAbstractDeclaratorParamTypeList.getParameterTypeList());
                   DecAlign();
                 },
                 [](const Syntax::DirectAbstractDeclaratorAsterisk &directAbstractDeclaratorAsterisk) {
                   Print("DirectAbstractDeclaratorAsterisk");
                   llvm::outs() << &directAbstractDeclaratorAsterisk << "\n";
                   IncAlign();
                   if (directAbstractDeclaratorAsterisk.getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorAsterisk
                                  .getDirectAbstractDeclarator());
                   }
                   DecAlign();
                 },
             }, directAbstractDeclarator);
  DecAlign();
}

void visitor(const Syntax::ParamTypeList &paramTypeList) {
  Print("ParamTypeList");
  llvm::outs() << &paramTypeList << "\n";
  IncAlign();
  visitor(paramTypeList.getParameterList());
  if (paramTypeList.hasEllipse()) {
    Println("...");
  }
  DecAlign();
}

void visitor(const Syntax::ParamList &paramList) {
  Print("ParamList");
  llvm::outs() << &paramList << "\n";
  IncAlign();
  for (const auto &paramDecl : paramList.getParameterDeclarations()) {
    visitor(paramDecl.declarationSpecifiers);
    std::visit(overload{
        [](const std::unique_ptr<Syntax::Declarator> &declarator) {
           visitor(*declarator);
        },
        [](const std::optional<std::unique_ptr<Syntax::AbstractDeclarator>> &abstractDeclarator) {
            if (abstractDeclarator.has_value())
              visitor(*abstractDeclarator.value());
        },
    }, paramDecl.declarator);
  }
  DecAlign();
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
  IncAlign();
  std::visit(overload{
      [](const Syntax::Stmt &stmt) {
         std::visit(overload{
            [](const Syntax::IfStmt &ifStmt){ visitor(ifStmt);},
            [](const Syntax::ForStmt &forStmt){ visitor(forStmt);},
            [](const Syntax::WhileStmt &whileStmt){ visitor(whileStmt);},
            [](const Syntax::DoWhileStmt &doWhileStmt){ visitor(doWhileStmt);},
            [](const Syntax::BreakStmt &breakStmt){ visitor(breakStmt);},
            [](const Syntax::ContinueStmt &continueStmt){ visitor(continueStmt);},
            [](const Syntax::SwitchStmt &switchStmt){ visitor(switchStmt);},
            [](const Syntax::CaseStmt &caseStmt){ visitor(caseStmt);},
            [](const Syntax::DefaultStmt &defaultStmt){ visitor(defaultStmt);},
            [](const Syntax::ReturnStmt &returnStmt){ visitor(returnStmt);},
            [](const Syntax::ExprStmt &exprStmt){ visitor(exprStmt);},
            [](const Syntax::GotoStmt &gotoStmt){ visitor(gotoStmt);},
            [](const Syntax::LabelStmt &labelStmt){ visitor(labelStmt);},
            [](const Syntax::BlockStmt &blockStmt){ visitor(blockStmt);},
         }, stmt);
      },
      [](const Syntax::Declaration &declaration) {
         visitor(declaration);
      }
  }, blockItem);
  DecAlign();
}

void visitor(const Syntax::Stmt &stmt) {
  std::visit(overload{
                 [](const Syntax::IfStmt &ifStmt){ visitor(ifStmt);},
                 [](const Syntax::ForStmt &forStmt){ visitor(forStmt);},
                 [](const Syntax::WhileStmt &whileStmt){ visitor(whileStmt);},
                 [](const Syntax::DoWhileStmt &doWhileStmt){ visitor(doWhileStmt);},
                 [](const Syntax::BreakStmt &breakStmt){ visitor(breakStmt);},
                 [](const Syntax::ContinueStmt &continueStmt){ visitor(continueStmt);},
                 [](const Syntax::SwitchStmt &switchStmt){ visitor(switchStmt);},
                 [](const Syntax::CaseStmt &caseStmt){ visitor(caseStmt);},
                 [](const Syntax::DefaultStmt &defaultStmt){ visitor(defaultStmt);},
                 [](const Syntax::ReturnStmt &returnStmt){ visitor(returnStmt);},
                 [](const Syntax::ExprStmt &exprStmt){ visitor(exprStmt);},
                 [](const Syntax::GotoStmt &gotoStmt){ visitor(gotoStmt);},
                 [](const Syntax::LabelStmt &labelStmt){ visitor(labelStmt);},
                 [](const Syntax::BlockStmt &blockStmt){ visitor(blockStmt);},
             }, stmt);
}

void visitor(const Syntax::IfStmt &ifStmt){
  Print("IfStmt");
  llvm::outs() << &ifStmt << "\n";
  IncAlign();
  visitor(ifStmt.getExpression());
  visitor(*ifStmt.getThenStmt());
  if (ifStmt.getElseStmt()) {
    visitor(*ifStmt.getElseStmt());
  }
  DecAlign();
}
void visitor(const Syntax::ForStmt &forStmt){
  Print("ForStmt");
  llvm::outs() << &forStmt << "\n";
  IncAlign();
  std::visit(overload{
      [](const std::unique_ptr<Syntax::Declaration>& declaration) { visitor(*declaration);},
      [](const std::unique_ptr<Syntax::Expr>& expr) { visitor(*expr);},
  }, forStmt.getInitial());
  if (forStmt.getControlling())
    visitor(*forStmt.getControlling());
  if (forStmt.getPost())
    visitor(*forStmt.getPost());
  if (forStmt.getStatement()) {
    visitor(*forStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::WhileStmt &whileStmt){
  Print("WhileStmt");
  llvm::outs() << &whileStmt << "\n";
  IncAlign();
  visitor(whileStmt.getExpression());
  if (whileStmt.getStatement()) {
    visitor(*whileStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::DoWhileStmt &doWhileStmt){
  Print("DoWhileStmt");
  llvm::outs() << &doWhileStmt << "\n";
  IncAlign();
  if (doWhileStmt.getStatement()) {
    visitor(*doWhileStmt.getStatement());
  }
  visitor(doWhileStmt.getExpression());
  DecAlign();
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
  IncAlign();
  visitor(switchStmt.getExpression());
  if (switchStmt.getStatement()) {
    visitor(*switchStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::CaseStmt &caseStmt){
  Print("CaseStmt");
  llvm::outs() << &caseStmt << "\n";
  IncAlign();
  visitor(caseStmt.getConstantExpr());
  if (caseStmt.getStatement()) {
    visitor(*caseStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::DefaultStmt &defaultStmt){
  Print("DefaultStmt");
  llvm::outs() << &defaultStmt << "\n";
  IncAlign();
  if (defaultStmt.getStatement()) {
    visitor(*defaultStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::GotoStmt &gotoStmt){
  Print("GotoStmt");
  llvm::outs() << &gotoStmt << "\n";
  IncAlign();
  Println(gotoStmt.getIdentifier());
  DecAlign();
}
void visitor(const Syntax::LabelStmt &labelStmt){
  Print("LabelStmt");
  llvm::outs() << &labelStmt << "\n";
  IncAlign();
  Println(labelStmt.getIdentifier());
  DecAlign();
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
    IncAlign();
    visitor(*returnStmt.getExpression());
    DecAlign();
  }
}

void visitor(const Syntax::Expr &expr) {
  Print("Expr");
  llvm::outs() << &expr << "\n";
  IncAlign();
  for (const auto &assignExpr : expr.getAssignExpressions()) {
    visitor(assignExpr);
  }
  DecAlign();
}

void visitor(const Syntax::AssignExpr &assignExpr) {
  Print("AssignExpr");
  llvm::outs() << &assignExpr << "\n";
  IncAlign();
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
  DecAlign();
}

/// conditionalExpr
void visitor(const Syntax::ConstantExpr &constantExpr) {
  Print("ConditionalExpr");
  llvm::outs() << &constantExpr << "\n";
  IncAlign();
  visitor(constantExpr.getLogicalOrExpression());
  if (constantExpr.getOptionalExpression()) {
    visitor(*constantExpr.getOptionalExpression());
  }
  if (constantExpr.getOptionalConditionalExpression()) {
    visitor(*constantExpr.getOptionalConditionalExpression());
  }
  DecAlign();
}
void visitor(const Syntax::LogOrExpr &logOrExpr){
  Print("LogOrExpr");
  llvm::outs() << &logOrExpr << "\n";
  IncAlign();
  visitor(logOrExpr.getAndExpression());
  for (const auto &logAndExpr: logOrExpr.getOptionalAndExpressions()) {
    visitor(logAndExpr);
  }
  DecAlign();
}
void visitor(const Syntax::LogAndExpr &logAndExpr){
  Print("LogAndExpr");
  llvm::outs() << &logAndExpr << "\n";
  IncAlign();
  visitor(logAndExpr.getBitOrExpression());
  for (const auto &bitOrExpr: logAndExpr.getOptionalBitOrExpressions()) {
    visitor(bitOrExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitOrExpr &bitOrExpr){
  Print("BitOrExpr");
  llvm::outs() << &bitOrExpr << "\n";
  IncAlign();
  visitor(bitOrExpr.getBitXorExpression());
  for (const auto &bitXorExpr: bitOrExpr.getOptionalBitXorExpressions()) {
    visitor(bitXorExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitXorExpr &bitXorExpr){
  Print("BitXorExpr");
  llvm::outs() << &bitXorExpr << "\n";
  IncAlign();
  visitor(bitXorExpr.getBitAndExpr());
  for (const auto &bitAndExpr: bitXorExpr.getOptionalBitAndExpressions()) {
    visitor(bitAndExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitAndExpr &bitAndExpr){
  Print("BitAndExpr");
  llvm::outs() << &bitAndExpr << "\n";
  IncAlign();
  visitor(bitAndExpr.getEqualExpr());
  for (const auto &equalExpr: bitAndExpr.getOptionalEqualExpr()) {
    visitor(equalExpr);
  }
  DecAlign();
}
void visitor(const Syntax::EqualExpr &equalExpr){
  Print("EqualExpr");
  llvm::outs() << &equalExpr << "\n";
  IncAlign();
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
  DecAlign();
}
void visitor(const Syntax::RelationalExpr &relationalExpr){
  Print("RelationalExpr");
  llvm::outs() << &relationalExpr << "\n";
  IncAlign();
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
  DecAlign();
}
void visitor(const Syntax::ShiftExpr &shiftExpr){
  Print("ShiftExpr");
  llvm::outs() << &shiftExpr << "\n";
  IncAlign();
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
  DecAlign();
}
void visitor(const Syntax::AdditiveExpr &additiveExpr){
  Print("AdditiveExpr");
  llvm::outs() << &additiveExpr << "\n";
  IncAlign();
  visitor(additiveExpr.getMultiExpr());
  for (const auto &multiExpr: additiveExpr.getOptionalMultiExpr()) {
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
  DecAlign();
}
void visitor(const Syntax::MultiExpr &multiExpr){
  Print("MultiExpr");
  llvm::outs() << &multiExpr << "\n";
  IncAlign();
  visitor(multiExpr.getCastExpr());
  for (const auto &castExpr: multiExpr.getOptionalCastExpr()) {
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
  DecAlign();
}
void visitor(const Syntax::CastExpr &castExpr){
  Print("CastExpr");
  llvm::outs() << &castExpr << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::UnaryExpr &unaryExpr) {visitor(unaryExpr);},
      [](const std::pair<Syntax::TypeName, std::unique_ptr<Syntax::CastExpr>> &pair) {
         visitor(pair.first);
         visitor(*pair.second);
      }
  }, castExpr.getVariant());
  DecAlign();
}
void visitor(const Syntax::UnaryExpr &unaryExpr){
  std::visit(overload{
    [](const Syntax::UnaryExprPostFixExpr &unaryExprPostFixExpr) {
       Print("UnaryExprPostFixExpr");
       llvm::outs() << &unaryExprPostFixExpr << "\n";
       visitor(unaryExprPostFixExpr.getPostExpr());
    },
    [](const Syntax::UnaryExprUnaryOperator &unaryExprUnaryOperator) {
       Print("UnaryExprUnaryOperator");
       llvm::outs() << &unaryExprUnaryOperator << "\n";
       IncAlign();
       switch (unaryExprUnaryOperator.getOperator()) {
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Increment: {
         Println("++");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Decrement: {
         Println("--");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Ampersand: {
         Println("&");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Asterisk:{
         Println("*");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Plus:{
         Println("+");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::Minus:{
         Println("-");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::BitNot:{
         Println("~");
         break;
       }
       case Syntax::UnaryExprUnaryOperator::UnaryOperator::LogicalNot:{
         Println("!");
         break;
       }
       default:
         break;
       }
       visitor(*unaryExprUnaryOperator.getCastExpr());
       DecAlign();
    },
    [](const Syntax::UnaryExprSizeOf &unaryExprSizeOf) {
       Print("UnaryExprSizeOf");
       llvm::outs() << &unaryExprSizeOf << "\n";
       IncAlign();
       std::visit(overload{
         [](const std::unique_ptr<Syntax::UnaryExpr>& unaryExpr){visitor(*unaryExpr);},
         [](const std::unique_ptr<Syntax::TypeName>& typeName){visitor(*typeName);},
       },
       unaryExprSizeOf.getVariant());
       DecAlign();
    },
  }, unaryExpr);
}
void visitor(const Syntax::TypeName &typeName){
  Print("TypeName");
  llvm::outs() << &typeName << "\n";
  IncAlign();
  visitor(typeName.getSpecifierQualifiers());
  if (typeName.getAbstractDeclarator())
    visitor(*typeName.getAbstractDeclarator());
  DecAlign();
}
void visitor(const Syntax::PostFixExpr &postFixExpr){
  std::visit(overload{
     [](const Syntax::PostFixExprPrimaryExpr &primaryExpr) {
          IncAlign();
          Print("PostFixExprPrimaryExpr");
          llvm::outs() << &primaryExpr << "\n";
          visitor(primaryExpr.getPrimaryExpr());
          DecAlign();
     },
     [](const Syntax::PostFixExprSubscript &subscript) {
         IncAlign();
         Print("PostFixExprSubscript");
         llvm::outs() << &subscript << "\n";
         visitor(*subscript.getPostFixExpr());
         visitor(subscript.getExpr());
         DecAlign();
     },
     [](const Syntax::PostFixExprFuncCall &funcCall) {
         IncAlign();
         Print("PostFixExprFuncCall");
         llvm::outs() << &funcCall << "\n";
         visitor(*funcCall.getPostFixExpr());
         for (const auto &assignExpr : funcCall.getOptionalAssignExpressions()) {
           visitor(*assignExpr);
         }
         DecAlign();
     },
     [](const Syntax::PostFixExprDot &dot) {
         IncAlign();
         Print("PostFixExprDot");
         llvm::outs() << &dot << "\n";
         visitor(*dot.getPostFixExpr());
         Println(dot.getIdentifier());
         DecAlign();
       },
     [](const Syntax::PostFixExprArrow &arrow) {
         IncAlign();
         Print("PostFixExprArrow");
         llvm::outs() << &arrow << "\n";
         visitor(*arrow.getPostFixExpr());
         Println(arrow.getIdentifier());
         DecAlign();
       },
     [](const Syntax::PostFixExprIncrement &increment) {
         IncAlign();
         Print("PostFixExprIncrement");
         llvm::outs() << &increment << "\n";
         visitor(*increment.getPostFixExpr());
         DecAlign();
     },
     [](const Syntax::PostFixExprDecrement &decrement) {
         IncAlign();
         Print("PostFixExprDecrement");
         llvm::outs() << &decrement << "\n";
         visitor(*decrement.getPostFixExpr());
         DecAlign();
     },
     [](const Syntax::PostFixExprTypeInitializer &typeInitializer) {
         IncAlign();
         Print("PostFixExprTypeInitializer");
         llvm::outs() << &typeInitializer << "\n";
         visitor(*typeInitializer.getTypeName());
         visitor(*typeInitializer.getInitializerList());
         DecAlign();
     },
  }, postFixExpr);
}
void visitor(const Syntax::PrimaryExpr &primaryExpr){
  std::visit(overload{
     [](const Syntax::PrimaryExprIdent& ident) {
       IncAlign();
       Print("PrimaryExprIdent");
       llvm::outs() << &ident << "\n";
       {
         IncAlign();
         Println(ident.getIdentifier());
         DecAlign();
       }
       DecAlign();
     },
     [](const Syntax::PrimaryExprConstant& constant) {
       IncAlign();
       Print("PrimaryExprConstant");
       llvm::outs() << &constant << "\n";
       std::visit([](auto &&value) {
         IncAlign();
         using T = std::decay_t<decltype(value)>;
         if constexpr (std::is_same_v<T, std::string>) {
           Println(value);
         }else {
           Println(std::to_string(value));
         }
         DecAlign();
       }, constant.getValue());
       DecAlign();
     },
     [](const Syntax::PrimaryExprParentheses & parent) {
         IncAlign();
         Print("PrimaryExprParentheses");
         llvm::outs() << &parent << "\n";
         visitor(parent.getExpr());
         DecAlign();
     },
  }, primaryExpr);
}
}
