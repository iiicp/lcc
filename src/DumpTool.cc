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

static uint64_t LeftAlign = 0;

void IncAlign() {
  LeftAlign++;
}

void DecAlign() {
  assert(LeftAlign > 0);
  LeftAlign--;
}

void PrintContent(std::string content, bool needNewLine) {
  std::string ws(LeftAlign, ' ');
  llvm::outs() << ws << content;
  if (needNewLine) {
    llvm::outs() << "\n";
  }else {
    llvm::outs() << " ";
  }
}

void dumpTokens(const std::vector<lcc::Token> &tokens) {
  for (auto &tok : tokens) {
    llvm::outs() << tok.getLine() << ", " << tok.getColumn() << ", " << tok.getRepresentation() << "\n";
  }
}

void dumpAst(const lcc::Syntax::TranslationUnit &unit) {
  visitor(unit);
}

void visitor(const Syntax::TranslationUnit &unit) {
  PrintContent("TranslationUnit");
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
  PrintContent("Declaration");
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
  PrintContent("FunctionDefinition");
  llvm::outs() << &functionDefinition << "\n";
  IncAlign();
  visitor(functionDefinition.getDeclarationSpecifiers());
  visitor(functionDefinition.getDeclarator());
  visitor(functionDefinition.getCompoundStatement());
  DecAlign();
}
void visitor(const Syntax::DeclarationSpecifiers &declarationSpecifiers) {
  PrintContent("DeclarationSpecifiers");
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
  PrintContent("Declarator");
  llvm::outs() << &declarator << "\n";
  IncAlign();
  for (const auto &point : declarator.getPointers()) {
    visitor(point);
  }
  visitor(declarator.getDirectDeclarator());
  DecAlign();
}

void visitor(const Syntax::AbstractDeclarator &abstractDeclarator) {
  PrintContent("AbstractDeclarator");
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
  PrintContent("Initializer");
  llvm::outs() << &initializer << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::AssignExpr &assignExpr) {
         visitor(assignExpr);
      },[](const Syntax::InitializerList &initializerList) {
       visitor(initializerList);}
  }, initializer.getVariant());
  DecAlign();
}

void visitor(const Syntax::InitializerList &initializerList) {
  PrintContent("InitializerList");
  llvm::outs() << &initializerList << "\n";
  IncAlign();
  for (const auto &vec : initializerList.getInitializerList()) {
    for (const auto &designator : vec.second) {
      std::visit(overload{
          [](const Syntax::ConstantExpr &constantExpr) {
             visitor(constantExpr);
          },
          [](const std::string &ident) {
             PrintContent(ident, true);
          },
      }, designator);
    }
    visitor(vec.first);
  }
  DecAlign();
}

void visitor(const Syntax::StorageClassSpecifier &storageClassSpecifier) {
  PrintContent("StorageClassSpecifier");
  llvm::outs() << &storageClassSpecifier << "\n";
  IncAlign();
  switch (storageClassSpecifier.getSpecifier()) {
  case Syntax::StorageClassSpecifier::Typedef:
    PrintContent("Typedef", true);
    break;
  case Syntax::StorageClassSpecifier::Extern:
    PrintContent("Extern", true);
    break;
  case Syntax::StorageClassSpecifier::Static:
    PrintContent("Static", true);
    break;
  case Syntax::StorageClassSpecifier::Auto:
    PrintContent("Auto", true);
    break;
  case Syntax::StorageClassSpecifier::Register:
    PrintContent("Register", true);
    break;
  default:
    break;
  }
  DecAlign();
}
void visitor(const Syntax::TypeQualifier &typeQualifier) {
  PrintContent("TypeQualifier");
  llvm::outs() << &typeQualifier << "\n";
  IncAlign();
  switch (typeQualifier.getQualifier()) {
  case Syntax::TypeQualifier::Const:
    PrintContent("Const", true);
    break;
  case Syntax::TypeQualifier::Volatile:
    PrintContent("Volatile", true);
    break;
  case Syntax::TypeQualifier::Restrict:
    PrintContent("Restrict", true);
    break;
  default:
    break;
  }
  DecAlign();
}
void visitor(const Syntax::TypeSpecifier &typeSpecifier) {
  PrintContent("TypeSpecifier");
  llvm::outs() << &typeSpecifier << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::TypeSpecifier::PrimitiveTypeSpecifier & primitiveTypeSpecifier) {
            PrintContent("PrimitiveTypeSpecifier");
            llvm::outs() << &primitiveTypeSpecifier << "\n";
            switch (primitiveTypeSpecifier) {
            case Syntax::TypeSpecifier::Void: {
              IncAlign();
              PrintContent("Void", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Char: {
              IncAlign();
              PrintContent("Char", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Bool: {
              IncAlign();
              PrintContent("Bool", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Short: {
              IncAlign();
              PrintContent("Short", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Int: {
              IncAlign();
              PrintContent("Int", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Long: {
              IncAlign();
              PrintContent("Long", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Float: {
              IncAlign();
              PrintContent("Float", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Double: {
              IncAlign();
              PrintContent("Double", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Unsigned: {
              IncAlign();
              PrintContent("Unsigned", true);
              DecAlign();
              break;
            }
            case Syntax::TypeSpecifier::Signed: {
              IncAlign();
              PrintContent("Signed", true);
              DecAlign();
              break;
            }
            default:
              break;
            }
          },
      [](const std::unique_ptr<Syntax::StructOrUnionSpecifier> & structOrUnionSpecifier) {
           PrintContent("StructOrUnionSpecifier");
           llvm::outs() << &structOrUnionSpecifier << " " << structOrUnionSpecifier->isUnion() << ", "
                        << structOrUnionSpecifier->getName() << "\n";
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
           PrintContent("EnumSpecifier");
           llvm::outs() << &enumSpecifier << " " << enumSpecifier->getName() << "\n";
           {
              IncAlign();
              for (const auto &enumerator : enumSpecifier->getEnumerators()) {
                PrintContent(enumerator.mName);
                if (enumerator.mValue) {
                  visitor(enumerator.mValue.value());
                }
              }
              DecAlign();
           }
      },
      [](const std::string_view& stringView) {
          llvm::outs() << stringView << "\n";
      }
  }, typeSpecifier.getVariant());
  DecAlign();
}
void visitor(const Syntax::FunctionSpecifier &functionSpecifier ) {
  PrintContent("FunctionSpecifier");
  llvm::outs() << &functionSpecifier << "\n";
  IncAlign();
  PrintContent("inline", true);
  DecAlign();
}

void visitor(const Syntax::SpecifierQualifiers &specifierQualifiers) {
  PrintContent("SpecifierQualifiers");
  llvm::outs() << &specifierQualifiers << "\n";
  IncAlign();
  for (const auto &qualifier : specifierQualifiers.getTypeQualifiers()) {
    visitor(qualifier);
  }
  for (const auto &specifier : specifierQualifiers.getTypeSpecifiers()) {
    visitor(specifier);
  }
  DecAlign();
}

void visitor(const Syntax::Pointer &pointer) {
  PrintContent("Pointer");
  llvm::outs() << &pointer << "\n";
  IncAlign();
  for (const auto &p : pointer.getTypeQualifiers()) {
    visitor(p);
  }
  DecAlign();
}

void visitor(const Syntax::DirectDeclarator &directDeclarator) {
//  PrintContent("DirectDeclarator");
//  llvm::outs() << &directDeclarator << "\n";
  IncAlign();
  std::visit(overload{
      [](const Syntax::DirectDeclaratorIdent &ident) {
         PrintContent("DirectDeclaratorIdent");
         llvm::outs() << &ident << "\n";
         IncAlign();
         PrintContent(ident.getIdentifierLoc(), true);
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorParent &directDeclaratorParent) {
         PrintContent("DirectDeclaratorParent");
         llvm::outs() << &directDeclaratorParent << "\n";
         IncAlign();
         visitor(*directDeclaratorParent.getDeclarator());
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorAssignExpr &directDeclaratorAssignExpr) {
         PrintContent("DirectDeclaratorAssignExpr");
         llvm::outs() << &directDeclaratorAssignExpr << "\n";
         IncAlign();
         visitor(*directDeclaratorAssignExpr.getDirectDeclarator());
         visitor(*directDeclaratorAssignExpr.getAssignmentExpression());
         DecAlign();
      },
      [](const Syntax::DirectDeclaratorParentParamTypeList &directDeclaratorParentParamTypeList) {
         PrintContent("DirectDeclaratorParentParamTypeList");
         llvm::outs() << &directDeclaratorParentParamTypeList << "\n";
         IncAlign();
         visitor(*directDeclaratorParentParamTypeList.getDirectDeclarator());
         visitor(directDeclaratorParentParamTypeList.getParameterTypeList());
         DecAlign();
      },
  }, directDeclarator);
  DecAlign();
}

void visitor(const Syntax::DirectAbstractDeclarator &directAbstractDeclarator) {
//  PrintContent("DirectAbstractDeclarator");
//  llvm::outs() << &directAbstractDeclarator << "\n";
  IncAlign();
  std::visit(overload{
                 [](const Syntax::DirectAbstractDeclaratorParent &directAbstractDeclaratorParent) {
                   PrintContent("DirectAbstractDeclaratorParent");
                   llvm::outs() << &directAbstractDeclaratorParent << "\n";
                   IncAlign();
                   visitor(*directAbstractDeclaratorParent.getAbstractDeclarator());
                   DecAlign();
                 },
                 [](const Syntax::DirectAbstractDeclaratorAssignExpr &directAbstractDeclaratorAssignExpr) {
                   PrintContent("DirectAbstractDeclaratorAssignExpr");
                   llvm::outs() << &directAbstractDeclaratorAssignExpr << "\n";
                   IncAlign();
                   if (directAbstractDeclaratorAssignExpr.getDirectAbstractDeclarator()) {
                     visitor(*directAbstractDeclaratorAssignExpr
                                  .getDirectAbstractDeclarator());
                   }
                   visitor(*directAbstractDeclaratorAssignExpr.getAssignmentExpression());
                   DecAlign();
                 },
                 [](const Syntax::DirectAbstractDeclaratorParamTypeList &directAbstractDeclaratorParamTypeList) {
                   PrintContent("DirectAbstractDeclaratorParamTypeList");
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
             }, directAbstractDeclarator);
  DecAlign();
}

void visitor(const Syntax::ParamTypeList &paramTypeList) {
  PrintContent("ParamTypeList");
  llvm::outs() << &paramTypeList << "\n";
  IncAlign();
  visitor(paramTypeList.getParameterList());
  if (paramTypeList.hasEllipse()) {
    llvm::outs() << "...\n";
  }
  DecAlign();
}

void visitor(const Syntax::ParamList &paramList) {
  PrintContent("ParamList");
  llvm::outs() << &paramList << "\n";
  IncAlign();
  for (const auto &paramDecl : paramList.getParameterDeclarations()) {
    visitor(paramDecl.declarationSpecifiers);
    std::visit(overload{
        [](const std::unique_ptr<Syntax::Declarator> &declarator) {
           visitor(*declarator);
        },
        [](const std::unique_ptr<Syntax::AbstractDeclarator> &abstractDeclarator) {
           visitor(*abstractDeclarator);
        },
    }, paramDecl.declarator);
  }
  DecAlign();
}

void visitor(const Syntax::BlockStmt &blockStmt) {
  PrintContent("BlockStmt");
  llvm::outs() << &blockStmt << "\n";
  IncAlign();
  for (const auto &blockItem : blockStmt.getBlockItems()) {
    visitor(blockItem);
  }
  DecAlign();
}

void visitor(const Syntax::BlockItem &blockItem) {
//  PrintContent("BlockItem");
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
  PrintContent("IfStmt");
  llvm::outs() << &ifStmt << "\n";
  IncAlign();
  std::string ws(LeftAlign, ' ');
  visitor(ifStmt.getExpression());
  visitor(*ifStmt.getThenStmt());
  if (ifStmt.getElseStmt()) {
    visitor(*ifStmt.getElseStmt());
  }
  DecAlign();
}
void visitor(const Syntax::ForStmt &forStmt){
  PrintContent("ForStmt");
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
  PrintContent("WhileStmt");
  llvm::outs() << &whileStmt << "\n";
  IncAlign();
  visitor(whileStmt.getExpression());
  if (whileStmt.getStatement()) {
    visitor(*whileStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::DoWhileStmt &doWhileStmt){
  PrintContent("DoWhileStmt");
  llvm::outs() << &doWhileStmt << "\n";
  IncAlign();
  if (doWhileStmt.getStatement()) {
    visitor(*doWhileStmt.getStatement());
  }
  visitor(doWhileStmt.getExpression());
  DecAlign();
}
void visitor(const Syntax::BreakStmt &breakStmt){
  PrintContent("BreakStmt");
  llvm::outs() << &breakStmt << "\n";
}
void visitor(const Syntax::ContinueStmt &continueStmt){
  PrintContent("ContinueStmt");
  llvm::outs() << &continueStmt << "\n";
}
void visitor(const Syntax::SwitchStmt &switchStmt){
  PrintContent("SwitchStmt");
  llvm::outs() << &switchStmt << "\n";
  IncAlign();
  visitor(switchStmt.getExpression());
  if (switchStmt.getStatement()) {
    visitor(*switchStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::CaseStmt &caseStmt){
  PrintContent("CaseStmt");
  llvm::outs() << &caseStmt << "\n";
  IncAlign();
  visitor(caseStmt.getConstantExpr());
  if (caseStmt.getStatement()) {
    visitor(*caseStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::DefaultStmt &defaultStmt){
  PrintContent("DefaultStmt");
  llvm::outs() << &defaultStmt << "\n";
  IncAlign();
  if (defaultStmt.getStatement()) {
    visitor(*defaultStmt.getStatement());
  }
  DecAlign();
}
void visitor(const Syntax::GotoStmt &gotoStmt){
  PrintContent("GotoStmt");
  llvm::outs() << &gotoStmt << "\n";
  IncAlign();
  PrintContent(gotoStmt.getIdentifier(), true);
  DecAlign();
}
void visitor(const Syntax::LabelStmt &labelStmt){
  PrintContent("LabelStmt");
  llvm::outs() << &labelStmt << "\n";
  IncAlign();
  PrintContent(labelStmt.getIdentifier(), true);
  DecAlign();
}
void visitor(const Syntax::ExprStmt &exprStmt){
  PrintContent("ExprStmt");
  llvm::outs() << &exprStmt << "\n";
  if (exprStmt.getOptionalExpression()) {
    visitor(*exprStmt.getOptionalExpression());
  }
}
void visitor(const Syntax::ReturnStmt &returnStmt){
  PrintContent("ReturnStmt");
  llvm::outs() << &returnStmt << "\n";
  if (returnStmt.getExpression()) {
    visitor(*returnStmt.getExpression());
  }
}

void visitor(const Syntax::Expr &expr) {
  PrintContent("Expr");
  llvm::outs() << &expr << "\n";
  IncAlign();
  for (const auto &assignExpr : expr.getAssignExpressions()) {
    visitor(assignExpr);
  }
  DecAlign();
}

void visitor(const Syntax::AssignExpr &assignExpr) {
  PrintContent("AssignExpr");
  llvm::outs() << &assignExpr << "\n";
  IncAlign();
  visitor(assignExpr.getConditionalExpr());
  for (const auto &pair : assignExpr.getOptionalConditionalExpr()) {
    switch (pair.first) {
    case Syntax::AssignExpr::Assign: {
      PrintContent("Assign");
      break;
    }
    case Syntax::AssignExpr::PlusAssign: {
      PrintContent("PlusAssign");
      break;
    }
    case Syntax::AssignExpr::MinusAssign: {
      PrintContent("MinusAssign");
      break;
    }
    case Syntax::AssignExpr::MultiplyAssign: {
      PrintContent("MultiplyAssign");
      break;
    }
    case Syntax::AssignExpr::DivideAssign: {
      PrintContent("DivideAssign");
      break;
    }
    case Syntax::AssignExpr::ModuloAssign: {
      PrintContent("ModuloAssign");
      break;
    }
    case Syntax::AssignExpr::LeftShiftAssign: {
      PrintContent("LeftShiftAssign");
      break;
    }
    case Syntax::AssignExpr::RightShiftAssign: {
      PrintContent("RightShiftAssign");
      break;
    }
    case Syntax::AssignExpr::BitAndAssign: {
      PrintContent("BitAndAssign");
      break;
    }
    case Syntax::AssignExpr::BitOrAssign: {
      PrintContent("BitOrAssign");
      break;
    }
    case Syntax::AssignExpr::BitXorAssign: {
      PrintContent("BitXorAssign");
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
  PrintContent("ConditionalExpr");
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
  PrintContent("LogOrExpr");
  llvm::outs() << &logOrExpr << "\n";
  IncAlign();
  visitor(logOrExpr.getAndExpression());
  for (const auto &logAndExpr: logOrExpr.getOptionalAndExpressions()) {
    visitor(logAndExpr);
  }
  DecAlign();
}
void visitor(const Syntax::LogAndExpr &logAndExpr){
  PrintContent("LogAndExpr");
  llvm::outs() << &logAndExpr << "\n";
  IncAlign();
  visitor(logAndExpr.getBitOrExpression());
  for (const auto &bitOrExpr: logAndExpr.getOptionalBitOrExpressions()) {
    visitor(bitOrExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitOrExpr &bitOrExpr){
  PrintContent("BitOrExpr");
  llvm::outs() << &bitOrExpr << "\n";
  IncAlign();
  visitor(bitOrExpr.getBitXorExpression());
  for (const auto &bitXorExpr: bitOrExpr.getOptionalBitXorExpressions()) {
    visitor(bitXorExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitXorExpr &bitXorExpr){
  PrintContent("BitXorExpr");
  llvm::outs() << &bitXorExpr << "\n";
  IncAlign();
  visitor(bitXorExpr.getBitAndExpr());
  for (const auto &bitAndExpr: bitXorExpr.getOptionalBitAndExpressions()) {
    visitor(bitAndExpr);
  }
  DecAlign();
}
void visitor(const Syntax::BitAndExpr &bitAndExpr){
  PrintContent("BitAndExpr");
  llvm::outs() << &bitAndExpr << "\n";
  IncAlign();
  visitor(bitAndExpr.getEqualExpr());
  for (const auto &equalExpr: bitAndExpr.getOptionalEqualExpr()) {
    visitor(equalExpr);
  }
  DecAlign();
}
void visitor(const Syntax::EqualExpr &equalExpr){
  PrintContent("EqualExpr");
  llvm::outs() << &equalExpr << "\n";
  IncAlign();
  visitor(equalExpr.getRelationalExpr());
  for (const auto &relationExpr: equalExpr.getOptionalRelationalExpr()) {
    switch (relationExpr.first) {
    case Syntax::EqualExpr::Equal: {
      PrintContent("Equal");
      break;
    }
    case Syntax::EqualExpr::NotEqual: {
      PrintContent("NotEqual");
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
  PrintContent("RelationalExpr");
  llvm::outs() << &relationalExpr << "\n";
  IncAlign();
  visitor(relationalExpr.getShiftExpr());
  for (const auto &shiftExpr: relationalExpr.getOptionalShiftExpressions()) {
    switch (shiftExpr.first) {
    case Syntax::RelationalExpr::LessThan: {
      PrintContent("LessThan");
      break;
    }
    case Syntax::RelationalExpr::LessThanOrEqual: {
      PrintContent("LessThanOrEqual");
      break;
    }
    case Syntax::RelationalExpr::GreaterThan: {
      PrintContent("GreaterThan");
      break;
    }
    case Syntax::RelationalExpr::GreaterThanOrEqual: {
      PrintContent("GreaterThanOrEqual");
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
  PrintContent("ShiftExpr");
  llvm::outs() << &shiftExpr << "\n";
  IncAlign();
  visitor(shiftExpr.getAdditiveExpr());
  for (const auto &additiveExpr: shiftExpr.getOptAdditiveExps()) {
    switch (additiveExpr.first) {
    case Syntax::ShiftExpr::Left: {
      PrintContent("Shift Left");
      break;
    }
    case Syntax::ShiftExpr::Right: {
      PrintContent("Shift Right");
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
  PrintContent("AdditiveExpr");
  llvm::outs() << &additiveExpr << "\n";
  IncAlign();
  visitor(additiveExpr.getMultiExpr());
  for (const auto &multiExpr: additiveExpr.getOptionalMultiExpr()) {
    switch (multiExpr.first) {
    case Syntax::AdditiveExpr::Minus: {
      PrintContent("Minus");
      break;
    }
    case Syntax::AdditiveExpr::Plus: {
      PrintContent("Plus");
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
  PrintContent("MultiExpr");
  llvm::outs() << &multiExpr << "\n";
  IncAlign();
  visitor(multiExpr.getCastExpr());
  for (const auto &castExpr: multiExpr.getOptionalCastExpr()) {
    switch (castExpr.first) {
    case Syntax::MultiExpr::Multiply: {
      PrintContent("Multiply");
      break;
    }
    case Syntax::MultiExpr::Divide: {
      PrintContent("Divide");
      break;
    }
    case Syntax::MultiExpr::Modulo: {
      PrintContent("Modulo");
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
}
void visitor(const Syntax::UnaryExpr &unaryExpr){}
void visitor(const Syntax::TypeName &typeName){}
void visitor(const Syntax::PostFixExpr &postFixExpr){}
void visitor(const Syntax::PrimaryExpr &primaryExpr){}
}
