/***********************************
 * File:     lexer.cpp
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/9/30
 *
 * Sign:     enjoy life
 ***********************************/

#include "Lexer.h"
#include "ChaLitTokGen.h"
#include "EofTokGen.h"
#include "IdeTokGen.h"
#include "NumTokGen.h"
#include "PunTokGen.h"
#include "StrLitTokGen.h"
#include "TokenGen.h"
#include <unordered_map>
#include <vector>

namespace lcc {
namespace lexer {
enum class State {
  NumericConstant,
  CharConstant,
  StringLiteral,
  Punctuator,
  Identifier,
  Eof
};

std::vector<Token> Tokenize(std::string &source) {
  TokenGen::InitTokenGen(reinterpret_cast<uint8_t *>(source.data()), source.size());

  std::unordered_map<State, std::unique_ptr<TokenGen>> TokGenMap;
  TokGenMap.insert({State::NumericConstant, std::make_unique<NumTokGen>()});
  TokGenMap.insert({State::CharConstant, std::make_unique<ChaLitTokGen>()});
  TokGenMap.insert({State::StringLiteral, std::make_unique<StrLitTokGen>()});
  TokGenMap.insert({State::Identifier, std::make_unique<IdeTokGen>()});
  TokGenMap.insert({State::Punctuator, std::make_unique<PunTokGen>()});
  TokGenMap.insert({State::Eof, std::make_unique<EofTokGen>()});

  std::vector<Token> tokArr;
  volatile bool isParsing = true;
  while (isParsing) {
    for (auto &v : TokGenMap) {
      v.second->SkipWhiteSpace();
      if (v.second->CanParseToken()) {
        Token t = v.second->ParseToken();
        if (t.GetTokenType() == TokenType::eof) {
          isParsing = false;
          break;
        }
        tokArr.push_back(t);
      }
    }
  }
  return tokArr;
}
}
}