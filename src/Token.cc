/***********************************
 * File:     CToken.cc
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2022/10/11
 ***********************************/

#include "Token.h"
#include "SourceObject.h"
#include <sstream>
#include <string_view>
namespace lcc {

std::string_view TokenBase::getRepresentation(const SourceInterface & sourceObject) const {
    auto sv = std::string_view(sourceObject.getFiles()[mFileId].source);
    return sv.substr(mOffset, mLength);
}

uint32_t TokenBase::getLine(const SourceInterface & sourceObject) const {
    return sourceObject.getLineNumber(mFileId, mOffset);
}

uint32_t TokenBase::getColumn(const SourceInterface & sourceObject) const {
  auto line = sourceObject.getLineNumber(mFileId, mOffset);
  return mOffset - sourceObject.getLineStartOffset(mFileId, line) + 1;
}
} // namespace lcc::lexer
