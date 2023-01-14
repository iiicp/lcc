/***********************************
 * File:     SourceInterface.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/1/14
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SOURCEINTERFACE_H
#define LCC_SOURCEINTERFACE_H
#include <cstdint>
#include <span>
namespace lcc {
namespace Source {
struct File;
}
class SourceInterface {
public:
  virtual uint32_t getLineNumber(uint32_t fileID, uint32_t offset) const = 0;
  virtual uint32_t getLineStartOffset(uint32_t fileID, uint32_t lineNumber) const = 0;
  virtual uint32_t getLineEndOffset(uint32_t fileID, uint32_t lineNumber) const = 0;
  virtual std::span<const Source::File> getFiles() const = 0;
};
}

#endif // LCC_SOURCEINTERFACE_H
