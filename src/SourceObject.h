/***********************************
 * File:     SourceObject.h
 *
 * Author:   caipeng
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/1/14
 *
 * Sign:     enjoy life
 ***********************************/
#ifndef LCC_SOURCEOBJECT_H
#define LCC_SOURCEOBJECT_H
#include "SourceInterface.h"
#include "Utilities.h"
#include <string>
#include <vector>
namespace lcc {

namespace Source {
  struct File {
    std::string path;
    std::string source;
    std::vector<uint32_t> lineStartOffsets;
  };
}

template <typename T>
class SourceObject final : public SourceInterface {
  std::vector<T> mTokens;
  std::vector<Source::File> mFiles;
public:
  SourceObject() = default;
  SourceObject(std::vector<T> tokens, std::vector<Source::File> files)
      : mTokens(std::move(tokens)),
        mFiles(std::move(files))
  {}
  ~SourceObject() = default;
  SourceObject(const SourceObject&) = delete;
  SourceObject &operator=(const SourceObject&) = delete;
  SourceObject(SourceObject &&) = default;
  SourceObject& operator=(SourceObject &&) = default;

  uint32_t getLineNumber(uint32_t fileID, uint32_t offset) const override {
    LCC_ASSERT(fileID < mFiles.size());
    auto result = std::lower_bound(mFiles[fileID].lineStartOffsets.begin(), mFiles[fileID].lineStartOffsets.end(), offset);
    LCC_ASSERT(result != mFiles[fileID].lineStartOffsets.end());
    return std::distance(mFiles[fileID].lineStartOffsets.begin(), result) + (*result == offset ? 1 : 0);
  }

  uint32_t getLineStartOffset(uint32_t fileID, uint32_t lineNumber) const override {
    LCC_ASSERT(fileID < mFiles.size());
    LCC_ASSERT(lineNumber - 1 < mFiles[fileID].lineStartOffsets.size());
    return mFiles[fileID].lineStartOffsets[lineNumber - 1];
  }

  uint32_t getLineEndOffset(uint32_t fileID, uint32_t lineNumber) const override {
    LCC_ASSERT(fileID < mFiles.size());
    LCC_ASSERT(lineNumber < mFiles[fileID].lineStartOffsets.size());
    return mFiles[fileID].lineStartOffsets[lineNumber] - 1;
  }

  std::span<const Source::File> getFiles() const override {
    return mFiles;
  }

  const std::vector<T>& data() const noexcept
  {
    return mTokens;
  }

  std::vector<T>& data() noexcept
  {
    return mTokens;
  }
};
}
#endif // LCC_SOURCEOBJECT_H
