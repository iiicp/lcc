/***********************************
 * File:     RecursiveVisitor.h
 *
 * Author:   蔡鹏
 *
 * Email:    iiicp@outlook.com
 *
 * Date:     2023/3/22
 ***********************************/

#ifndef LCC_RECURSIVEVISITOR_H
#define LCC_RECURSIVEVISITOR_H
#include "Match.h"
#include "Syntax.h"
#include "Type.h"
#include "Util.h"
#include <iterator>
#include <variant>
namespace lcc {
template <typename TopType, typename Callable>
class RecursiveVisitor {
  const TopType& m_start;
  Callable m_nextFunc;

  class Iterator {
    const TopType* m_curr;
    const Callable* m_nextFunc;
  public:
    using reference = const TopType&;
    using value_type = const TopType;
    using pointer = const TopType*;
    using iterator_category = std::forward_iterator_tag;
    using difference_type = void;

    Iterator() = default;

    Iterator(const TopType* curr, const Callable* nextFunc)
        : m_curr(curr), m_nextFunc(nextFunc){}

    bool operator==(const Iterator& rhs) const {
      return m_curr == rhs.m_curr;
    }

    bool operator!=(const Iterator& rhs) const {
      return !(*this == rhs);
    }

    const TopType& operator*() const {
      LCC_ASSERT(m_curr);
      return *m_curr;
    }

    const TopType* operator->() const {
      return m_curr;
    }

    Iterator& operator++(int) {
      LCC_ASSERT(m_curr && m_nextFunc);
      m_curr = (*m_nextFunc)(*m_curr);
      return *this;
    }

    Iterator operator++() {
      auto before = *this;
      LCC_ASSERT(m_curr && m_nextFunc);
      m_curr = (*m_nextFunc)(*m_curr);
      return before;
    }
  };

public:
  RecursiveVisitor(const TopType& start, Callable nextFunc)
  : m_start(start), m_nextFunc(nextFunc) {}

  using value_type = const TopType;
  using reference = const TopType&;
  using const_reference = const TopType&;
  using const_iterator = Iterator;
  using iterator = Iterator;

  const_iterator begin() const {
    return Iterator(&m_start, &m_nextFunc);
  }

  const_iterator cbegin() const {
    return begin();
  }

  const_iterator end() const {
    return Iterator(nullptr, &m_nextFunc);
  }

  const_iterator cend() const {
    return end();
  }
};

constexpr auto DIRECT_DECL_NEXT_FN = [](const Syntax::DirectDeclarator &value)
    -> const Syntax::DirectDeclarator * {
  return match(
      value,
      [](const box<Syntax::DirectDeclaratorParentheses> &parentheses)
          -> const Syntax::DirectDeclarator * {
        return &parentheses->getDeclarator().getDirectDeclarator();
      },
      [](const box<Syntax::DirectDeclaratorIdent> &)
          -> const Syntax::DirectDeclarator * { return nullptr; },
      [](const auto &value) -> const Syntax::DirectDeclarator * {
        return &value->getDirectDeclarator();
      });
};

constexpr auto ARRAY_TYPE_NEXT_FN =
    [](const Sema::Type &type) -> const Sema::Type * {
  return match(type.getVariant(), [](auto &&value) -> const Sema::Type * {
    using T = std::decay_t<decltype(value)>;
    if constexpr (std::is_same_v<Sema::ArrayType,T>
        || std::is_same_v<Sema::AbstractArrayType, T>
            || std::is_same_v<Sema::ValArrayType, T>)
    {
      if (value.getType().isArray())
      {
        return &value.getType();
      }
    }
    return nullptr;
  });
};

constexpr auto TYPE_NEXT_FN = [](const Sema::Type &type) -> const Sema::Type * {
  return match(
      type.getVariant(),
      [](const auto &value) -> const Sema::Type * {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<Sema::ArrayType, T>
            || std::is_same_v<Sema::AbstractArrayType, T>
            || std::is_same_v<Sema::ValArrayType, T>)
        {
          return &value.getType();
        }
        else
        {
          return nullptr;
        }
      },
      [](const Sema::PointerType &pointerType) -> const Sema::Type * {
        return &pointerType.getElementType();
      },
      [](const Sema::FunctionType &functionType) -> const Sema::Type * {
        return &functionType.getReturnType();
      });
};
}

#endif // LCC_RECURSIVEVISITOR_H
