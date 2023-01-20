#ifndef LCC_FAILUREREASON_HPP
#define LCC_FAILUREREASON_HPP

#include <string>

namespace lcc
{
    class FailureReason
    {
        std::string m_text;

    public:
        explicit FailureReason(std::string text);

        const std::string& getText() const;
    };
} // namespace LCC

#endif // LCC_FAILUREREASON_HPP
