#include "FailureReason.h"

namespace lcc {
FailureReason::FailureReason(std::string text)
    : m_text(std::move(text)) {}

const std::string &FailureReason::getText() const { return m_text; }
}
