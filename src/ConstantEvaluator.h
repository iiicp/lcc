#ifndef LCC_CONSTANTEVALUATOR_HPP
#define LCC_CONSTANTEVALUATOR_HPP

#include "Expected.h"
#include "FailureReason.h"
#include "Semantics.h"
#include "Syntax.h"

namespace lcc::Semantics
{
    using ConstRetType =
        Expected<std::variant<std::int32_t, std::uint32_t, std::int64_t, std::uint64_t, float, double, void*>,
                         FailureReason>;

    class ConstantEvaluator final
    {
        std::map<std::string, Semantics::RecordType> m_structOrUnions;
        std::map<std::string, std::reference_wrapper<const Semantics::Type>> m_typedefs;

    public:
        explicit ConstantEvaluator(
            const std::map<std::string, Semantics::RecordType>& structOrUnions = {},
            const std::map<std::string, std::reference_wrapper<const Semantics::Type>>& typedefs = {});

        ConstRetType visit(const Syntax::Expr& node);

        ConstRetType visit(const Syntax::AssignExpr& node);

        ConstRetType visit(const Syntax::PrimaryExpr& node);

        ConstRetType visit(const Syntax::PrimaryExprConstant& node);

        ConstRetType visit(const Syntax::PrimaryExprParent& node);

        ConstRetType visit(const Syntax::PostFixExpr& node);

        ConstRetType visit(const Syntax::PostFixExprPrimary& node);

        ConstRetType visit(const Syntax::PostFixExprSubscript& node);

        ConstRetType visit(const Syntax::PostFixExprDot& node);

        ConstRetType visit(const Syntax::PostFixExprArrow& node);

        ConstRetType visit(const Syntax::UnaryExpr& node);

        ConstRetType visit(const Syntax::UnaryExprPostFixExpr& node);

        ConstRetType visit(const Syntax::UnaryExprUnaryOperator& node);

        ConstRetType visit(const Syntax::UnaryExprSizeOf& node);

        ConstRetType visit(const Syntax::CastExpr& node);

        ConstRetType visit(const Syntax::MultiExpr& node);

        ConstRetType visit(const Syntax::AdditiveExpr& node);

        ConstRetType visit(const Syntax::ShiftExpr& node);

        ConstRetType visit(const Syntax::RelationalExpr& node);

        ConstRetType visit(const Syntax::EqualExpr& node);

        ConstRetType visit(const Syntax::BitAndExpr& node);

        ConstRetType visit(const Syntax::BitXorExpr& node);

        ConstRetType visit(const Syntax::BitOrExpr& node);

        ConstRetType visit(const Syntax::LogAndExpr& node);

        ConstRetType visit(const Syntax::LogOrExpr& node);

        ConstRetType visit(const Syntax::ConditionalExpr& node);
    };
} // namespace OpenCL::Semantics

#endif // OPENCLPARSER_CONSTANTEVALUATOR_HPP
