#ifndef LCC_EXPECTED_HPP
#define LCC_EXPECTED_HPP

#include <variant>

namespace lcc
{
    /**
     * Type which can either contain T or E. T is the expected and normal value type. If this does not contain T it
     * instead contains E which explains why T is not contained
     * @tparam T Value Type
     * @tparam E Error Type
     */
    template <class T, class E>
    class Expected final
    {
        std::variant<T, E> m_value;

    public:
        using ValueType = T;
        using ErrorType = E;

        static_assert(
            std::is_move_constructible_v<
                ValueType> && std::is_move_assignable_v<ValueType> && std::is_move_constructible_v<ErrorType> && std::is_move_assignable_v<ErrorType>);

        /**
         * Implicit move constructor with value
         * @param value Moveable value
         */
        constexpr Expected(ValueType&& value) noexcept(std::is_nothrow_move_constructible_v<ValueType>);

        /**
         * Copy constructs this from value
         * @tparam U ValueType by default
         * @param value copyable value
         */
        template <class U = ValueType>
        constexpr Expected(const U& value) noexcept(std::is_nothrow_copy_constructible_v<U>);

        /**
         * Implicit move constructs with error
         * @param error Error to construct with
         */
        constexpr Expected(ErrorType&& error) noexcept(std::is_nothrow_move_constructible_v<ErrorType>);

        /**
         * Constructs an expected with an error state from another expected which has the same error type
         * @throws std::bad_variant_access if supplied expected is not in an error state
         * @tparam U Value Type of the other expected
         * @param expected Other expected
         */
        template <class U>
        constexpr Expected(const Expected<U, E>& expected);

        /**
         * Move assigns Value to this
         * @param value Moveable value
         * @return Reference to this
         */
        constexpr Expected& operator=(ValueType&& value) noexcept(std::is_nothrow_move_assignable_v<ValueType>);

        /**
         * Copy assigns Expected from ValueType by default or ErrorType
         * @tparam U Type to construct with
         * @param value Copyable value
         * @return Reference to this
         */
        template <class U = ValueType>
        constexpr Expected& operator=(const U& value) noexcept(std::is_nothrow_copy_assignable_v<U>);

        /**
         *
         * @param error
         * @return
         */
        constexpr Expected& operator=(ErrorType&& error) noexcept(std::is_nothrow_move_assignable_v<ErrorType>);

        /**
         * @return Contained ValueType or nullptr if contained type is ErrorType
         */
        constexpr const ValueType* operator->() const noexcept;

        /**
         * @copydoc operator->() const
         */
        constexpr ValueType* operator->() noexcept;

        /**
         * @throws std::bad_variant_access if this does not contain ValueType
         * @return Reference to contained ValueType
         */
        constexpr const ValueType& operator*() const&;

        /**
         * @copydoc operator*() const &
         */
        constexpr ValueType& operator*() &;

        /**
         * @copydoc operator*() const &
         */
        constexpr const ValueType&& operator*() const&&;

        /**
         * @copydoc operator*() const &
         */
        constexpr ValueType&& operator*() &&;

        /**
         * @return True if this contains ValueType
         */
        constexpr explicit operator bool() const noexcept;

        /**
         * @copydoc operator bool()
         */
        constexpr bool hasValue() const noexcept;

        /**
         * @return True if this contains ErrorType
         */
        constexpr bool hasError() const noexcept;

        /**
         * @throws std::bad_variant_access if this does not contain ErrorType
         * @return Contained ErrorType
         */
        constexpr ErrorType& error() &;

        /**
         * @copydoc error() &
         */
        constexpr const ErrorType& error() const&;

        /**
         * @copydoc error() &
         */
        constexpr ErrorType&& error() &&;

        /**
         * @copydoc error() &
         */
        constexpr const ErrorType&& error() const&&;
    };

    template <class T, class E>
    constexpr Expected<T, E>::Expected(ValueType&& value) noexcept(std::is_nothrow_move_constructible_v<ValueType>)
        : m_value(std::move(value))
    {
    }

    template <class T, class E>
    template <class U>
    constexpr Expected<T, E>::Expected(const U& value) noexcept(std::is_nothrow_copy_constructible_v<U>)
        : m_value(value)
    {
    }

    template <class T, class E>
    constexpr Expected<T, E>::Expected(ErrorType&& error) noexcept(std::is_nothrow_move_constructible_v<ErrorType>)
        : m_value(std::move(error))
    {
    }

    template <class T, class E>
    constexpr Expected<T, E>& Expected<T, E>::
        operator=(ValueType&& value) noexcept(std::is_nothrow_move_assignable_v<ValueType>)
    {
        m_value = std::move(value);
        return *this;
    }

    template <class T, class E>
    template <class U>
    constexpr Expected<T, E>& Expected<T, E>::operator=(const U& value) noexcept(std::is_nothrow_copy_assignable_v<U>)
    {
        m_value = value;
        return *this;
    }

    template <class T, class E>
    constexpr Expected<T, E>& Expected<T, E>::
        operator=(ErrorType&& error) noexcept(std::is_nothrow_move_assignable_v<ErrorType>)
    {
        m_value = std::move(error);
        return *this;
    }

    template <class T, class E>
    constexpr const typename Expected<T, E>::ValueType* Expected<T, E>::operator->() const noexcept
    {
        return std::get_if<0>(&m_value);
    }

    template <class T, class E>
    constexpr typename Expected<T, E>::ValueType* Expected<T, E>::operator->() noexcept
    {
        return std::get_if<0>(&m_value);
    }

    template <class T, class E>
    constexpr const typename Expected<T, E>::ValueType& Expected<T, E>::operator*() const&
    {
        return std::get<0>(m_value);
    }

    template <class T, class E>
    constexpr typename Expected<T, E>::ValueType& Expected<T, E>::operator*() &
    {
        return std::get<0>(m_value);
    }

    template <class T, class E>
    constexpr const typename Expected<T, E>::ValueType&& Expected<T, E>::operator*() const&&
    {
        return std::move(std::get<0>(m_value));
    }

    template <class T, class E>
    constexpr typename Expected<T, E>::ValueType&& Expected<T, E>::operator*() &&
    {
        return std::move(std::get<0>(m_value));
    }

    template <class T, class E>
    constexpr Expected<T, E>::operator bool() const noexcept
    {
        return m_value.index() == 0;
    }

    template <class T, class E>
    constexpr bool Expected<T, E>::hasValue() const noexcept
    {
        return m_value.index() == 0;
    }

    template <class T, class E>
    constexpr bool Expected<T, E>::hasError() const noexcept
    {
        return m_value.index() == 1;
    }

    template <class T, class E>
    constexpr typename Expected<T, E>::ErrorType& Expected<T, E>::error() &
    {
        return std::get<1>(m_value);
    }

    template <class T, class E>
    constexpr const typename Expected<T, E>::ErrorType& Expected<T, E>::error() const&
    {
        return std::get<1>(m_value);
    }

    template <class T, class E>
    constexpr typename Expected<T, E>::ErrorType&& Expected<T, E>::error() &&
    {
        return std::move(std::get<1>(m_value));
    }

    template <class T, class E>
    constexpr const typename Expected<T, E>::ErrorType&& Expected<T, E>::error() const&&
    {
        return std::move(std::get<1>(m_value));
    }

    template <class T, class E>
    template <class U>
    constexpr Expected<T, E>::Expected(const Expected<U, E>& expected) : m_value(expected.error())
    {
    }
} // namespace OpenCL

#endif // LCC_EXPECTED_HPP
