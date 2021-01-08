#pragma once

// Copyright © 2020-2021 Alex Qzminsky.
// License: MIT. All rights reserved.

/// \brief Dynamic, contiguous storage of UTF-8-encoded characters set
///
/// \author https://github.com/qzminsky
/// \version 3.0.0
/// \date 2021/01/06

#ifndef UTF8CPP_H
#define UTF8CPP_H

static_assert(__cplusplus >= 2017'00, "C++17 or higher is required");

#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <fstream>
#include <initializer_list>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <limits>
#include <numeric>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

#if __cplusplus >= 2020'00
#   include <compare>
#   include <concepts>
#endif

namespace utf
{
    // SECTION Exceptions classes

    /// Out-of-bounds iterators and invalid views
    using out_of_range     = std::out_of_range;

    /// Invalid numeric offsets
    using invalid_argument = std::invalid_argument;

    /// Pop on an empty string
    using underflow_error  = std::underflow_error;

    /// Invalid numeric lengths of views
    using length_error     = std::length_error;

    /// Unresolved modifying of an iterator
    struct bad_operation   : std::runtime_error
    {
        explicit bad_operation (const char* msg)
            : std::runtime_error{ msg }
        {}
    };

    /// Unrepresentable characters codes
    struct unicode_error   : std::range_error
    {
        explicit unicode_error (const char* msg)
            : std::range_error{ msg }
        {}
    };
    // !SECTION

    /**
     * \class string
     *
     * \brief An UTF-8-based string class
     *
     * \details Stores an Unicode string as a dynamically-allocated memory buffer
    */
    class string
    {
    public:

        // ANCHOR Member types
        using size_type       = std::ptrdiff_t;
        using difference_type = std::ptrdiff_t;
        using unit            = std::uint8_t;
        using pointer         = unit*;
        using char_type       = char32_t;
        using value_type      = char_type;

        /// The special value. The exact meaning depends on context
        static constexpr auto npos = std::numeric_limits<size_type>::max();

    private:

        /*         size() == bytes_end() - bytes()    capacity() == _myend - bytes()
         *  ╭—————————˄——————————╮                    /
         * [x xx x xx xxx x xxxx x]..................⇤ _myend      — data
         *  ↑                      ↑
         *  _myfirst == bytes()    _mylast == bytes_end()
        */

        pointer _myfirst = nullptr, _mylast = nullptr, _myend = nullptr;

    public:

        // SECTION Inner types
        // ANCHOR Type: view
        /**
         * \class view
         *
         * \brief Iterable span type for string
         *
         * \details Desribes an iterable range with two pointers and a direction flag.
         * A forward direction means an iteration to the higher addresses; backward — to the lower addresses.
         * The view doesn't provides any mutators to the original string
        */
        class view
        {
        public:

            // ANCHOR Member types
            using size_type       = string::size_type;
            using difference_type = string::difference_type;
            using unit            = string::unit;
            using pointer         = string::pointer;
            using char_type       = string::char_type;
            using value_type      = string::value_type;

            /**
             * \enum direction
             *
             * \brief Describes a view's direction
            */
            enum class direction : bool
            {
                forward = true,
                backward = false
            };

        private:

            friend class string;

            // Iterable range
            pointer _forward_begin = nullptr, _forward_end = nullptr;

            // Direcion flag
            direction _direction;

            /* end(⇐)           begin(⇐)
             * ↓                 ↓
             * .[x xx x xx xxx x xxxx].
             *   ↑                    ↑
             *   begin(⇒)            end(⇒)
             * _forward_begin      _forward_end
            */

        public:

            // ANCHOR Type: iterator
            /**
             * \class iterator
             *
             * \brief An iterator over the string's view
            */
            class iterator
            {
            public:

                // ANCHOR Member types
                using difference_type   = view::difference_type;
                using value_type        = view::value_type;
                using iterator_category = std::bidirectional_iterator_tag;

            private:

                friend class view;
                friend class string;

                // The pointing unit
                pointer _ptrbase = nullptr;

                // The view which created the iterator. Using for select direction and range control
                const view* _parent = nullptr;

            public:

                /// There is no default constructor for an iterator
                iterator () = delete;

                /**
                 * \brief Untying the iterator from its parent
                 *
                 * \return Reference to the modified iterator
                */
                auto free () noexcept -> iterator&
                {
                    _parent = nullptr;
                    return *this;
                }

                /**
                 * \brief Tying the iterator to the new parent view
                 *
                 * \param to New parent view
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw out_of_range
                */
                auto bind (view& to) -> iterator&
                {
                    if (to.bytes() - 1 > _base() ||
                        to.bytes_end() < _base()
                    ) {
                        throw out_of_range{ "Out of bounds iterator binding" };
                    }

                    _parent = &to;
                    return *this;
                }

                /**
                 * \brief Predicate. Checks if the iterator have the parent view
                */
                [[nodiscard]]
                auto is_bound () const noexcept -> bool
                {
                    return _parent;
                }

                /**
                 * \brief Predicate. Checks if the iterator is bound with specified view
                 *
                 * \param to Parent view candidate
                */
                [[nodiscard]]
                auto is_bound (view& to) const noexcept -> bool
                {
                    return _parent == &to;
                }

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto operator ++ () -> iterator&
                {
                    _assert_bound("Unable to increase unbound iterator");

                    return _parent->is_forward() ? _forward_increase() : _forward_decrease();
                }

                /**
                 * \brief Offsets the iterator to the previous character consider direction
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto operator -- () -> iterator&
                {
                    _assert_bound("Unable to decrease unbound iterator");

                    return _parent->is_forward() ? _forward_decrease() : _forward_increase();
                }

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 *
                 * \return Previous iterator's state as copy of it
                 *
                 * \throw bad_operation
                */
#if __cplusplus >= 2020'00
                [[nodiscard("Using of the pre-increment is more optimal")]]
#else
                [[nodiscard]]
#endif
                auto operator ++ (int) -> iterator
                {
                    auto tmp = *this; ++*this;
                    return tmp;
                }

                /**
                 * \brief Offsets the iterator to the previous character consider direction
                 *
                 * \return Previous iterator's state as copy of it
                 *
                 * \throw bad_operation
                */
#if __cplusplus >= 2020'00
                [[nodiscard("Using of the pre-decrement is more optimal")]]
#else
                [[nodiscard]]
#endif
                auto operator -- (int) -> iterator
                {
                    auto tmp = *this; --*this;
                    return tmp;
                }

                /**
                 * \brief Offsets the iterator to the `count` characters ahead consider direction
                 *
                 * \param count Number of hops
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto operator += (difference_type count) -> iterator&
                {
                    pointer prev = nullptr;

                    if (count > 0)
                        for (; count--; prev = _base())
                        {
                            ++(*this);
                            if (prev == _base()) break;
                        }
                    else
                        for (; count++; prev = _base())
                        {
                            --(*this);
                            if (prev == _base()) break;
                        }

                    return *this;
                }

                /**
                 * \brief Offsets the iterator to the `count` characters back consider direction
                 *
                 * \param count Number of hops
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto operator -= (difference_type count) -> iterator&
                {
                    return *this += -count;
                }

                /**
                 * \brief Offsets the copy of the iterator to the `count` characters ahead consider direction
                 *
                 * \param count Number of hops
                 *
                 * \return Iterator with corresponding changes
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator + (difference_type count) const -> iterator
                {
                    iterator tmp = *this;
                    return tmp += count;
                }

                /**
                 * \brief Offsets the copy of the iterator to the `count` characters back consider direction
                 *
                 * \param count Number of hops
                 *
                 * \return Iterator with corresponding changes
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator - (difference_type count) const -> iterator
                {
                    iterator tmp = *this;
                    return tmp += -count;
                }

                /**
                 * \brief Returns the number of hops to another iterator
                 *
                 * \param other Iterator pointing to begin of the range
                 *
                 * \return The number of increments needed to go from `other` to `*this` if less.
                 * Otherwise, returns the negative number of increments from `*this` to `other`
                 *
                 * \note It also works with backward-directed views' iterators, but doesn't with unbound ones
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator - (iterator other) const -> difference_type
                {
                    difference_type count = 0;

                    if (other <= *this)
                    {
                        for (; *this != other; ++other) ++count;
                    }
                    else
                    {
                        for (; *this != other; --other) --count;
                    }

                    return count;
                }

                /**
                 * \brief Iterator dereferencing
                 *
                 * \return Codepoint of the pointing character
                 *
                 * \note This operator returns a value, not a reference
                 *
                 * \throw out_of_range
                */
                [[nodiscard]]
                auto operator * () const -> value_type
                {
                    if (is_bound() && !*this)
                    {
                        throw out_of_range{ "Out of bounds iterator dereferencing" };
                    }
                    return string::_decode(_base());
                }

                /**
                 * \brief Iterator dereferencing with specified offset
                 *
                 * \param index Offset from current iterator
                 *
                 * \return Codepoint of the `index`-th character in the bound view
                 *
                 * \note This operator returns a value, not a reference
                 *
                 * \throw out_of_range
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator [] (difference_type index) const -> value_type
                {
                    return *(*this + index);
                }

                /**
                 * \brief Calculates the pointing character's index in the parent view
                 *
                 * \return Count of hops from the beginning of the view
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto as_index () const -> difference_type
                {
                    _assert_bound("Unable to calculate unbound iterator's index");

                    return *this - _parent->begin();
                }

                /**
                 * \brief Calculates the pointing character's index in the parent view
                 * as if its direction is forward
                 *
                 * \return Count of hops from the beginning of the view
                 *
                 * \note The result of `.backward().end().as_forward_index()` is -1
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto as_forward_index () const -> difference_type
                {
                    _assert_bound("Unable to calculate unbound iterator's index");

                    return *this - _parent->forward().begin();
                }

                /**
                 * \brief Calculates the pointing character's index in the parent view
                 * as if its direction is backward
                 *
                 * \return Count of hops from the beginning of the view
                 *
                 * \note The result of `.forward().end().as_backward_index()` is -1
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto as_backward_index () const -> difference_type
                {
                    _assert_bound("Unable to calculate unbound iterator's index");

                    return *this - _parent->backward().begin();
                }

                /**
                 * \brief Compares two iterators by equality
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if both iterators are pointing to the same character; `false` otherwise
                */
                [[nodiscard]]
                auto operator == (iterator const& other) const noexcept -> bool
                {
                    return _base() == other._base();
                }

                /**
                 * \brief Compares two iterators by non-equality
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if comparing iterators are pointing to the different characters; `false` otherwise
                */
                [[nodiscard]]
                auto operator != (iterator const& other) const noexcept -> bool
                {
                    return !(*this == other);
                }

                /**
                 * \brief Compares two iterators by less
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if `*this` is less than `other`; `false` otherwise
                 *
                 * \details The iterator `X` is less than `Y`, if it is possible to make them
                 * equal each other by increasing `X` sequentally (at least once)
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator < (iterator const& other) const -> bool
                {
                    auto err_msg = "Unbound iterators are unordered";

                    _assert_bound(err_msg);
                    other._assert_bound(err_msg);

                    return _parent->is_forward() ? (_base() < other._base()) : (_base() > other._base());
                }

                /**
                 * \brief Compares two iterators by less or equality
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if `*this` is less than (or equal to) `other`; `false` otherwise
                 *
                 * \details The iterator `X` is less than `Y`, if it is possible to make them
                 * equal each other by increasing `X` sequentally (or they are equal already)
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator <= (iterator const& other) const -> bool
                {
                    return *this < other || *this == other;
                }

                /**
                 * \brief Compares two iterators by great
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if `*this` is greater than `other`; `false` otherwise
                 *
                 * \details The iterator `X` is greater than `Y`, if it is possible to make them
                 * equal each other by increasing `Y` sequentally (at least once)
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator > (iterator const& other) const -> bool
                {
                    return other < *this;
                }

                /**
                 * \brief Compares two iterators by great or equality
                 *
                 * \param other Iterator to compare with (right)
                 *
                 * \return `true` if `*this` is greater than (or equal to) `other`; `false` otherwise
                 *
                 * \details The iterator `X` is greater than `Y`, if it is possible to make them
                 * equal each other by increasing `Y` sequentally (or they are equal already)
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator >= (iterator const& other) const -> bool
                {
                    return *this > other || *this == other;
                }

                /**
                 * \brief Predicate operator. Returns `true` if the iterator points to the end of parent view
                 *
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto operator ! () const -> bool
                {
                    _assert_bound("Unbound iterator range-checking");

                    return *this == _parent->end();
                }

            private:

                /**
                 * \internal
                 * \brief Constructs the iterator from address bounded to specified view
                 *
                 * \param dp Pointer to the character inside the view
                 * \param whos Parent view
                */
                iterator (pointer dp, const view* whos) : _ptrbase{ dp }, _parent{ whos } {};

                /**
                 * \internal
                 * \brief Offsets the iterator to the previous character in forward direction
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto _forward_decrease () -> iterator&
                {
                    _assert_bound("Unbound iterator modifying");

                    // Reverse ending iterator stays at the same place...
                    if (
                        auto rend_ptr = _parent->_forward_rend()._base();
                        _base() == _parent->bytes() || _base() == rend_ptr
                    ) {
                        _ptrbase = rend_ptr;
                    }

                    // ...otherwise, the base pointer have to be decreased
                    // while pointing to a `10xxxxxx`-like byte
                    else while (_ptrbase && (*(--_ptrbase) & 0xC0) == 0x80);

                    return *this;
                }

                /**
                 * \internal
                 * \brief Offsets the iterator to the next character in forward direction
                 *
                 * \return Reference to the modified iterator
                 *
                 * \throw bad_operation
                */
                auto _forward_increase () -> iterator&
                {
                    _assert_bound("Unbound iterator modifying");

                    // Shift the base pointer to the character's bytes count;
                    // forward ending iterator stays at the same place
                    if (_base() != _parent->bytes_end())
                    {
                        _ptrbase += string::_charsize(_base());
                    }
                    return *this;
                }

                /**
                 * \internal
                 * \brief Getting the base pointer of the iterator
                */
                auto _base () const noexcept -> pointer
                {
                    return _ptrbase;
                }

                /**
                 * \internal
                 * \brief Checks if an operation is allowed with iterator; throws otherwise
                 *
                 * \throw bad_operation
                */
                auto _assert_bound (const char* exception_msg) const -> void
                {
                    if (!is_bound()) throw bad_operation{ exception_msg };
                }

            };  // end class iterator

            /// There is no default constructor for a view
            view () = delete;

            /**
             * \brief Constructs the view over the string
             *
             * \param base String to view
            */
            view (string const& base)
                : _forward_begin{ base.bytes() }
                , _forward_end{ base.bytes_end() }
                , _direction{ direction::forward }
            {}

            /**
             * \brief Converting constructor from C-string
             *
             * \param cstr C-string to construct from
            */
            view (const char* cstr)
                : _forward_begin{ (pointer)(void*)cstr }
                , _forward_end{ (pointer)(void*)(cstr + std::strlen(cstr)) }
                , _direction{ direction::forward }
            {}

#if __cplusplus >= 2020'00
            /**
             * \brief Converting constructor from an UTF-8-encoded C-string
             *
             * \param c8str UTF-8-encoded C-string to construct from
            */
            view (const char8_t* c8str)
                : _forward_begin{ (pointer)(void*)c8str }
                , _forward_end{ (pointer)(void*)((char*)c8str + std::strlen((char*)c8str)) }
                , _direction{ direction::forward }
            {}
#endif

            /**
             * \brief Converting constructor from an iterator
             *
             * \param it Begin iterator
             *
             * \details Creates a view over single character by an iterator
            */
            view (iterator it)
                : _forward_begin{ it._base() }
                , _forward_end{ it._base() + _charsize(it._base()) }
                , _direction{ direction::forward }
            {}

            /**
             * \brief Constructs a view via pair of iterators
             *
             * \param be Begin iterator
             * \param en End iterator
             *
             * \details In actual, checks the order of given iterators and may swap them
             * to keep the valid range taking the forward direction into account
            */
            view (iterator const& be, iterator const& en, direction dir = direction::forward)
                : _forward_begin{ std::min(be._base(), en._base()) }
                , _forward_end{ std::max(be._base(), en._base()) }
                , _direction{ dir }
            {}

            /**
             * \brief Returns the copy of the original span
            */
            [[nodiscard]]
            auto clone () const -> view
            {
                return *this;
            }

            /**
             * \brief Converting C-string assignment
             *
             * \param cstr Source C-string (`const char*`) to assign
             *
             * \return Reference to the left operand
            */
            auto operator = (const char* cstr) -> view&
            {
                _forward_begin = (pointer)(void*)cstr;
                _forward_end = _forward_begin + std::strlen(cstr);
                _direction = direction::forward;

                return *this;
            }

            /**
             * \brief Creates a view-based string as the copy of original
             *
             * \return Copy of the original string in specified characters range
            */
            [[nodiscard]]
            auto to_string () const -> string
            {
                return *this;
            }

            /**
             * \brief Flips the iterating direction
             *
             * \return Reference to the modfied view
            */
            auto reverse () noexcept -> view&
            {
                _direction = is_forward() ? direction::backward : direction::forward;
                return *this;
            }

            /**
             * \brief Copies the view and sets the copy's iterating direction as forward
             *
             * \return Modified copy of the original view
            */
            auto forward () const -> view
            {
                auto tmp = *this;

                tmp._direction = direction::forward;
                return tmp;
            }

            /**
             * \brief Copies the view and sets the copy's iterating direction as backward
             *
             * \return Modified copy of the original view
            */
            auto backward () const -> view
            {
                auto tmp = *this;

                tmp._direction = direction::backward;
                return tmp;
            }

            /**
             * \brief Predicate. Returns `true` if the view is forward-directed
            */
            [[nodiscard]]
            auto is_forward () const noexcept -> bool
            {
                return _direction == direction::forward;
            }

            /**
             * \brief Predicate. Returns `true` if the view is backward-directed
            */
            [[nodiscard]]
            auto is_backward () const noexcept -> bool
            {
                return _direction == direction::backward;
            }

            /**
             * \brief Returns the beginning iterator consider direction
            */
            [[nodiscard]]
            auto begin () const -> iterator
            {
                return is_forward() ? iterator{ bytes(), this } : _forward_rbegin();
            }

            /**
             * \brief Returns the ending iterator consider direction
            */
            [[nodiscard]]
            auto end () const -> iterator
            {
                return is_forward() ? iterator{ bytes_end(), this } : _forward_rend();
            }

            /**
             * \brief Returns the pointer to the beginning of the view's data
            */
            [[nodiscard]]
            auto bytes () const noexcept -> pointer
            {
                return _forward_begin;
            }

            /**
             * \brief Returns the pointer to the ending of the view's data
            */
            [[nodiscard]]
            auto bytes_end () const noexcept -> pointer
            {
                return _forward_end;
            }

            /**
             * \brief Returns the size of the subspan's memory buffer
             *
             * \warning This isn't equivalent to `length()`, which returns the number of _characters_.
             * Every UTF-8 character has a different size, from 1 to 4 bytes
            */
            [[nodiscard]]
            auto size () const noexcept -> size_type
            {
                return bytes_end() - bytes();
            }

            /**
             * \brief Returns the number of Unicode characters in the view
             *
             * \warning This isn't equivalent to `size()`, which returns exactly the number of _bytes_.
             * Every UTF-8 character has a different size, from 1 to 4 bytes
             * \note This is an `O(n)` operation as it requires iteration over every UTF-8 character of the view
            */
            [[nodiscard]]
            auto length () const -> size_type
            {
                return forward().end().as_index();
            }

            /**
             * \brief Predicate. Returns `true` if the view's range has zero-size
            */
            [[nodiscard]]
            auto is_empty () const noexcept -> bool
            {
                return size() == 0;
            }

            /**
             * \brief Predicate operator. Returns `true` if the view is empty
            */
            [[nodiscard]]
            auto operator ! () const noexcept -> bool
            {
                return is_empty();
            }

            /**
             * \brief Returns a vector of the views pointing to the every occurence of the given substrings
             *
             * \param views Substrings to search
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<view>... View>
            requires
                     (sizeof...(View) > 0)
#else
            template <typename... View,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<View, view>...>>,
                      typename = std::enable_if_t<sizeof...(View)>
            >
#endif
            [[nodiscard]]
            auto matches (View const&... views) const noexcept -> std::vector<view>
            {
                std::vector<view> res;

                for (auto it = begin(); !! it; ++it)
                {
                    // Single view comparer
                    // (pushes a view into the vector if it matches the `vcmp` pattern)
                    auto _checked_push = [&res, &it, this] (view const& vcmp)
                    {
                        if (
                            auto last = _sub_equal(vcmp.begin(), vcmp.end(), it);
                            last.is_bound()
                        )
                            res.emplace_back(it - is_backward(), last, _direction);
                    };

                    // Folding filling with all of the views in the pack
                    (_checked_push(views), ...);
                }
                return res;
            }

            /**
             * \brief Returns a vector of the iterators pointing to the every occurence of all characters
             * satisfying specified criteria
             *
             * \param pred Predicate to check
            */
#if __cplusplus >= 2020'00
            template <std::predicate<char_type> Functor>
#else
            template <typename Functor,
                      typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
            >
#endif
            [[nodiscard]]
            auto matches_if (Functor&& pred) const noexcept -> std::vector<iterator>
            {
                std::vector<iterator> res;

                for (auto it = begin(); !! it; ++it)
                {
                    if (pred(*it)) res.push_back(it);
                }
                return res;
            }

            /**
             * \brief Returns a vector of the iterators pointing to the every occurence of the characters
             *
             * \param ucodes Characters to search
             *
             * \throw unicode_error
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<char_type>... Char>
            requires
                     (sizeof...(Char) > 0)
#else
            template <typename... Char,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                      typename = std::enable_if_t<sizeof...(Char)>
            >
#endif
            [[nodiscard]]
            auto matches (Char... ucodes) const -> std::vector<iterator>
            {
                std::vector<iterator> res;

                _validate_chars("Matching with an invalid Unicode character", ucodes...);

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (((*it == ucodes) || ...)) res.push_back(it);
                }
                return res;
            }

            /**
             * \brief Search for the given substring inside the view
             *
             * \param views Substrings to search
             *
             * \return View of first occurrence of any substring or `[end(); end())` if it was not found
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<view>... View>
            requires
                     (sizeof...(View) > 0)
#else
            template <typename... View,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<View, view>...>>,
                      typename = std::enable_if_t<sizeof...(View)>
            >
#endif
            [[nodiscard]]
            auto find (View const&... views) const noexcept -> view
            {
                view res = { end(), end() };

                for (auto it = begin(); !! it; ++it)
                {
                    // Single view comparer
                    // (assignes the longest occurence for intersecting patterns, e.g., "match" & "matches")
                    //                                                                             ^^^^^^^
                    auto _check = [&res, &it, this] (view const& vcmp)
                    {
                        if (
                            auto last = _sub_equal(vcmp.begin(), vcmp.end(), it);
                            vcmp.size() > res.size() && last.is_bound()
                        ) {
                            res = { it - is_backward(), last, _direction };

                            return true;
                        }
                        return false;
                    };

                    // Folding check all of the views in the pack
                    if ((_check(views) | ... | false)) break;
                }
                return res;
            }

            /**
             * \brief Search for the first character in the view satisfying specified criteria
             *
             * \param pred Predicate to check
             *
             * \return Iterator to the first occurence of the character
            */
#if __cplusplus >= 2020'00
            template <std::predicate<char_type> Functor>
#else
            template <typename Functor,
                      typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
            >
#endif
            [[nodiscard]]
            auto find_if (Functor&& pred) const noexcept -> iterator
            {
                for (auto it = begin(); !! it; ++it)
                {
                    if (pred(*it)) return it;
                }
                return end();
            }

            /**
             * \brief Search for the character in the view by its codepoint
             *
             * \param ucodes Characters to search
             *
             * \return Iterator to the first occurence of any character from the pack
             *
             * \throw unicode_error
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<char_type>... Char>
            requires
                     (sizeof...(Char) > 0)
#else
            template <typename... Char,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                      typename = std::enable_if_t<sizeof...(Char)>
            >
#endif
            [[nodiscard]]
            auto find (Char... ucodes) const -> iterator
            {
                _validate_chars("Search for an invalid Unicode character", ucodes...);

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (((*it == ucodes) || ...)) return it;
                }
                return end();
            }

            /**
             * \brief Predicate. Returns `true` if the view starts with another one
             *
             * \param vi View to match
            */
            [[nodiscard]]
            auto starts_with (view const& vi) const noexcept -> bool
            {
                return (size() >= vi.size()) && _sub_equal(vi.begin(), vi.end(), begin()).is_bound();
            }

            /**
             * \brief Predicate. Returns `true` if the view ends with another one
             *
             * \param vi View to match
            */
            [[nodiscard]]
            auto ends_with (view const& vi) const noexcept -> bool
            {
                return view{ *this }.reverse().starts_with(view{ vi }.reverse());
            }

            /**
             * \brief Returns the codepoint of a character by its index in the view
             *
             * \param index Index of the character in range `[0; length())`
             *
             * \note This is an `O(n)` operation as it requires iteration over every UTF-8 character from
             * the beginning of the view to the `index`-th position
             *
             * \throw invalid_argument
             * \throw out_of_range
            */
            [[nodiscard]]
            auto get (size_type index) const -> char_type
            {
                if (index < 0) throw invalid_argument{ "Negative character index" };

                return begin()[index];
            }

            /**
             * \brief Returns the codepoint of the first character of the view
             *
             * \throw out_of_range
            */
            [[nodiscard]]
            auto front () const -> char_type
            {
                return get(0);
            }

            /**
             * \brief Returns the codepoint of the last character of the view
             *
             * \note This is an `O(1)` operation
             *
             * \throw out_of_range
            */
            [[nodiscard]]
            auto back () const -> char_type
            {
                return *backward().begin();
            }

            /**
             * \brief Predicate. Returns `true` if the view contains at least single specified
             * substring from the presented list
             *
             * \param views Substrings to search
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<view>... View>
            requires
                     (sizeof...(View) > 0)
#else
            template <typename... View,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<View, view>...>>,
                      typename = std::enable_if_t<sizeof...(View)>
            >
#endif
            [[nodiscard]]
            auto contains (View const&... views) const noexcept -> bool
            {
                return !! find(views...);
            }

            /**
             * \brief Predicate. Returns `true` if the view contains characters satisfying specified criteria
             *
             * \param pred Predicate to check
            */
#if __cplusplus >= 2020'00
            template <std::predicate<char_type> Functor>
#else
            template <typename Functor,
                      typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
            >
#endif
            [[nodiscard]]
            auto contains_if (Functor&& pred) const noexcept -> bool
            {
                return !! find_if(pred);
            }

            /**
             * \brief Predicate. Returns `true` if the view contains at least single specified
             * character from the presented list
             *
             * \param uchars Characters to search
             *
             * \throw unicode_error
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<char_type>... Char>
            requires
                     (sizeof...(Char) > 0)
#else
            template <typename... Char,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                      typename = std::enable_if_t<sizeof...(Char)>
            >
#endif
            [[nodiscard]]
            auto contains (Char... uchars) const -> bool
            {
                return !! find(uchars...);
            }

            /**
             * \brief Counts the number of occurences of all substrings in the pack
             *
             * \param views Substrings to count
             *
             * \return Summary number of occurences
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<view>... View>
            requires
                     (sizeof...(View) > 0)
#else
            template <typename... View,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<View, view>...>>,
                      typename = std::enable_if_t<sizeof...(View)>
            >
#endif
            [[nodiscard]]
            auto count (View const&... views) const noexcept -> size_type
            {
                size_type cnt = 0;

                for (auto it = begin(); !! it; ++it)
                {
                    // Folding counting with all of the views in the pack
                    cnt += (_sub_equal(views.begin(), views.end(), it).is_bound() + ... + 0);
                }
                return cnt;
            }

            /**
             * \brief Counts the number of characters satisfying specified criteria
             *
             * \param pred Predicate to check
             *
             * \return Number of occurences
            */
#if __cplusplus >= 2020'00
            template <std::predicate<char_type> Functor>
#else
            template <typename Functor,
                      typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
            >
#endif
            [[nodiscard]]
            auto count_if (Functor&& pred) const noexcept -> size_type
            {
                size_type cnt = 0;

                for (auto it = begin(); !! it; ++it)
                {
                    if (pred(*it)) ++cnt;
                }
                return cnt;
            }

            /**
             * \brief Counts the number of occurences of all characters in the pack
             *
             * \param ucodes Characters to count
             *
             * \return Summary number of occurences
             *
             * \throw unicode_error
            */
#if __cplusplus >= 2020'00
            template <std::convertible_to<char_type>... Char>
            requires
                     (sizeof...(Char) > 0)
#else
            template <typename... Char,
                      typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                      typename = std::enable_if_t<sizeof...(Char)>
            >
#endif
            [[nodiscard]]
            auto count (Char... ucodes) const -> size_type
            {
                _validate_chars("Counting of an invalid Unicode character", ucodes...);

                size_type cnt = 0;

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (((*it == ucodes) || ...)) ++cnt;
                }
                return cnt;
            }

            /**
             * \brief Restricts the range of the view
             *
             * \param off Offset of the new starting character index from its old
             * \param N Number of characters in the new range
             *
             * \return Reference to the modified view
             *
             * \throw invalid_argument
             * \throw length_error
            */
            auto truncate (size_type off, size_type N) -> view&
            {
                if (off < 0) throw invalid_argument{ "Negative subspan offset" };
                if (N < 0) throw length_error{ "Negative subspan length" };

                if (is_forward())
                {
                    _forward_begin = (begin() + off)._base();
                    _forward_end = (begin() + N)._base();
                }
                else
                {
                    _forward_end = (_forward_rbegin() + (off - 1))._base();
                    _forward_begin = (_forward_rbegin() + (N - 1))._base();
                }

                return *this;
            }

            /**
             * \brief Returns a view with `N` first characters of the original span
             *
             * \param N Number of slicing characters
             *
             * \throw length_error
            */
            [[nodiscard]]
            auto first (size_type N) const -> view
            {
                return clone().truncate(0, N);
            }

            /**
             * \brief Returns a view with `N` last characters of the original span
             *
             * \param N Number of slicing characters
             *
             * \throw length_error
            */
            [[nodiscard]]
            auto last (size_type N) const -> view
            {
                return clone().reverse().truncate(0, N).reverse();
            }

            /**
             * \brief Compares two views by equality
             *
             * \param other View to compare with
             *
             * \return `true` if the views' data is equivalent to each other; `false` otherwise
            */
            [[nodiscard]]
            auto operator == (view const& other) const noexcept -> bool
            {
                return (size() == other.size()) && _sub_equal(begin(), end(), other.begin()).is_bound();
            }

#if __cplusplus >= 2020'00
            /**
             * \brief Three-way comparison between two views (non-including its equality check)
             *
             * \param other View to compare with
             *
             * \return Ordering-typed result
            */
            [[nodiscard]]
            auto operator <=> (view const& other) const noexcept -> std::strong_ordering
            {
                return std::lexicographical_compare_three_way(begin(), end(), other.begin(), other.end());
            }
#else
            /**
             * \brief Compares two views by non-equality
             *
             * \param other View to compare with
             *
             * \return `true` if the views' data differs from each other; `false` otherwise
            */
            [[nodiscard]]
            auto operator != (view const& other) const noexcept -> bool
            {
                return !(*this == other);
            }

            /**
             * \brief Checks if the view's data lexicographically less than the other's
             *
             * \param vi View to compare with
             *
             * \return `true` if the first view is lexicographically less than the second;
             * `false` otherwise
            */
            [[nodiscard]]
            auto operator < (view const& vi) const noexcept -> bool
            {
                return std::lexicographical_compare(begin(), end(), vi.begin(), vi.end());
            }

            /**
             * \brief Checks if the view's data lexicographically greater than the other's
             *
             * \param vi View to compare with
             *
             * \return `true` if the first view is lexicographically greater than the second;
             * `false` otherwise
            */
            [[nodiscard]]
            auto operator > (view const& vi) const noexcept -> bool
            {
                return vi < *this;
            }

            /**
             * \brief Checks if the view's data lexicographically less or equal to the other's
             *
             * \param vi View to compare with
             *
             * \return `true` if the first view is lexicographically less or equal to the second;
             * `false` otherwise
            */
            [[nodiscard]]
            auto operator <= (view const& vi) const noexcept -> bool
            {
                return !(*this > vi);
            }

            /**
             * \brief Checks if the view's data lexicographically greater or equal to the other's
             *
             * \param vi View to compare with
             *
             * \return `true` if the first view is lexicographically greater or equal to the second;
             * `false` otherwise
            */
            [[nodiscard]]
            auto operator >= (view const& vi) const noexcept -> bool
            {
                return !(*this < vi);
            }
#endif

            /**
             * \brief Predicate. Checks if the view contains only valid UTF-8 characters
            */
            [[nodiscard]]
            auto is_valid () const noexcept -> bool
            {
                for (auto ptr = bytes(); ptr != bytes_end(); ptr += string::_charsize(ptr))
                {
                    if (!_is_valid_utf(ptr)) return false;
                }
                return true;
            }

            /**
             * \brief Predicate. Returns `true` if the view contains ASCII-only characters
             *
             * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
            */
            [[nodiscard]]
            auto is_ascii () const noexcept -> bool
            {
                for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
                {
                    if (!string::_is_ascii(*ptr)) return false;
                }
                return true;
            }

            /**
             * \brief Swaps the spans of two views
             *
             * \param other View to swap
            */
            auto swap (view& other) noexcept -> void
            {
                std::swap(_forward_begin, other._forward_begin);
                std::swap(_forward_end, other._forward_end);
                std::swap(_direction, other._direction);
            }

        private:

            /**
             * \internal
             * \brief Constructs a view via pair of pointers
             *
             * \param start Beginning of the range
             * \param end Ending of the range
            */
            view (pointer start, pointer end)
                : _forward_begin{ start }
                , _forward_end{ end }
                , _direction{ direction::forward }
            {}

            /**
             * \internal
             * \brief Сompares two subviews by equality
             *
             * \param first1 Begin of the first range
             * \param last1 End of the first range
             * \param first2 Begin of the second range
             *
             * \return End of the second range (unbound if equality not satisfied)
            */
            static auto _sub_equal (iterator first1, iterator last1, iterator first2) noexcept -> iterator
            {
                for (; first1 != last1; ++first1, ++first2)
                {
                    if (!first2 || *first1 != *first2) return first2.free();
                }
                return first2;
            }

            /**
             * \internal
             * \brief Returns the beginning iterator on backward iterating direction
             *
             * \note Returning iterator points to the last character of the view
            */
            auto _forward_rbegin () const -> iterator
            {
                return iterator{ bytes_end(), this }._forward_decrease();
            }

            /**
             * \internal
             * \brief Returns the ending iterator on backward iterating direction
             *
             * \note Returning iterator points to the previous byte of the first character of the view
            */
            auto _forward_rend () const -> iterator
            {
                return { bytes() - 1, this };
            }

            /**
             * \internal
             * \brief Predicate. Checks if a pointing character is Unicode-valid
             *
             * \param ptr Pointer to the character
            */
            [[nodiscard]]
            auto _is_valid_utf (pointer ptr) const noexcept -> bool
            {
                if (string::_is_ascii(*ptr)) {
                    return true;
                }
                else if (auto sz = string::_charsize(ptr); sz > 4) {
                    return false;
                }
                else while (--sz) {
                    if (++ptr == bytes_end() || (*ptr & 0xC0) != 0x80) return false;
                }
                return true;
            }

        };  // end class view

        // !SECTION

        friend auto get (std::istream&) -> char_type;
        friend auto put (std::ostream&, char_type) -> void;
        friend auto operator >> (std::istream&, string&) -> std::istream&;
        friend auto is_space (char_type) noexcept -> bool;
        friend constexpr auto is_ascii (char_type) noexcept -> bool;
        friend constexpr auto is_valid (char_type) noexcept -> bool;

        /**
         * \brief Default constructor
        */
        string () = default;

        /**
         * \brief Copy constructor
         *
         * \param other String to copy
        */
        string (string const& other) { _bufinit((void*)other.bytes(), other.size()); }

        /**
         * \brief Move constructor
         *
         * \param other String to move from
        */
        string (string&& other) noexcept
            : _myfirst{ std::exchange(other._myfirst, nullptr) }
            , _mylast{ std::exchange(other._mylast, nullptr) }
            , _myend{ std::exchange(other._myend, nullptr) }
        {}

        /**
         * \brief Filling constructor
         *
         * \param ucode Duplicating character
         * \param count Number of character's copies
        */
        string (char_type ucode, size_type count)
        {
            auto bufsize = _codebytes(ucode) * count;

            _myfirst = new unit[bufsize];
            _mylast =
            _myend = bytes() + bufsize;

            for (pointer ptr = bytes(); count--;)
            {
                ptr = _encode(ptr, ucode);
            }
        }

        /**
         * \brief Constructs a string via given array of Unicode codepoints
         *
         * \param data Initializer list
         *
         * \throw unicode_error
        */
        [[nodiscard]]
        static auto from_unicode (std::initializer_list<char_type> data) -> string
        {
            string tmp;

            // Total size calculation
            auto size = std::accumulate
            (
                data.begin(), data.end(), (size_type)0,
                [] (auto c, auto v) { return c + _codebytes(v); }
            );

            // Span initialization
            tmp._myfirst = new unit[size];
            tmp._mylast =
            tmp._myend = tmp.bytes() + size;

            // Encoding into UTF-8
            auto dit = tmp.bytes();

            for (auto ucode : data)
            {
                _validate_chars("Source array contains a character with invalid codepoint", ucode);
                dit = _encode(dit, ucode);
            }

            return tmp;
        }

        /**
         * \brief Constrcuts a string via UTF-8 buffer stored in vector
         *
         * \param vec UTF-8-encoded characters' vector to construct from
         *
         * \note The reverse operation is `make_bytes()`
         *
         * \throw unicode_error
        */
        [[nodiscard]]
        static auto from_bytes (std::vector<unit> const& vec) -> string
        {
            string tmp;
            tmp._bufinit((void*)vec.data(), vec.size());

            if (!tmp.chars().is_valid()) throw unicode_error{ "Source vector contains invalid UTF-8 data" };

            return tmp;
        }

        /**
         * \brief Constructs a string containing file data
         *
         * \param path File system path
         *
         * \throw unicode_error
        */
        [[nodiscard]]
        static auto from_file (std::filesystem::path const& path) -> string
        {
            string tmp;
            std::ifstream ifs{ path, std::ios_base::binary | std::ios_base::ate };

            // Allocate memory once
            tmp.reserve(ifs.tellg());
            ifs.seekg(0, std::ios_base::beg);

            ifs.rdbuf()->sgetn((char*)tmp.bytes(), tmp.capacity());
            tmp._mylast = tmp._myend;

            if (!tmp.chars().is_valid()) throw unicode_error{ "Source file contains invalid UTF-8 data" };

            return tmp;
        }

        /**
         * \brief Converting constructor from `std::string`
         *
         * \param stds Source `std::string` to construct from
        */
        [[nodiscard]]
        static auto from_std_string (std::string const& stds) -> string
        {
            string tmp;
            tmp._bufinit((void*)stds.data(), stds.length());

            return tmp;
        }

        /**
         * \brief Converting constructor from a C-string
         *
         * \param cstr Source C-string (`const char*`) to construct from
        */
        string (const char* cstr) { _bufinit((void*)cstr, std::strlen(cstr)); }

#if __cplusplus >= 2020'00
        /**
         * \brief Converting constructor from an UTF-8-encoded C-string
         *
         * \param c8str Source UTF-8-encoded C-string (`const char8_t*`) to construct from
        */
        string (const char8_t* c8str) { _bufinit((void*)c8str, std::strlen((char*)c8str)); }
#endif

        /**
         * \brief Converting constructor from a view
         *
         * \param vi View providing the set of characters to copy
        */
        string (view const& vi) : _myfirst{ new unit[vi.size()] }
        {
            _mylast =
            _myend = bytes() + vi.size(); auto ptr = bytes();

            // Using for-cycle to construct from reversed views correctly
            for (auto ucode : vi) {
                ptr = _encode(ptr, ucode);
            }
        }

        /**
         * \brief Copy assignment
         *
         * \param other String to copy
         *
         * \return Reference to the left operand
        */
        auto operator = (string const& other) -> string&
        {
            if (this != &other)
            {
                _bufinit(other.bytes(), other.size());
            }
            return *this;
        }

        /**
         * \brief Move assignment
         *
         * \param other String to move from
         *
         * \return Reference to the left operand
        */
        auto operator = (string&& other) noexcept -> string&
        {
            if (this != &other)
            {
                _myfirst = std::exchange(other._myfirst, nullptr);
                _mylast = std::exchange(other._mylast, nullptr);
                _myend = std::exchange(other._myend, nullptr);
            }
            return *this;
        }

        /**
         * \brief Converting C-string assignment
         *
         * \param cstr Source C-string (`const char*`) to assign
         *
         * \return Reference to the left operand
        */
        auto operator = (const char* cstr) -> string&
        {
            _bufinit((void*)cstr, std::strlen(cstr));

            return *this;
        }

        /**
         * \brief Deallocation of the memory buffer
        */
        ~string () noexcept { delete[] bytes(); }

        /**
         * \brief Returns the copy of the original string
        */
        [[nodiscard]]
        auto clone () const -> string
        {
            return *this;
        }

        /**
         * \brief Creates an iterable span object
         *
         * \return `view`-wrapper over the current string
         *
         * \note This is an `O(1)` operation as it uses a pre-known range
        */
        [[nodiscard]]
        auto chars () const noexcept -> view
        {
            return *this;
        }

        /**
         * \brief Creates an iterable object with specified range
         *
         * \param shift Starting character position
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over the current string
         *
         * \note This is an `O(n)` operation
         *
         * \throw invalid_argument
         * \throw length_error
        */
        [[nodiscard]]
        auto chars (size_type shift, size_type N = npos) const -> view
        {
            return chars().truncate(shift, N);
        }

        /**
         * \brief Creates an iterable span object of `N` first characters of the string
         *
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over current string
         *
         * \note This is an `O(n)` operation
         *
         * \throw length_error
        */
        [[nodiscard]]
        auto first (size_type N) const -> view
        {
            return chars(0, N);
        }

        /**
         * \brief Creates an iterable span object of `N` last characters of the string
         *
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over the current string
         *
         * \note This is an `O(n)` operation
         *
         * \throw length_error
        */
        [[nodiscard]]
        auto last (size_type N) const -> view
        {
            return chars().reverse().truncate(0, N).reverse();
        }

        /**
         * \brief Returns the pointer to the beginning of the string's data
        */
        [[nodiscard]]
        auto bytes () const noexcept -> pointer
        {
            return _myfirst;
        }

        /**
         * \brief Returns the pointer to the ending of the string's data
        */
        [[nodiscard]]
        auto bytes_end () const noexcept -> pointer
        {
            return _mylast;
        }

        /**
         * \brief Creates and returns an `std::vector` object containing the buffer bytes data
        */
        [[nodiscard]]
        auto as_bytes () const -> std::vector<unit>
        {
            return std::vector<unit>(bytes(), bytes_end());
        }

        /**
         * \brief Creates and returns an `std::vector` object containing the characters' codepoints
        */
        [[nodiscard]]
        auto as_unicode () const -> std::vector<char_type>
        {
            std::vector<char_type> tmp;

            auto vi = chars(); for (auto ucode : vi)
            {
                tmp.push_back(ucode);
            }
            return tmp;
        }

        /**
         * \brief Converts all ASCII characters in the string into lowercase
         *
         * \return Reference to the modified string
        */
        auto to_ascii_lower () noexcept -> string&
        {
            for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
            {
                if (_is_ascii(*ptr)) *ptr = unit(std::tolower(*ptr));
            }
            return *this;
        }

        /**
         * \brief Converts all ASCII characters in the string into uppercase
         *
         * \return Reference to the modified string
        */
        auto to_ascii_upper () noexcept -> string&
        {
            for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
            {
                if (_is_ascii(*ptr)) *ptr = unit(std::toupper(*ptr));
            }
            return *this;
        }

        /**
         * \brief Compares the string and the view by its contents equality
         *
         * \param vi View to compare with
         *
         * \return `true` if `*this` is equivalent to view's data; `false` otherwise
        */
        [[nodiscard]]
        auto operator == (view const& vi) const noexcept -> bool
        {
            return vi == chars();
        }

#if __cplusplus >= 2020'00
        /**
         * \brief Three-way comparison between the string and the view (non-including its equality check)
         *
         * \param vi View to compare with
         *
         * \return Ordering-typed result
        */
        [[nodiscard]]
        auto operator <=> (view const& vi) const noexcept -> std::strong_ordering
        {
            return chars() <=> vi;
        }
#else
        /**
         * \brief Compares the string and the view by non-equality
         *
         * \param vi View to compare with
         *
         * \return `true` if `*this` differs from view's data; `false` otherwise
        */
        [[nodiscard]]
        auto operator != (view const& vi) const noexcept -> bool
        {
            return vi != chars();
        }

        /**
         * \brief Checks if the string's data lexicographically less than the view's
         *
         * \param vi View to compare with
         *
         * \return `true` if the string is lexicographically less than the view;
         * `false` otherwise
        */
        [[nodiscard]]
        auto operator < (view const& vi) const noexcept -> bool
        {
            return chars() < vi;
        }

        /**
         * \brief Checks if the string's data lexicographically greater than the view's
         *
         * \param vi View to compare with
         *
         * \return `true` if the string is lexicographically greater than the view;
         * `false` otherwise
        */
        [[nodiscard]]
        auto operator > (view const& vi) const noexcept -> bool
        {
            return chars() > vi;
        }

        /**
         * \brief Checks if the string's data lexicographically less or equal to the view's
         *
         * \param vi View to compare with
         *
         * \return `true` if the string is lexicographically less or equal to the view;
         * `false` otherwise
        */
        [[nodiscard]]
        auto operator <= (view const& vi) const noexcept -> bool
        {
            return chars() <= vi;
        }

        /**
         * \brief Checks if the string's data lexicographically greater or equal to the view's
         *
         * \param vi View to compare with
         *
         * \return `true` if the string is lexicographically greater or equal to the view;
         * `false` otherwise
        */
        [[nodiscard]]
        auto operator >= (view const& vi) const noexcept -> bool
        {
            return chars() >= vi;
        }
#endif

        /**
         * \brief Predicate. Returns `true` if string does not contains any characters
        */
        [[nodiscard]]
        auto is_empty () const noexcept -> bool
        {
            return size() == 0;
        }

        /**
         * \brief Predicate operator. Returns `true` if the string is empty
        */
        [[nodiscard]]
        auto operator ! () const noexcept -> bool
        {
            return is_empty();
        }

        /**
         * \brief Returns the number of Unicode characters in this string
         *
         * \warning This isn't equivalent to `size()`, which returns exactly the number of _bytes_.
         * Each UTF-8 character has a different size, from 1 to 4 bytes
         * \note This is an `O(n)` operation as it requires iteration over every UTF-8 character of the string
        */
        [[nodiscard]]
        auto length () const noexcept -> size_type
        {
            return chars().length();
        }

        /**
         * \brief Returns the size of the memory buffer used by the string
         *
         * \warning This isn't equivalent to `length()`, which returns the number of _characters_.
         * Each UTF-8 character has a different size, from 1 to 4 bytes
        */
        [[nodiscard]]
        auto size () const noexcept -> size_type
        {
            return bytes_end() - bytes();
        }

        /**
         * \brief Returns the full size of the allocated buffer's memory
        */
        [[nodiscard]]
        auto capacity () const noexcept -> size_type
        {
            return _myend - bytes();
        }

        /**
         * \brief Checks the current capacity of the string and reallocates if it's less than ordered
         *
         * \param new_cap New string's capacity (in bytes)
         *
         * \return Reference to the string
        */
        auto reserve (size_type new_cap) -> string&
        {
            if (new_cap > capacity())
            {
                auto tmp = bytes();

                _myfirst = new unit[new_cap];
                _myend = bytes() + new_cap;

                _bufinit((void*)tmp, _mylast - tmp);
                delete[] tmp;
            }
            return *this;
        }

        /**
         * \brief Appends a given Unicode character to the end of the string
         *
         * \param ucode Appending character
         *
         * \return Reference to the modified string
         *
         * \throw unicode_error
        */
        auto push (char_type ucode) -> string&
        {
            _validate_chars("Pushing an invalid Unicode character", ucode);
            _encode(_expand(size() + _codebytes(ucode)), ucode);

            return *this;
        }

        /**
         * \brief Appends a given substring to the end of current string
         *
         * \param vi Appending view
         *
         * \return Reference to the modified string
        */
        auto push (view const& vi) -> string&
        {
            std::copy_n(vi.bytes(), vi.size(), _expand(size() + vi.size()));

            return *this;
        }

        /**
         * \brief Removes the last character from the string and returns its codepoint
         *
         * \throw underflow_error
        */
        auto pop () -> char_type
        {
            if (is_empty()) throw underflow_error{ "Pop from empty string" };

            auto it = chars().reverse().begin();
            _mylast = it._base();

            return *it;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param pos Inserting position
         * \param ucode Unicode character to insert
         *
         * \return Reference to the modified string
         *
         * \throw invalid_argument
         * \throw unicode_error
        */
        auto insert (size_type pos, char_type ucode) -> string&
        {
            _validate_chars("Invalid Unicode character insertion", ucode);

            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            _encode(_spread((chars().begin() + pos)._base(), size() + _codebytes(ucode)), ucode);
            return *this;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param iter Inserting position (by iterator)
         * \param ucode Unicode character to insert
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
         * \throw unicode_error
        */
        auto insert (view::iterator const& iter, char_type ucode) -> string&
        {
            _validate_chars("Invalid Unicode character insertion", ucode);

            if (auto ptr = iter._base(); _bad_range_check(ptr))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                _encode(_spread(ptr, size() + _codebytes(ucode)), ucode);
            }
            return *this;
        }

        /**
         * \brief Inserts the substring into current string
         *
         * \param pos Inserting position
         * \param vi Substring to insert
         *
         * \return Reference to the modified string
         *
         * \throw invalid_argument
        */
        auto insert (size_type pos, view const& vi) -> string&
        {
            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            std::copy_n
            (
                vi.bytes(),
                vi.size(),
                _spread((chars().begin() + pos)._base(), size() + vi.size())
            );

            return *this;
        }

        /**
         * \brief Inserts the substring into current string
         *
         * \param iter Inserting position (by iterator)
         * \param vi Substring to insert
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
        */
        auto insert (view::iterator const& iter, view const& vi) -> string&
        {
            if (auto ptr = iter._base(); _bad_range_check(ptr))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                std::copy_n(vi.bytes(), vi.size(), _spread(ptr, size() + vi.size()));
            }
            return *this;
        }

        /**
         * \brief Applies given mutator to every character in the string
         *
         * \param mutator Character-transforming functor
         *
         * \return Reference to the modified string
        */
#if __cplusplus >= 2020'00
        template <std::invocable<char_type> Functor>
        requires
                 std::convertible_to<
                    std::invoke_result_t<Functor, char_type>,
                    char_type
                 >
#else
        template <typename Functor,
                  typename = std::enable_if_t<std::is_invocable_r_v<char_type, Functor, char_type>>
        >
#endif
        auto transform (Functor&& mutator) -> string&
        {
            string tmp; tmp.reserve(capacity());

            // Remake the string in a temporary location
            auto vi = chars(); for (auto ch : vi)
            {
                tmp.push(mutator(ch));
            }
            swap(tmp);

            return *this;
        }

        /**
         * \brief Replaces all occurences of the given substring by another
         *
         * \param vi Substring pattern to replace
         * \param other Replacing data
         *
         * \return Reference to the modified string
        */
        auto replace_all (view const& vi, view const& other) -> string&
        {
            string tmp; tmp.reserve(capacity());
            auto chs = chars();

            auto start = chs.begin();
            auto mlist = chs.matches(vi);

            /*             vi     (*)
             *         ╭———˄——╮ ╭——˄—╮
             * [x xx x yy yyy y zzzz z].       — old buffer
             *  ↓    ↓           ↓    ↓
             * [x xx x w wwww ww zzzz z].      — new buffer
             *         ↑       ↑
             *         └ other ┘
            */
            for (auto& match : mlist)
            {
                tmp.push({ start, match.begin() }).push(other);
                start = match.end();
            }
            tmp.push({ start, chs.end() });     // (*)

            swap(tmp);

            return *this;
        }

        /**
         * \brief Replaces all characters satisfying specified criteria by another
         *
         * \param pred Checking predicate
         * \param ucode Replacing character
         *
         * \return Reference to the modified string
         *
         * \throw unicode_error
        */
#if __cplusplus >= 2020'00
        template <std::predicate<char_type> Functor>
#else
        template <typename Functor,
                  typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
        >
#endif
        auto replace_all_if (Functor&& pred, char_type ucode) -> string&
        {
            _validate_chars("Replacing by an invalid Unicode character", ucode);

            return transform(
                [&pred, &ucode] (char_type ch) { return pred(ch) ? ucode : ch; }
            );
        }

        /**
         * \brief Replaces all occurences of the given character by another
         *
         * \param what Character to replace
         * \param ucode Replacing character
         *
         * \return Reference to the modified string
         *
         * \throw unicode_error
        */
        auto replace_all (char_type what, char_type ucode) -> string&
        {
            _validate_chars("Replacing an invalid Unicode character", what);

            return replace_all_if(
                [&what] (char_type ch) { return ch == what; },
                ucode
            );
        }

        /**
         * \brief Completely clears the string by deallocating its owned memory
         *
         * \return Reference to the modified string
        */
        auto clear () noexcept -> string&
        {
            delete[] bytes();
            _myfirst = _mylast = _myend = nullptr;

            return *this;
        }

        /**
         * \brief Removes the character by the iterator
         *
         * \param iter Iterator pointing to the character to remove
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
        */
        auto erase (view::iterator const& iter) -> string&
        {
            if (auto ptr = iter._base(); _bad_range_check(ptr) && ptr != bytes_end())
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                auto sz = _charsize(ptr);
                std::copy(ptr + sz, bytes_end(), ptr);

                _mylast -= sz;
            }
            return *this;
        }

        /**
         * \brief Removes characters in the given range
         *
         * \param vi View providing the range
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
        */
        auto erase (view const& vi) -> string&
        {
            if (
                auto vi_be = vi.begin()._base(), vi_en = vi.end()._base();
                _bad_range_check(vi_be) ||
                _bad_range_check(vi_en)
            ) {
                throw out_of_range{ "Span error" };
            }
            else {
                std::copy(vi_en, bytes_end(), vi_be);
                _mylast -= vi.size();
            }
            return *this;
        }

        /**
         * \brief Removes `N` characters starting from the given index
         *
         * \param pos Erasing start position
         * \param N Number of characters to remove
         *
         * \return Reference to the modified string
         *
         * \note In actual, just moves `[pos + N; length())` characters in memory buffer to `pos`,
         * i.e., it stays the same internal size. To free up unused memory, call `shrink_to_fit()` after
         *
         * \throw invalid_argument
         * \throw length_error
        */
        auto erase (size_type pos, size_type N = 1) -> string&
        {
            return erase(chars(pos, N));

            /*     pos     N           bytes_end()
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].      — old state
             *  ↓    ↓ ←————————┘                         〉same buffer
             * [x xx x zzzz z ........].      — new state
             *  ↑             ↑
             *  bytes()       bytes_end()
            */
        }

        /**
         * \brief Removes all characters satisfying specified criteria
         *
         * \param pred Checking predicate
         *
         * \return Reference to the modified string
        */
#if __cplusplus >= 2020'00
        template <std::predicate<char_type> Functor>
#else
        template <typename Functor,
                  typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
        >
#endif
        auto remove_if (Functor&& pred) noexcept -> string&
        {
            for (auto it = chars().begin(); it._base() != bytes_end();)
            {
                if (pred(*it)) erase(it);
                else
                    ++it;
            }
            return *this;
        }

        /**
         * \brief Removes all occurrences of the characters in the string
         *
         * \param ucodes Characters to remove
         *
         * \return Reference to the modified string
         *
         * \throw unicode_error
        */
#if __cplusplus >= 2020'00
        template <std::convertible_to<char_type>... Char>
        requires
                 (sizeof...(Char) > 0)
#else
        template <typename... Char,
                  typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                  typename = std::enable_if_t<sizeof...(Char)>
        >
#endif
        auto remove (Char... ucodes) -> string&
        {
            _validate_chars("Removing an invalid Unicode character", ucodes...);

            // Folding removing all of the characters in the pack
            return remove_if
            (
                [&] (char_type cmp)
                {
                    return ((cmp == ucodes) || ...);
                }
            );
        }

        /**
         * \brief Removes all occurences of the substrings in the current string
         *
         * \param views Substrings to remove
         *
         * \return Reference to the modified string
        */
#if __cplusplus >= 2020'00
        template <std::convertible_to<view>... View>
        requires
                 (sizeof...(View) > 0)
#else
        template <typename... View,
                  typename = std::enable_if_t<std::conjunction_v<std::is_convertible<View, view>...>>,
                  typename = std::enable_if_t<sizeof...(View)>
        >
#endif
        auto remove (View const&... views) noexcept -> string&
        {
            for (;;)
            {
                if (auto range = chars().find(views...); !! range) erase(range);
                else
                    break;
            }
            return *this;
        }


        /**
         * \brief Replaces the characters in the given range by other string
         *
         * \param vi View providing the range
         * \param other New substring
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
        */
        auto replace (view const& vi, view const& other) -> string&
        {
            auto rsize = vi.size(), osize = other.size();
            auto ptrpos = vi.begin()._base(), tail = vi.end()._base();

            if (_bad_range_check(ptrpos) || _bad_range_check(tail))
            {
                throw out_of_range{ "Span error" };
            }

            /*     pos     N           bytes_end()
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].      — old state
             *  ↓    ↓       ←——┘                         〉same buffer
             * [x xx x __ __ zzzz z ..].      — new state
             *         ↑            ↑
             *         ptrpos       bytes_end()
            */
            if (rsize > osize)
            {
                std::copy(tail, bytes_end(), ptrpos + osize);
                _mylast -= rsize - osize;
            }
            /*     pos     N           bytes_end()
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].            — old buffer
             *  ↓    ↓ └————→
             * [x xx x . ... __ ___ _ zzzz z].      — new buffer
             *         ↑                     ↑
             *         ptrpos                bytes_end()
            */
            else if (rsize < osize)
            {
                ptrpos = _spread(ptrpos, size() + osize - rsize);
            }

            std::copy_n(other.bytes(), osize, ptrpos);

            return *this;
        }


        /**
         * \brief Replaces some characters starting from given index by other string
         *
         * \param pos Replacement start position
         * \param N Number of characters to replace
         * \param other New substring
         *
         * \return Reference to the modified string
         *
         * \throw invalid_argument
         * \throw length_error
        */
        auto replace (size_type pos, size_type N, view const& other) -> string&
        {
            return replace(chars(pos, N), other);
        }

        /**
         * \brief Replaces a characters by another one
         *
         * \param pos Replacement position
         * \param ucode New character
         *
         * \return Reference to the modified string
         *
         * \throw invalid_argument
         * \throw unicode_error
        */
        auto replace (size_type pos, char_type ucode) -> string&
        {
            if (pos < 0)
            {
                throw invalid_argument{ "Negative character index" };
            }
            return replace(chars().begin() + pos, ucode);
        }

        /**
         * \brief Replaces a character (by its iterator) by another one
         *
         * \param iter Replacement position (by an iterator)
         * \param ucode New character
         *
         * \return Reference to the modified string
         *
         * \throw out_of_range
         * \throw unicode_error
        */
        auto replace (view::iterator const& iter, char_type ucode) -> string&
        {
            _validate_chars("Replacing by an invalid Unicode character", ucode);

            if (auto ptr = iter._base(); _bad_range_check(ptr))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else
            {
                auto rsize = _charsize(ptr), osize = _codebytes(ucode);

                if (rsize > osize)
                {
                    std::copy(ptr + rsize, bytes_end(), ptr + osize);
                    _mylast -= rsize - osize;
                }
                else if (rsize < osize)
                {
                    ptr = _spread(ptr, size() + osize - rsize);
                }

                _encode(ptr, ucode);
            }
            return *this;
        }

        /**
         * \brief Reallocates the memory buffer used by a string
         *
         * \return Reference to the modified string
        */
        auto shrink_to_fit () -> string&
        {
            if (auto copy_bytes = size(); copy_bytes < capacity())
            {
                auto tmp = new unit[copy_bytes];
                std::copy_n(bytes(), copy_bytes, tmp);

                delete[] bytes();

                _myfirst = tmp;
                _mylast =
                _myend = bytes() + copy_bytes;
            }
            return *this;
        }

        /**
         * \brief Splits a string into 2 strings at specified position
         *
         * \param pos Splitting position
         *
         * \return Right side of original string, length (`length() - pos`).
         * If `pos >= length()`, returns an empty string
         *
         * \throw invalid_argument
        */
        auto split_off (size_type pos) -> string
        {
            string tmp{ chars(pos) };
            erase(pos, npos);

            return tmp;
        }

        /**
         * \brief Removes all characters satisfying specified criteria from both sides of the string
         *
         * \param pred Checking predicate
         *
         * \return Reference to the modified string
        */
#if __cplusplus >= 2020'00
        template <std::predicate<char_type> Functor>
#else
        template <typename Functor,
                  typename = std::enable_if_t<std::is_invocable_r_v<bool, Functor, char_type>>
        >
#endif
        auto trim_if (Functor&& pred) -> string&
        {
            while (pred(chars().back())) pop();

            for (auto start = bytes(); start < bytes_end(); start += _charsize(start))
            {
                if (!pred(_decode(start)))
                {
                    erase({ bytes(), start });
                    break;
                }
            }
            return *this;
        }

        /**
         * \brief Removes all occurrences of the given character from both sides of the string
         *
         * \param ucode Unicode character to trim
         *
         * \return Reference to the modified string
         *
         * \throw unicode_error
        */
        auto trim (char_type ucode) -> string&
        {
            _validate_chars("Trimming an invalid Unicode character", ucode);

            return trim_if(
                [&ucode] (char_type ch) { return ch == ucode; }
            );
        }

        /**
         * \brief Removes all whitespace-like characters from both sides of the string
         *
         * \return Reference to the modified string
        */
        auto trim () -> string&
        {
            return trim_if(_is_space);
        }

        /**
         * \brief Removes the whitespaces from the start and the end and replaces all
         * sequences of internal whitespace with a single space
         *
         * \return Reference to the modified string
        */
        auto simplify () -> string&
        {
            // Trimming whitespaces
            trim();

            // Simplifying internal spaces
            bool spaces = false;

            for (
                pointer current = bytes(), start;
                current != bytes_end();
                current += _charsize(current)
            ) {
                if (_is_space(_decode(current)))
                {
                    if (!spaces)
                    {
                        start = current;
                        spaces = true;
                    }
                }
                else if (spaces)
                {
                    replace({ start, current }, " ");

                    current = start;
                    spaces = false;
                }
            }

            return *this;
        }

        /**
         * \brief Swaps the contents of two string
         *
         * \param other String to exchange the contents with
         *
         * \note In actual, it swaps the memory pointers only, without reallocations
        */
        auto swap (string& other) noexcept -> void
        {
            std::swap(_myfirst, other._myfirst);
            std::swap(_mylast, other._mylast);
            std::swap(_myend, other._myend);
        }

    private:

        /**
         * \internal
         * \brief Expanding the memory space used by the string
         *
         * \param new_size Expanded size of memory buffer
         *
         * \return Pointer to the end of the old buffer data in the new location
         *
         * \details The new buffer has an extra space after the original content
         * \warning New buffer size must be at least equal to old. Otherwise, it will cause the UB
        */
        auto _expand (size_type new_size) -> pointer
        {
            auto old_size = size();

            if (new_size > capacity())
            {
                reserve(std::max(capacity() * 3 / 2, new_size));
            }
            _mylast = bytes() + new_size;

            return bytes() + old_size;

            /*   new_size       returning pointer
             *  ╭———˄—————————— ↓ —————╮
             * [x xx x xx xxx x ........]    — new buffer
             *  ↑             ↑
             * [x xx x xx xxx x]             — old buffer
            */
        }

        /**
         * \internal
         * \brief Shifts the buffer's tail to the end of specified volume
         *
         * \param where Pointer to the splitting position
         * \param new_size Expanded size of memory buffer
         *
         * \return Pointer to the beginning of the empty area inside the new buffer
         *
         * \warning New buffer size must be at least equal to old. Otherwise, it will cause the UB
        */
        auto _spread (pointer where, size_type new_size) -> pointer
        {
            auto shift = where - bytes();
            auto tail = _expand(new_size);

            where = bytes() + shift;
            std::copy_backward(where, tail, bytes_end());

            return where;

            /*   new_size     returning pointer
             *  ╭———˄———————— ↓ ——————————————╮
             * [x xx x xx xxx ........ y yyyy y]    — new buffer ←——————┐
             *  ↑           ↑ ┌———————→                             shifting
             * [x xx x xx xxx y yyyy y ........]                        |
             *  ↑           ↑                                       expanding
             * [x xx x xx xxx y yyyy y]             — old buffer ———————┘
             *                ↑
             *                where (original)
            */
        }

        /**
         * \internal
         * \brief Reallocates the memory buffer and fills it with the given contents
         *
         * \param buf Pointer to the initialization buffer
         * \param count Number of bytes to copy from init-buffer
         *
         * \details In fact, reallocation occurs only if `bufsize` exceeds the actual buffer capacity
        */
        auto _bufinit (void* buf, size_type count) -> void
        {
            // Reallocate only if there is not enough memory
            if (count > capacity())
            {
                delete[] bytes();

                _myfirst = new unit[count];
                _myend = bytes() + count;
            }

            _mylast = bytes() + count;
            std::copy_n((char*)buf, count, bytes());
        }

        /**
         * \internal
         * \brief Calculates the size of the UTF-8-encoded character passed by the pointer
         *
         * \param where Pointer to the character representation inside the buffer
         *
         * \return Number of bytes for character storage
         *
         * \warning Method works correctly with valid UTF-8-encoded characters only
        */
        [[nodiscard]]
        static auto _charsize (pointer where) noexcept -> size_type
        {
            if (!where) return 0;

            unit tmp = *where; size_type res = 0;

            if (!(tmp & 0x80)) return 1;

            do {
                ++res; tmp <<= 1;
            }
            while (tmp & 0x80);

            return res;
        }

        /**
         * \internal
         * \brief Decodes an UTF-8 character
         *
         * \param where Pointer to the character representation inside the buffer
         *
         * \return Characher's codepoint
         *
         * \warning Method works correctly with valid UTF-8-encoded characters only
        */
        [[nodiscard]]
        static auto _decode (pointer where) noexcept -> char_type
        {
            if (!where) return 0;

            char_type result = 0;

            if (!(*where & 0x80)) return *where;
            else {
                result = *where & (0x7F >> _charsize(where));

                while ((*++where & 0xC0) == 0x80)
                {
                    result <<= 6;
                    result |= *where & 0x3F;
                }
            }

            return result;
        }

        /**
         * \internal
         * \brief Counts the bytes to store the encoded UTF-8 character by its codepoint
         *
         * \param ucode Unicode character
         *
         * \return Number of bytes
        */
        [[nodiscard]]
        static auto _codebytes (char_type ucode) noexcept -> size_type
        {
            if (ucode < 0x80) return 1;
            else if (ucode < 0x800) return 2;
            else if (ucode < 0x10000) return 3;
            return 4;
        }

        /**
         * \internal
         * \brief Predicate. Checks if `ptr` doesn't point into the string's buffer
         *
         * \param ptr Checking pointer
        */
        [[nodiscard]]
        auto _bad_range_check (pointer ptr) noexcept -> bool
        {
            return ptr < bytes() || ptr > bytes_end();
        }

        /**
         * \internal
         * \brief Provokes a `unicode_error` exception throwing in case of any character's invalid codepoint
         *
         * \param exception_msg Message into exception object
         * \param ucodes Unicode characters to check
         *
         * \throw unicode_error
        */
#if __cplusplus >= 2020'00
        template <std::convertible_to<char_type>... Char>
        requires
                 (sizeof...(Char) > 0)
#else
        template <typename... Char,
                  typename = std::enable_if_t<std::conjunction_v<std::is_convertible<Char, char_type>...>>,
                  typename = std::enable_if_t<sizeof...(Char)>
        >
#endif
        static auto _validate_chars (const char* exception_msg, Char... ucodes) -> void
        {
            if (((!_is_valid(ucodes)) || ...)) throw unicode_error{ exception_msg };
        }

        /**
         * \internal
         * \brief Predicate. Returns `true` if the character is ASCII-valid
         *
         * \param ucode Unicode character
         *
         * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
        */
        [[nodiscard]]
        static constexpr auto _is_ascii (char_type ucode) noexcept -> bool
        {
            return ucode < 0x80;
        }

        /**
         * \internal
         * \brief Predicate. Returns `true` if the character is Unicode-valid
         *
         * \param ucode Unicode character
        */
        [[nodiscard]]
        static constexpr auto _is_valid (char_type ucode) noexcept -> bool
        {
            return ucode <= 0x10FFFF;
        }

        /**
         * \internal
         * \brief Encodes an UTF-8 character
         *
         * \param dest Destination pointer to the UTF-8 location inside the buffer
         * \param ucode Unicode character to encode
         *
         * \return Pointer to the following byte
        */
        static auto _encode (pointer dest, char_type ucode) noexcept -> pointer
        {
            if (!dest) return nullptr;

            // One-byted subset [0; 127]
            if (ucode < 0x80)
            {
                *dest = ucode; return ++dest;
            }

            // Multibyte characters representation
            else {
                auto size = _codebytes(ucode), tmp = size;
                unit mask = 0x80;

                while (--size)
                {
                    *(dest + size) = (ucode & 0x3F) | 0x80;
                    ucode >>= 6; mask >>= 1; mask |= 0x80;
                }

                *dest = (ucode & 0x07) | mask;

                return dest + tmp;
            }

            /*                               dest  single  returning pointer
             * ucode ∈ [0; 0x7F]           ⇒     \╭———˄——╮ ↓
             *                               [... 0_______ ...]
             *
             *                               dest       pair        returning pointer
             * ucode ∈ [0x80; 0x7FF]       ⇒     \╭———————˄———————╮ ↓
             *                               [... 110_____ 10______ ...]
             *
             *                               dest     3-bytes encoding       returning pointer
             * ucode ∈ [0x800; 0xFFFF]     ⇒     \╭————————————˄———————————╮ ↓
             *                               [... 1110____ 10______ 10______ ...]
             *
             *                               dest         4-bytes encoding            returning pointer
             * ucode ∈ [0x10000; 0x10FFFF] ⇒     \╭————————————————˄————————————————╮ ↓
             *                               [... 11110___ 10______ 10______ 10______ ...]
            */
        }

        /**
         * \internal
         * \brief Predicate. Returns `true` if a character is space-qualified
         *
         * \param ucode Checking Unicode character
         *
         * \note Unlike `std::isspace`, this function also matches the Unicode spaces
        */
        [[nodiscard]]
        static auto _is_space (char_type ucode) noexcept -> bool
        {
            auto _is_any = [ucode] (auto... lst) -> bool
            {
                return ((ucode == (char_type)lst) || ...);
            };

            return std::isspace(ucode) || _is_any
            (
                0xA0, 0x1680, 0x180E,
                0x2000, 0x2001, 0x2002, 0x2003,
                0x2004, 0x2005, 0x2006, 0x2007,
                0x2008, 0x2009, 0x200A, 0x200B,
                0x202F, 0x205F, 0x3000, 0xFEFF
            );
        }

    };  // end class string

    // ANCHOR Typedef: string_view
    using string_view = string::view;

    /**
     * \brief UTF-8 `Byte Order Mark` character
    */
    [[nodiscard]]
    constexpr auto BOM () noexcept -> string::char_type
    {
        return 0xFEFF;
    }

    /**
     * \brief Predicate. Returns `true` if a character is space-qualified
     *
     * \param ucode Checking Unicode character
     *
     * \note Unlike `std::isspace`, this function also matches the Unicode spaces
    */
    [[nodiscard]]
    auto is_space (string::char_type ucode) noexcept -> bool
    {
        return string::_is_space(ucode);
    }

    /**
     * \brief Predicate. Returns `true` if the character is ASCII-valid
     *
     * \param ucode Checking Unicode character
     *
     * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
    */
    [[nodiscard]]
    constexpr auto is_ascii (string::char_type ucode) noexcept -> bool
    {
        return string::_is_ascii(ucode);
    }

    /**
     * \brief Predicate. Returns `true` if the ASCII-character is in uppercase
     *
     * \param ucode Checking Unicode character
     *
     * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
    */
    [[nodiscard]]
    auto is_ascii_upper (string::char_type ucode) noexcept -> bool
    {
        return ucode == std::toupper(ucode);
    }

    /**
     * \brief Predicate. Returns `true` if the ASCII-character is in lowercase
     *
     * \param ucode Checking Unicode character
     *
     * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
    */
    [[nodiscard]]
    auto is_ascii_lower (string::char_type ucode) noexcept -> bool
    {
        return ucode == std::tolower(ucode);
    }

    /**
     * \brief Predicate. Returns `true` if the character is Unicode-valid
     *
     * \param ucode Checking Unicode character
    */
    [[nodiscard]]
    constexpr auto is_valid (string::char_type ucode) noexcept -> bool
    {
        return string::_is_valid(ucode);
    }

    // ANCHOR Char-by-char i/o
    /**
     * \brief Reads an UTF-8 character from an input stream
     *
     * \param in Input stream to read from
     *
     * \return Character's codepoint
    */
    [[nodiscard]]
    auto get (std::istream& in) -> string::char_type
    {
        string::unit code[4] {};

        code[0] = in.get(); if (!in) return 0;

        for (string::size_type i = 1; i < string::_charsize(code);)
        {
            code[i++] = in.get();
        }
        return string::_decode(code);
    }

    /**
     * \brief Writes an UTF-8 character into an output stream
     *
     * \param out Output stream to write into
     * \param ucode Unicode character
    */
    auto put (std::ostream& out, string::char_type ucode) -> void
    {
        string::unit code[5] {};
        string::_encode(code, ucode);

        out << code;
    }

    /**
     * \brief Reads UTF-8 characters from input stream until the first space
     *
     * \param is Reference to the input stream
     * \param to String to store characters
     *
     * \return Reference to the input stream
    */
    auto operator >> (std::istream& is, string& to) -> std::istream&
    {
        to._mylast = to.bytes();

        for (auto ch = get(is); is && !is_space(ch); ch = get(is))
        {
            // Reuse available memory to avoid reallocation
            if (to.size() + string::_codebytes(ch) <= to.capacity())
            {
                to._mylast = string::_encode(to.bytes_end(), ch);
            }
            else to.push(ch);
        }
        return is;
    }

    /**
     * \brief Inserts characters from the view's span into the stream
     *
     * \param os Reference to the output stream
     * \param vi View to insert
     *
     * \return Reference to the output stream
    */
    auto operator << (std::ostream& os, string_view const& vi) -> std::ostream&
    {
        for (auto ucode : vi)
        {
            put(os, ucode);
        }
        return os;
    }

    /**
     * \brief Inserts characters from the string into the stream
     *
     * \param os Reference to the output stream
     * \param str String to insert
     *
     * \return Reference to the output stream
    */
    auto operator << (std::ostream& os, string const& str) -> std::ostream&
    {
        return os << str.chars();
    }

    /**
     * \brief Converts the given number into the string according to the specified base
     *
     * \param number Input integral number
     * \param base Conversion base
     *
     * \return String equivalent of `number`
     *
     * \note The value of `base` must be in range [2; 36]. Otherwise, an exception will be thrown
     *
     * \throw invalid_argument
    */
#if __cplusplus >= 2020'00
    template <std::integral Integer>
#else
    template <typename Integer,
              typename = std::enable_if_t<std::is_integral_v<Integer>>
    >
#endif
    [[nodiscard]]
    auto to_string (Integer number, int base = 10) -> string
    {
        if (base < 2 || base > 36) throw invalid_argument{ "The base have to be between 2 and 36" };

        if (!number) return "0";

        constexpr auto digits = std::numeric_limits<Integer>::digits;
        bool is_negative = number < 0;

        char buff[1 + digits + 1] {}; auto p = buff + digits + 1;
        //        ↑            ↑                    ⇒        ⇒
        //      sign     null-terminator      from base to the null-terminator

        // Significants cast
        while (number != 0)
        {
            if (int c = std::fabs(number % base); c < 10)
                *(--p) = '0' + c;
            else
                *(--p) = c - 10 + 'a';

            number /= base;
        }

        if (is_negative) *(--p) = '-';

        return p;
    }

    /**
     * \brief Converts the given number into the string according to the specified format and precision
     *
     * \param number Input floating-point number
     * \param format Representation format specifier (`e` — scientific or `f` — fixed)
     * \param prec Number's precision
     *
     * \return String equivalent of `number`
     *
     * \throw invalid_argument
    */
#if __cplusplus >= 2020'00
    template <std::floating_point Float>
#else
    template <typename Float,
              typename = std::enable_if_t<std::is_floating_point_v<Float>>
    >
#endif
    [[nodiscard]]
    auto to_string (Float number, char format = 'e', unsigned prec = 6) -> string
    {
        std::ostringstream conv;

        if (format == 'e') conv << std::scientific;
        else if (format == 'f') conv << std::fixed;
        else
        {
            throw invalid_argument{ "Unexpected format specifier" };
        }
        conv << std::setprecision(prec) << number;

        return string::from_std_string(conv.str());
    }

}   // end namespace utf

namespace std
{
    /**
     * \class iterator_traits <utf::string_view::iterator>
     *
     * \brief Iterator traits specialization for the `utf::string_view::iterator` class
    */
    template<>
    class iterator_traits <utf::string_view::iterator>
    {
        using Iter = utf::string_view::iterator;

    public:

        using difference_type   = Iter::difference_type;
        using value_type        = Iter::value_type;
        using iterator_category = Iter::iterator_category;
    };

    /**
     * \brief `std::swap` specialization for strings
     *
     * \param s1 First swappable string
     * \param s2 Second swappable string
    */
    auto swap (utf::string& s1, utf::string& s2) noexcept -> void
    {
        s1.swap(s2);
    }

    /**
     * \brief `std::swap` specialization for views
     *
     * \param v1 First swappable view
     * \param v2 Second swappable view
    */
    auto swap (utf::string_view& v1, utf::string_view& v2) noexcept -> void
    {
        v1.swap(v2);
    }

    /**
     * \struct hash <utf::string_view>
     *
     * \brief Hash function object specialization for the `utf::string_view` class
    */
    template<>
    struct hash <utf::string_view>
    {
        auto operator () (utf::string_view const& vi) const noexcept -> std::size_t
        {
            std::size_t h = 0; for (auto ch : vi)
            {
                h = 31 * h + ch;
            }
            return h;
        }
    };

    /**
     * \struct hash <utf::string>
     *
     * \brief Hash function object specialization for the `utf::string` class
    */
    template<>
    struct hash <utf::string>
    {
        auto operator () (utf::string const& str) const noexcept -> std::size_t
        {
            return std::hash<utf::string_view>{}(str.chars());
        }
    };

}   // end namespace std

#endif  // UTF8CPP_H

// MIT License
//
// Copyright (c) 2020-2021 Alex Qzminsky
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
