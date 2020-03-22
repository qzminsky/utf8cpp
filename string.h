#pragma once

// Copyright © 2020 Alex Qzminsky.
// License: MIT. All rights reserved.

#ifndef UTF8CPP_H
#define UTF8CPP_H

static_assert(__cplusplus >= 201700L, "C++17 or higher is required");

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <initializer_list>
#include <iostream>
#include <iterator>
#include <limits>
#include <numeric>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <vector>

/**
 * \namespace utf
 * 
 * \brief utf8cpp library source
 * \author Qzminsky
 * 
 * \version 0.8.4
 * \date 2020/03/22
*/
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
        using size_type       = intmax_t;
        using difference_type = ptrdiff_t;
        using pointer         = uint8_t*;
        using char_type       = uint32_t;
        using value_type      = char_type;

        /// The special value. The exact meaning depends on context
        static constexpr auto npos = std::numeric_limits<size_type>::max();

    private:

        /*            size() == end_bytes() - bytes()
         *  ╭—————————˄——————————╮
         * [x xx x xx xxx x xxxx x].  -- data
         *  ↑                      ↑
         *  _repr == bytes()       _end == end_bytes()
        */

        pointer _repr = nullptr, _end = nullptr;

    public:

        // SECTION Inner types
        // ANCHOR Type: view
        /**
         * \class view
         * 
         * \brief An iterable, non-owning proxy type for string
         * 
         * \details Desribes an iterable range with two pointers and a direction flag.
         * A forward direction means an iteration to the higher addresses; backward -- to the lower addresses.
         * The view doesn't provides any mutators to the original string
        */
        class view
        {
        public:

            // ANCHOR Member types
            using size_type       = string::size_type;
            using difference_type = string::difference_type;
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
                auto free () -> iterator&
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
                auto is_bound() const -> bool
                {
                    return _parent;
                }

                /**
                 * \brief Predicate. Checks if the iterator is bound with specified view
                 * 
                 * \param to Parent view candidate
                */
                [[nodiscard]]
                auto is_bound(view& to) const -> bool
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
                    return _parent->is_forward() ? _forward_decrease() : _forward_increase();
                }

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 * 
                 * \return Previous iterator's state as copy of it
                 * 
                 * \throw bad_operation
                */
                [[nodiscard]]
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
                [[nodiscard]]
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
                 * \note It also works with backward-directed views' iterators
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
                 * \return Codepoint of the character `*this + index`
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
                 * \brief Transforms an iterator into character's index in the parent view
                 * 
                 * \return Count of hops from the beginning of the view
                 * 
                 * \throw bad_operation
                */
                [[nodiscard]]
                auto as_index () const -> difference_type
                {
                    return *this - _parent->forward().begin();
                }

                /**
                 * \brief Compares two iterators by equality
                 * 
                 * \param other Iterator to compare with (right)
                 * 
                 * \return `true` if both iterators are pointing to the same characters; `false` otherwise
                */
                [[nodiscard]]
                auto operator == (iterator const& other) const -> bool
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
                auto operator != (iterator const& other) const -> bool
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
                    _confirm_op("Unbound iterators are unordered");
                    other._confirm_op("Unbound iterators are unordered");

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
                    _confirm_op("Unbound iterator range-checking");

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
                    _confirm_op("Unbound iterator modifying");

                    // Reverse ending iterator stays at the same place...
                    if (_base() == _parent->bytes() || _base() == _parent->_forward_rend()._base())
                    {
                        _ptrbase = _parent->bytes() - 1;
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
                    _confirm_op("Unbound iterator modifying");

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
                auto _base () const -> pointer
                {
                    return _ptrbase;
                }

                /**
                 * \internal
                 * \brief Checks if an operation is allowed with iterator; throws otherwise
                 * 
                 * \throw bad_operation
                */
                auto _confirm_op (const char* exception_msg) const -> void
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
            auto reverse () -> view&
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
            auto is_forward () const -> bool
            {
                return _direction == direction::forward;
            }

            /**
             * \brief Predicate. Returns `true` if the view is backward-directed
            */
            [[nodiscard]]
            auto is_backward () const -> bool
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
            auto bytes () const -> pointer
            {
                return _forward_begin;
            }

            /**
             * \brief Returns the size of the subspan's memory buffer
             *
             * \warning This isn't equivalent to `length()`, which returns the number of _characters_.
             * Every UTF-8 character has a different size, from 1 to 4 bytes
            */
            [[nodiscard]]
            auto size () const -> size_type
            {
                return _forward_end - bytes();
            }

            /**
             * \brief Returns the pointer to the ending of the view's data
            */
            [[nodiscard]]
            auto bytes_end () const -> pointer
            {
                return bytes() + size();
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
                return end() - begin();
            }

            /**
             * \brief Predicate. Returns `true` if the view's range has zero-size
            */
            [[nodiscard]]
            auto is_empty () const -> bool
            {
                return size() == 0;
            }

            /**
             * \brief Predicate operator. Returns `true` if the view is empty
            */
            [[nodiscard]]
            auto operator ! () const -> bool
            {
                return is_empty();
            }

            /**
             * \brief Returns a vector of the iterators pointing to the every occurence of the given substrings
             * 
             * \param vi Substring to search
             * \param pack Other substrings to search
            */
            template <typename... View>
            [[nodiscard]]
            auto matches (view const& vi, View const&... pack) const -> std::vector<iterator>
            {
                std::vector<iterator> res;

                for (auto it = begin(); !! it; ++it)
                {
                    // Single view comparer
                    auto _equal = [this] (pointer pcmp, view const& vcmp)
                    {
                        return pcmp + vcmp.size() <= bytes_end() &&
                               std::equal(vcmp.bytes(), vcmp.bytes_end(), pcmp);
                    };

                    // Folding comparison with all of the views in the pack
                    if (
                        auto ptr = it._base();
                        _equal(ptr, vi) || (_equal(ptr, std::forward<view>(pack)) || ...)
                    )
                        res.push_back(it);
                }
                return res;
            }

            /**
             * \brief Returns a vector of the iterators pointing to the every occurence of all characters
             * satisfying specified criteria
             * 
             * \param pred Predicate to check
            */
            template <typename Functor>
            [[nodiscard]]
            auto matches_if (Functor&& pred) const -> std::vector<iterator>
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
             * \param ch Character to search
             * \param pack Other characters to search
             * 
             * \throw unicode_error
            */
            template <typename... Char>
            [[nodiscard]]
            auto matches (char_type ch, Char... pack) const -> std::vector<iterator>
            {
                std::vector<iterator> res;

                // Single character comparer
                auto _equal = [] (char_type a, char_type b)
                {
                    _validate_char(b, "Matching with an invalid Unicode character");
                    return a == b;
                };

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (_equal(*it, ch) || (_equal(*it, (char_type)pack) || ...)) res.push_back(it);
                }
                return res;
            }

            /**
             * \brief Search for the given substring inside the view
             * 
             * \param vi First substring to search
             * \param pack Other substrings to search
             * 
             * \return View of first occurrence of any substring or `[end(); end())` if it does not found
            */
            template <typename... View>
            [[nodiscard]]
            auto find (view const& vi, View const&... pack) const -> view
            {
                for (auto it = begin(); !! it; ++it)
                {
                    // Single view comparer
                    auto _equal = [this] (pointer pcmp, view const& vcmp)
                    {
                        return pcmp + vcmp.size() <= bytes_end() &&
                               std::equal(vcmp.bytes(), vcmp.bytes_end(), pcmp);
                    };

                    // Folding comparison with all of the views in the pack
                    if (
                        auto ptr = it._base();
                        _equal(ptr, vi) || (_equal(ptr, std::forward<view>(pack)) || ...)
                    )
                        return { ptr, ptr + vi.size() };
                }
                return { end(), end() };
            }

            /**
             * \brief Search for the first character in the view satisfying specified criteria
             * 
             * \param pred Predicate to check
             * 
             * \return Iterator to the first occurence of the character
            */
            template <typename Functor>
            [[nodiscard]]
            auto find_if (Functor&& pred) const -> iterator
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
             * \param ch First character to search
             * \param pack Other characters to count
             * 
             * \return Iterator to the first occurence of any character from the pack
             * 
             * \throw unicode_error
            */
            template <typename... Char>
            [[nodiscard]]
            auto find (char_type ch, Char... pack) const -> iterator
            {
                // Single character comparer
                auto _equal = [] (char_type a, char_type b)
                {
                    _validate_char(b, "Search for an invalid Unicode character");
                    return a == b;
                };

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (_equal(*it, ch) || (_equal(*it, (char_type)pack) || ...)) return it;
                }
                return end();
            }

            /**
             * \brief Predicate. Returns `true` if the view starts with another one
             * 
             * \param vi View to match
            */
            [[nodiscard]]
            auto starts_with (view const& vi) const -> bool
            {
                return (size() >= vi.size()) && std::equal(vi.bytes(), vi.bytes_end(), bytes());
            }

            /**
             * \brief Predicate. Returns `true` if the view ends with another one
             * 
             * \param vi View to match
            */
            [[nodiscard]]
            auto ends_with (view const& vi) const -> bool
            {
                return (size() >= vi.size()) && std::equal(vi.bytes(), vi.bytes_end(), bytes_end() - vi.size());
            }

            /**
             * \brief Getting the codepoint of the character by index in the view
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
             * \brief Getting the codepoint of the first character of the view
             * 
             * \throw out_of_range
            */
            [[nodiscard]]
            auto front () const -> char_type
            {
                return get(0);
            }

            /**
             * \brief Getting the codepoint of the last character of the view
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
             * \brief Predicate. Returns `trie` if the view contains at least single specified
             * substring from the presented list
             * 
             * \param value First substring to search
             * \param pack Other substrings to search
            */
            template <typename... View>
            [[nodiscard]]
            auto contains (view const& vi, View const&... pack) const -> bool
            {
                return !! find(vi, pack...);
            }

            /**
             * \brief Predicate. Returns `trie` if the view contains characters satisfying specified criteria
             * 
             * \param pred Predicate to check
            */
            template <typename Functor>
            [[nodiscard]]
            auto contains_if (Functor&& pred) const -> bool
            {
                return !! find_if(pred);
            }

            /**
             * \brief Predicate. Returns `trie` if the view contains at least single specified
             * character from the presented list
             * 
             * \param ch First character to search
             * \param pack Other characters to search
             * 
             * \throw unicode_error
            */
            template <typename... Char>
            [[nodiscard]]
            auto contains (char_type ch, Char... pack) const -> bool
            {
                return !! find(ch, pack...);
            }

            /**
             * \brief Counts the number of occurences of all substrings in the pack
             * 
             * \param vi First substring to count
             * \param pack Other substrings to count
             * 
             * \return Summary number of occurences
            */
            template <typename... View>
            [[nodiscard]]
            auto count (view const& vi, View const&... pack) const -> size_type
            {
                size_type cnt = 0;

                for (auto it = begin(); !! it; ++it)
                {
                    // Single view comparer
                    auto _equal = [&it, this] (view const& vcmp)
                    {
                        return it._base() + vcmp.size() <= bytes_end() &&
                               std::equal(vcmp.bytes(), vcmp.bytes_end(), it._base());
                    };

                    // Folding comparison with all of the views in the pack
                    cnt += _equal(vi) + (_equal(std::forward<view>(pack)) + ...);
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
            template <typename Functor>
            [[nodiscard]]
            auto count_if (Functor&& pred) const -> size_type
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
             * \param ch First character to count
             * \param pack Other characters to count
             * 
             * \return Summary number of occurences
             * 
             * \throw unicode_error
            */
            template <typename... Char>
            [[nodiscard]]
            auto count (char_type ch, Char... pack) const -> size_type
            {
                size_type cnt = 0;

                // Single character comparer
                auto _equal = [] (char_type a, char_type b)
                {
                    _validate_char(b, "Counting of an invalid Unicode character");
                    return a == b;
                };

                // Folding comparison with all of the characters in the pack
                for (auto it = begin(); !! it; ++it)
                {
                    if (_equal(*it, ch) || (_equal(*it, (char_type)pack) || ...)) ++cnt;
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
             * \brief Compares two views by equality
             * 
             * \param other View to compare with
             * 
             * \return `true` if the views' data is equivalent to each other; `false` otherwise
            */
            [[nodiscard]]
            auto operator == (view const& other) const -> bool
            {
                return (size() == other.size()) && std::equal(bytes(), bytes_end(), other.bytes());
            }

            /**
             * \brief Compares two views by non-equality
             * 
             * \param other View to compare with
             * 
             * \return `true` if the views' data differs from each other; `false` otherwise
            */
            [[nodiscard]]
            auto operator != (view const& other) const -> bool
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
            auto operator < (view const& vi) const -> bool
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
            auto operator > (view const& vi) const -> bool
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
            auto operator <= (view const& vi) const -> bool
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
            auto operator >= (view const& vi) const -> bool
            {
                return !(*this < vi);
            }

            /**
             * \brief Predicate. Checks if the view contains only valid UTF-8 characters
            */
            [[nodiscard]]
            auto is_valid () const -> bool
            {
                for (auto ch = bytes(); ch != bytes_end(); ++ch)
                {
                    if (is_ascii(*ch)) {
                        continue;
                    }
                    else if (auto sz = string::_charsize(ch); sz > 4) {
                        return false;
                    }
                    else while (--sz) {
                        if (++ch == bytes_end() || (*ch & 0xC0) != 0x80) return false;
                    }
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

        };  // end class view

        // !SECTION

        friend auto get (std::istream&) -> char_type;
        friend auto put (std::ostream&, char_type) -> void;
        friend auto operator >> (std::istream&, string&) -> std::istream&;
        friend auto isspace (string::char_type) -> bool;

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
            : _repr{ std::exchange(other._repr, nullptr) }
            , _end{ std::exchange(other._end, nullptr) }
        {}

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
            tmp._repr = new uint8_t[size];
            tmp._end = tmp.bytes() + size;

            // Encoding into UTF-8
            auto dit = tmp.bytes();
            
            for (auto ch : data)
            {
                _validate_char(ch, "Source array contains a character with invalid codepoint");
                dit = _encode(dit, ch);
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
        static auto from_bytes (std::vector<uint8_t> const& vec) -> string
        {
            string tmp;
            tmp._bufinit((void*)vec.data(), vec.size());

            if (!tmp.is_valid()) throw unicode_error{ "Source vector contains invalid UTF-8 data" };

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

        /**
         * \brief Converting constructor from a view
         * 
         * \param vi View providing the set of characters to copy
        */
        string (view const& vi) : _repr{ new uint8_t[vi.size()] }
        {
            _end = bytes() + vi.size(); auto ptr = bytes();

            for (auto ch : vi) {
                ptr = _encode(ptr, ch);
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
                _repr = std::exchange(other._repr, nullptr);
                _end = std::exchange(other._end, nullptr);
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
        ~string () { delete[] bytes(); }

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
        auto chars () const -> view
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
        auto bytes () const -> pointer
        {
            return _repr;
        }

        /**
         * \brief Returns the pointer to the ending of the string's data
        */
        [[nodiscard]]
        auto bytes_end () const -> pointer
        {
            return _end;
        }

        /**
         * \brief Creates and returns an `std::vector` object containing the buffer bytes data
        */
        [[nodiscard]]
        auto as_bytes () const -> std::vector<uint8_t>
        {
            return std::vector<uint8_t>(bytes(), bytes_end());
        }

        /**
         * \brief Creates and returns an `std::vector` object containing the characters' codepoints
        */
        [[nodiscard]]
        auto as_unicode () const -> std::vector<char_type>
        {
            std::vector<char_type> tmp;

            auto vi = chars(); for (auto ch : vi)
            {
                tmp.push_back(ch);
            }
            return tmp;
        }

        /**
         * \brief Predicate. Returns `true` if the string contains ASCII-only characters
         *
         * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
        */
        [[nodiscard]]
        auto is_ascii () const -> bool
        {
            for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
            {
                if (!is_ascii(*ptr)) return false;
            }
            return true;
        }

        /**
         * \brief Predicate. Returns `true` if the character is ASCII-valid
         * 
         * \param ch Character's codepoint
         *
         * \note ASCII-subset of Unicode is presented by codepoints 0-127 (`0x00`-`0x7F`)
        */
        [[nodiscard]]
        static auto is_ascii (char_type ch) -> bool
        {
            return ch < 0x80;
        }

        /**
         * \brief Predicate. Returns `true` if the character is UTF-8-valid
         * 
         * \param ch Character's codepoint
        */
        [[nodiscard]]
        static auto is_valid (char_type ch) -> bool
        {
            return ch <= 0x10FFFF;
        }

        /**
         * \brief Converts all ASCII characters in the string into lowercase
         * 
         * \return Reference to the modified string
        */
        auto to_lower_ascii () -> string&
        {
            for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
            {
                if (is_ascii(*ptr)) *ptr = uint8_t(std::tolower(*ptr));
            }
            return *this;
        }

        /**
         * \brief Converts all ASCII characters in the string into uppercase
         * 
         * \return Reference to the modified string
        */
        auto to_upper_ascii () -> string&
        {
            for (auto ptr = bytes(); ptr != bytes_end(); ++ptr)
            {
                if (is_ascii(*ptr)) *ptr = uint8_t(std::toupper(*ptr));
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
        auto operator == (view const& vi) const -> bool
        {
            return vi == *this;
        }

        /**
         * \brief Compares the string and the view by non-equality
         * 
         * \param vi View to compare with
         * 
         * \return `true` if `*this` differs from view's data; `false` otherwise
        */
        [[nodiscard]]
        auto operator != (view const& vi) const -> bool
        {
            return vi != *this;
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
        auto operator < (view const& vi) const -> bool
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
        auto operator > (view const& vi) const -> bool
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
        auto operator <= (view const& vi) const -> bool
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
        auto operator >= (view const& vi) const -> bool
        {
            return chars() >= vi;
        }

        /**
         * \brief Predicate. Returns `true` if all of characters in the string are valid UTF-8-encoded
        */
        [[nodiscard]]
        auto is_valid () const -> bool
        {
            return chars().is_valid();
        }

        /**
         * \brief Predicate. Returns `true` if string does not contains any characters
        */
        [[nodiscard]]
        auto is_empty () const -> bool
        {
            return size() == 0;
        }

        /**
         * \brief Predicate operator. Returns `true` if the string is empty
        */
        [[nodiscard]]
        auto operator ! () const -> bool
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
        auto length () const -> size_type
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
        auto size () const -> size_type
        {
            return bytes_end() - bytes();
        }

        /**
         * \brief Appends a given Unicode character to the end of the string
         *
         * \param ch Codepoint of appending character
         *
         * \return Reference to the modified string
         * 
         * \throw unicode_error
        */
        auto push (char_type ch) -> string&
        {
            _validate_char(ch, "Pushing an invalid Unicode character");
            _encode(_expanded_copy(size() + _codebytes(ch)), ch);

            return *this;
        }

        /**
         * \brief Appends a given view to the end of current string
         *
         * \param other Appending view
         *
         * \return Reference to the modified string
        */
        auto push (view const& vi) -> string&
        {
            std::copy_n(vi.bytes(), vi.size(), _expanded_copy(size() + vi.size()));

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
            _end = it._base();

            return *it;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param pos Inserting position
         * \param value Unicode character's codepoint
         *
         * \return Reference to the modified string
         * 
         * \throw invalid_argument
         * \throw unicode_error
        */
        auto insert (size_type pos, char_type value) -> string&
        {
            _validate_char(value, "Invalid Unicode character insertion");

            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            _encode(_spread((chars().begin() + pos)._base(), size() + _codebytes(value)), value);
            return *this;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param iter Inserting position (by iterator)
         * \param value Unicode character's codepoint
         *
         * \return Reference to the modified string
         * 
         * \throw out_of_range
         * \throw unicode_error
        */
        auto insert (view::iterator const& iter, char_type value) -> string&
        {
            _validate_char(value, "Invalid Unicode character insertion");

            if (auto ptr = iter._base(); _range_check(ptr))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                _encode(_spread(ptr, size() + _codebytes(value)), value);
            }
            return *this;
        }
        
        /**
         * \brief Inserts the view into current string
         *
         * \param pos Inserting position
         * \param vi View to insert
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
         * \brief Inserts the view into current string
         * 
         * \param iter Inserting position (by iterator)
         * \param vi View to insert
         * 
         * \return Reference to the modified string
         * 
         * \throw out_of_range
        */
        auto insert (view::iterator const& iter, view const& vi) -> string&
        {
            if (auto ptr = iter._base(); _range_check(ptr))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                std::copy_n(vi.bytes(), vi.size(), _spread(ptr, size() + vi.size()));
            }
            return *this;
        }

        /**
         * \brief Replaces all occurences of the given view (by its data) by another
         * 
         * \param vi View to replace
         * \param other Replacing data
         * 
         * \return Reference to the modified string
        */
        auto replace_all (view const& vi, view const& other) -> string&
        {
            for (;;)
            {
                if (auto range = chars().find(vi); !! range) replace(range, other);
                else
                    break;
            }
            return *this;
        }

        /**
         * \brief Replaces all characters satisfying specified criteria by another
         * 
         * \param pred Checking predicate
         * \param value Replacing character's codepoint
         * 
         * \return Reference to the modified string
         * 
         * \throw unicode_error
        */
        template <typename Functor>
        auto replace_all_if (Functor&& pred, char_type value) -> string&
        {
            _validate_char(value, "Replacing by an invalid Unicode character");

            auto matches = chars().matches_if(pred);
            for (auto& off : matches)
            {
                replace(off, string::from_unicode({ value }));
            }
            return *this;
        }

        /**
         * \brief Replaces all occurences of the given character by another
         * 
         * \param what Character to replace
         * \param value Replacing character's codepoint
         * 
         * \return Reference to the modified string
         * 
         * \throw unicode_error
        */
        auto replace_all (char_type what, char_type value) -> string&
        {
            _validate_char(what, "Replacing an invalid Unicode character");

            return replace_all_if(
                [&what] (char_type ch) { return ch == what; },
                value
            );
        }

        /**
         * \brief Completely clears the string by deallocating its owned memory
         * 
         * \return Reference to the modified string
        */
        auto clear () -> string&
        {
            delete[] bytes();
            _repr = _end = nullptr;

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
            if (auto ptr = iter._base(); _range_check(ptr) && ptr != bytes_end())
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                auto sz = _charsize(ptr);
                std::copy(ptr + sz, bytes_end(), ptr);

                _end -= sz;
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
            if (auto vi_be = vi.begin()._base(), vi_en = vi.end()._base();
                _range_check(vi_be) ||
                _range_check(vi_en)
            ) {
                throw out_of_range{ "Span error" };
            }
            else {
                std::copy(vi_en, bytes_end(), vi_be);
                _end -= vi.size();
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
             * [x xx x yy yyy y zzzz z].      -- old state
             *  ↓    ↓ ←————————┘                          〉same buffer
             * [x xx x zzzz z ........].      -- new state
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
        template <typename Functor>
        auto remove_if (Functor&& pred) -> string&
        {
            for (auto it = chars().begin(); it._base() != bytes_end();)
            {
                if (pred(*it)) erase(view{ it });
                else
                    ++it;
            }
            return *this;
        }

        /**
         * \brief Removes all occurrences of the character in the string
         * 
         * \param value Character to remove (by its codepoint)
         * 
         * \return Reference to the modified string
         * 
         * \throw unicode_error
        */
        auto remove (char_type value) -> string&
        {
            _validate_char(value, "Removing an invalid Unicode character");

            return remove_if(
                [&value] (char_type ch) { return ch == value; }
            );
        }

        /**
         * \brief Removes all occurences of the substring in the current string
         * 
         * \param vi Substring to remove
         * 
         * \return Reference to the modified string
        */
        auto remove (view const& vi) -> string&
        {
            auto len = vi.length();

            for (auto it = chars().begin(); it._base() != bytes_end();)
            {
                if (auto rvi = view{ it, it + len }; rvi == vi)
                {
                    erase(rvi);
                }
                else ++it;
            }
            return *this;
        }


        /**
         * \brief Replaces the characters in the given range by other string
         *
         * \param vi View providing the range
         * \param other New substring
         *
         * \return Reference to the modified string object
         * 
         * \throw out_of_range
        */
        auto replace (view const& vi, view const& other) -> string&
        {
            auto rsize = vi.size(), osize = other.size();
            auto ptrpos = vi.begin()._base(), tail = vi.end()._base();

            if (_range_check(ptrpos) || _range_check(tail))
            {
                throw out_of_range{ "Span error" };
            }

            /*     pos     N           bytes_end()
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].      -- old state
             *  ↓    ↓       ←——┘                          〉same buffer
             * [x xx x __ __ zzzz z ..].      -- new state
             *         ↑            ↑
             *         ptrpos       bytes_end()
            */
            if (rsize > osize)
            {
                std::copy(tail, bytes_end(), ptrpos + osize);
                _end -= rsize - osize;
            }
            /*     pos     N           bytes_end()
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].            -- old buffer
             *  ↓    ↓ └————→
             * [x xx x . ... __ ___ _ zzzz z].      -- new buffer
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
         * \return Reference to the modified string object
         * 
         * \throw invalid_argument
         * \throw length_error
        */
        auto replace (size_type pos, size_type N, view const& other) -> string&
        {
            return replace(chars(pos, N), other);
        }

        /**
         * \brief Replaces all characters starting from given index by other string
         *
         * \param pos Replacement position
         * \param other New substring
         *
         * \return Reference to the modified string object
         * 
         * \throw invalid_argument
        */
        auto replace (size_type pos, view const& other) -> string&
        {
            return replace(pos, npos, other);
        }

        /**
         * \brief Replaces a character (by its iterator) by a new substring
         *
         * \param iter Replacement position (by an iterator)
         * \param other New substring
         *
         * \return Reference to the modified string object
         * 
         * \throw out_of_range
        */
        auto replace (view::iterator const& iter, view const& other) -> string&
        {
            if (_range_check(iter._base()))
            {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            return replace(view{ iter }, other);
        }

        /**
         * \brief Reallocates the memory buffer used by a string
         *
         * \return Reference to the modified string
        */
        auto shrink_to_fit () -> string&
        {
            _expanded_copy(size());
            return *this;
        }

        /**
         * \brief Splits a string into 2 strings at specified position
         *
         * \param pos Splitting position
         *
         * \return Right side of original string, length (`length() - pos`).
         * If `pos >= length()`, returns an empty string
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
        template <typename Functor>
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
         * \brief Removes all characters satisfying specified criteria from both sides of the string
         * 
         * \param value Character to trim (by its codepoint)
         * 
         * \return Reference to the modified string
         * 
         * \throw unicode_error
        */
        auto trim (char_type value) -> string&
        {
            _validate_char(value, "Trimming an invalid Unicode character");

            return trim_if(
                [&value] (char_type ch) { return value == ch; }
            );
        }

        /**
         * \brief Removes all whitespace-like characters from both sides of the string
         * 
         * \return Reference to the modified string
        */
        auto trim () -> string&
        {
            return trim_if(_isspace);
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
            bool series = false;

            for (
                pointer current = bytes(), start;
                current != bytes_end();
                current += _charsize(current)
            ) {
                if (_isspace(_decode(current)))
                {
                    if (!series)
                    {
                        start = current;
                        series = true;
                    }
                }
                else if (series)
                {
                    replace({ start, current }, " ");

                    current = start;
                    series = false;
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
            std::swap(_repr, other._repr);
            std::swap(_end, other._end);
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
        auto _expanded_copy (size_type new_size) -> pointer
        {
            auto tmp = new uint8_t[new_size]; auto copy_bytes = size();

            std::copy_n(bytes(), copy_bytes, tmp);
            delete[] bytes();

            _repr = tmp; _end = bytes() + new_size;

            return bytes() + copy_bytes;

            /*   new_size       returning pointer
             *  ╭———˄—————————— ↓ —————╮
             * [x xx x xx xxx x ........]    -- new buffer
             *  ↑             ↑
             * [x xx x xx xxx x]             -- old buffer
            */
        }

        /**
         * \internal
         * \brief Reallocates the memory buffer
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
            auto tail = _expanded_copy(new_size);

            where = bytes() + shift;
            std::copy_backward(where, tail, bytes_end());

            return where;

            /*   new_size     returning pointer
             *  ╭———˄———————— ↓ ——————————————╮
             * [x xx x xx xxx ........ y yyyy y]    -- new buffer ←——————┐
             *  ↑           ↑ ┌———————→                              shifting
             * [x xx x xx xxx y yyyy y ........]                         |
             *  ↑           ↑                                        expanding
             * [x xx x xx xxx y yyyy y]             -- old buffer ———————┘
             *                ↑
             *                where (original)
            */
        }

        /**
         * \internal
         * \brief Reallocates the memory buffer and fills it with the given contents
         *
         * \param buf Pointer to the initialization buffer
         * \param bufsize New buffer size
         * 
         * \details In fact, reallocation occurs only if `bufsize` exceeds the actual buffer size
        */
        auto _bufinit (void* buf, size_type bufsize) -> void
        {
            // Reallocate only if there is not enough memory
            if (bufsize > size())
            {
                delete[] bytes();
                _repr = new uint8_t[bufsize];
            }
            
            _end = bytes() + bufsize;
            std::copy_n((char*)buf, bufsize, bytes());
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
        static auto _charsize (pointer where) -> size_type
        {
            if (!where) return 0;

            uint8_t tmp = *where; size_type res = 0;

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
        static auto _decode (pointer where) -> char_type
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
            };

            return result;
        }

        /**
         * \internal
         * \brief Counts the bytes to store the encoded UTF-8 character by its codepoint
         * 
         * \param value Character's codepoint
         * 
         * \return Number of bytes
        */
        [[nodiscard]]
        static auto _codebytes (char_type value) -> size_type
        {
            if (value < 0x80) return 1;
            else if (value < 0x800) return 2;
            else if (value < 0x10000) return 3;
            return 4;
        }

        /**
         * \internal
         * \brief Predicate. Checks if `ptr` doesn't point into the string's buffer
         * 
         * \param ptr Checking pointer
        */
        [[nodiscard]]
        auto _range_check (pointer ptr) -> bool
        {
            return ptr < bytes() || ptr > bytes_end();
        }

        /**
         * \internal
         * \brief Provokes a `unicode_error` exception throwing in case of invalid character's codepoint
         * 
         * \param value Character to check (by its codepoint)
         * \param exception_msg Message into exception object
         * 
         * \throw unicode_error
        */
        static auto _validate_char (char_type value, const char* exception_msg) -> void
        {
            if (!is_valid(value)) throw unicode_error{ exception_msg };
        }

        /**
         * \internal
         * \brief Encodes an UTF-8 character
         * 
         * \param dest Destination pointer to the UTF-8 location inside the buffer
         * \param value Character's codepoint
         * 
         * \return Pointer to the following byte
        */
        static auto _encode (pointer dest, char_type value) -> pointer
        {
            if (!dest) return nullptr;

            // One-byted subset [0; 127]
            if (value < 0x80)
            {
                *dest = value; return ++dest;
            }

            // Multibyte characters representation
            else {
                auto size = _codebytes(value), tmp = size;
                uint8_t mask = 0x80;

                while (--size)
                {
                    *(dest + size) = value & 0x3F | 0x80;
                    value >>= 6; mask >>= 1; mask |= 0x80;
                }

                *dest = value & 0x07 | mask;

                return dest + tmp;
            }

            /*                               dest  single  returning pointer
             * value ∈ [0; 0x7F]           ⇒     \╭———˄——╮ ↓
             *                               [... 0_______ ...]
             * 
             *                               dest       pair        returning pointer
             * value ∈ [0x80; 0x7FF]       ⇒     \╭———————˄———————╮ ↓
             *                               [... 110_____ 10______ ...]
             * 
             *                               dest     3-bytes encoding       returning pointer
             * value ∈ [0x800; 0xFFFF]     ⇒     \╭————————————˄———————————╮ ↓
             *                               [... 1110____ 10______ 10______ ...]
             * 
             *                               dest         4-bytes encoding            returning pointer
             * value ∈ [0x10000; 0x10FFFF] ⇒     \╭————————————————˄————————————————╮ ↓
             *                               [... 11110___ 10______ 10______ 10______ ...]
            */
        }

        /**
         * \internal
         * \brief Predicate. Returns `true` if a character is space-qualified
         * 
         * \param value Checking character's codepoint
         * 
         * \note Unlike `std::isspace`, this function also matches the Unicode spaces
        */
        [[nodiscard]]
        static auto _isspace (char_type value) -> bool
        {
            auto _is_any = [value] (auto... lst) -> bool
            {
                return ((value == lst) || ...);
            };

            return std::isspace(value) || _is_any
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
    constexpr auto BOM () -> string::char_type
    {
        return 0xFEFF;
    }

    /**
     * \brief Predicate. Returns `true` if a character is space-qualified
     * 
     * \param value Checking character's codepoint
     * 
     * \note Unlike `std::isspace`, this function also matches the Unicode spaces
    */
    [[nodiscard]]
    auto isspace (string::char_type value) -> bool
    {
        return string::_isspace(value);
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
        std::remove_pointer<string::pointer>::type code[4] {};

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
     * \param value Character's codepoint
    */
    auto put (std::ostream& out, string::char_type value) -> void
    {
        std::remove_pointer<string::pointer>::type code[5] {};
        string::_encode(code, value);

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
        auto tmp_size = to.size();
        to._end = to.bytes();

        for (auto ch = get(is); is && !isspace(ch); ch = get(is))
        {
            // Reuse available memory to avoid reallocation
            if (to.size() + string::_codebytes(ch) <= tmp_size)
            {
                to._end = string::_encode(to.bytes_end(), ch);
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
        for (auto ch : vi)
        {
            put(os, ch);
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
     * \param number Input signed number
     * \param base Conversion base
     * 
     * \return String equivalent of `number`
     * 
     * \note The value of `base` must be in range [2; 36]. Otherwise, an exception will be thrown
     * 
     * \throw invalid_argument
    */
    [[nodiscard]]
    auto to_string (uintmax_t number, int base = 10) -> string
    {
        if (base < 2 || base > 36) throw invalid_argument{ "The base have to be between 2 and 36" };

        if (!number) return "0";

        constexpr auto digits = std::numeric_limits<uintmax_t>::digits;
        char buff[digits + 1] {}; auto p = buff + digits;

        while (number != 0)
        {
            if (int c = number % base; c < 10)
                *(--p) = '0' + c;
            else
                *(--p) = c - 10 + 'a';

            number /= base;
        }

        return p;
    }

    /**
     * \brief Converts the given number into the string according to the specified base
     * 
     * \param number Input unsigned number
     * \param base Conversion base
     * 
     * \return String equivalent of `number`
     * 
     * \details The value of `base` must be in range [2; 36]. Otherwise, an exception will be thrown
     * 
     * \throw invalid_argument
    */
    [[nodiscard]]
    auto to_string (intmax_t number, int base = 10) -> string
    {
        auto mstr = to_string((uintmax_t)std::abs(number), base);

        if (number < 0) mstr.insert(0, "-");

        return mstr;
    }

}   // end namespace utf

namespace std
{
    /**
     * \struct iterator_traits
     * 
     * \brief Iterator traits instantiation for the `string_view::iterator` class
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
    auto swap(utf::string_view& v1, utf::string_view& v2) noexcept -> void
    {
        v1.swap(v2);
    }

}   // end namespace std

#endif
