#pragma once

#ifndef UTF8CPP_H
#define UTF8CPP_H

static_assert(__cplusplus >= 201700L, "C++17 or higher is required");

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <initializer_list>
#include <iostream>
#include <iterator>
#include <limits>
#include <string>
#include <type_traits>
#include <vector>

namespace utf
{
    // ANCHOR Exceptions classes
    using out_of_range = std::out_of_range;
    using invalid_argument = std::invalid_argument;

    /**
     * \class string
     * 
     * \brief An UTF-8-based string class
     * \author Qzminsky
     *
     * \details Stores an Unicode string as a dynamically-allocated memory buffer
     * 
     * \version 0.5.0
     * \date 2020/03/05
    */
    class string
    {
    public:

        // ANCHOR Member types
        using size_type = intmax_t;
        using difference_type = ptrdiff_t;
        using pointer = uint8_t*;
        using char_type = uint32_t;

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
                 * \brief Offsets the iterator to the next character consider direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto operator ++ () -> iterator&
                {
                    return _parent->is_forward() ? _forward_increase() : _forward_decrease();
                }

                /**
                 * \brief Offsets the iterator to the previous character consider direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto operator -- () -> iterator&
                {
                    return _parent->is_forward() ? _forward_decrease() : _forward_increase();
                }

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 * 
                 * \return Previous iterator's state as copy of it
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
                 * \return Code point of the pointing character
                 * 
                 * \note This operator returns a value, not a reference
                 * 
                 * \throw out_of_range
                */
                [[nodiscard]]
                auto operator * () const -> char_type
                {
                    if (!*this)
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
                 * \return Code point of the character `*this + index`
                 * 
                 * \note This operator returns a value, not a reference
                 * 
                 * \throw out_of_range
                */
                [[nodiscard]]
                auto operator [] (difference_type index) const -> char_type
                {
                    return *(*this + index);
                }

                /**
                 * \brief Transforms an iterator into character's index in the parent view
                 * 
                 * \return Count of hops from the beginning of the view
                */
                [[nodiscard]]
                auto as_index () const -> size_type
                {
                    return *this - view{ *_parent }.forward().begin();
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
                 * \details The iterator `X` is less than another, if it is possible to make them
                 * equal each other by increasing `X` sequentally (at least once)
                */
                [[nodiscard]]
                auto operator < (iterator const& other) const -> bool
                {
                    return _parent->is_forward() ? (_base() < other._base()) : (_base() > other._base());
                }

                /**
                 * \brief Compares two iterators by less or equality
                 * 
                 * \param other Iterator to compare with (right)
                 * 
                 * \return `true` if `*this` is less than (or equal to) `other`; `false` otherwise
                 * 
                 * \details The iterator `X` is less than another, if it is possible to make them
                 * equal each other by increasing `X` sequentally (or they are equal already)
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
                 * \details The iterator `X` is greater than another, if it is not possible to
                 * make them equal each other by increasing `X` sequentally (at least once)
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
                 * \details The iterator `X` is greater than another, if it is not possible to
                 * make them equal each other by increasing `X` sequentally (or they are equal already)
                */
                [[nodiscard]]
                auto operator >= (iterator const& other) const -> bool
                {
                    return other < *this || *this == other;
                }

                /**
                 * \brief Predicate operator. Returns `true` if the iterator points to the end of parent view
                */
                [[nodiscard]]
                auto operator ! () const -> bool
                {
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
                */
                auto _forward_decrease () -> iterator&
                {
                    if (_base() == _parent->bytes() || _base() == _parent->_forward_rend()._base())
                    {
                        _ptrbase = _parent->bytes() - 1;
                    }
                    else while (_ptrbase && (*(--_ptrbase) & 0xC0) == 0x80);

                    return *this;
                }

                /**
                 * \internal
                 * \brief Offsets the iterator to the next character in forward direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto _forward_increase () -> iterator&
                {
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
            };

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
                return is_forward() ? backward() : forward();
            }

            /**
             * \brief Sets the iterating direction as forward
             * 
             * \return Reference to the modified view
            */
            auto forward () -> view&
            {
                _direction = direction::forward;
                return *this;
            }

            /**
             * \brief Sets the iterating direction as backward
             * 
             * \return Reference to the modified view
            */
            auto backward () -> view&
            {
                _direction = direction::backward;
                return *this;
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
             * \brief Returns the number of Unicode characters in the span
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
             * \brief Predicate operator. Returns `true` if view is not empty
            */
            [[nodiscard]]
            auto operator ! () const -> bool
            {
                return is_empty();
            }

            /**
             * \brief Search for the given substring inside the view
             * 
             * \param vi Substring to search (by its view)
             * 
             * \return Iterator to the first character of the substring or `end()` if it does not found
            */
            [[nodiscard]]
            auto find (view const& vi) const -> iterator
            {
                for (auto it = begin(); !! it; ++it)
                {
                    if (
                        it._base() + vi.size() <= bytes_end() &&
                        std::equal(vi.bytes(), vi.bytes_end(), it._base())
                    )
                        return it;
                }
                return end();
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
             * \brief Search for the first character in the view by its code point
             * 
             * \param value Character to search
             * 
             * \return Iterator to the first occurence of the given character
            */
            [[nodiscard]]
            auto find (char_type value) const -> iterator
            {
                return find_if(
                    [&value](char_type ch){ return value == ch; }
                );
            }

            /**
             * \brief Predicate. Returns `trie` if the view contains specified substring (by its view)
             * 
             * \param vi View to check the substring's containing
            */
            [[nodiscard]]
            auto contains (view const& vi) const -> bool
            {
                return !! find(vi);
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
             * \brief Predicate. Returns `trie` if the view contains at least single specified character
             * 
             * \param value Character to check its containing (given by its code point)
            */
            [[nodiscard]]
            auto contains (char_type value) const -> bool
            {
                return !! find(value);
            }

            /**
             * \brief Counts the number of substring occurences
             * 
             * \param vi Substring to count (by its view)
             * 
             * \return Number of occurences
            */
            [[nodiscard]]
            auto count (view const& vi) const -> size_type
            {
                size_type cnt = 0;

                for (auto it = begin(); !! it; ++it)
                {
                    if (
                        it._base() + vi.size() <= bytes_end() &&
                        std::equal(vi.bytes(), vi.bytes_end(), it._base())
                    )
                        ++cnt;
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
             * \brief Counts the number of occurences of the given characters
             * 
             * \param value Character to count
             * 
             * \return Number of occurences
            */
            [[nodiscard]]
            auto count (char_type value) const -> size_type
            {
                return count_if(
                    [&value](char_type ch){ return value == ch; }
                );
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
            */
            auto truncate (size_type off, size_type N) -> view&
            {
                if (off < 0) throw invalid_argument{ "Negative subspan offset" };
                if (N < 0) throw invalid_argument{ "Negative subspan length" };

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
             * \brief Compares the view and the string by its contents equality
             * 
             * \param str String to compare with
             * 
             * \return `true` if the view's data is equivalent to the string's; `false` otherwise
            */
            [[nodiscard]]
            auto operator == (string const& str) const -> bool
            {
                return (size() == str.size()) && std::equal(bytes(), bytes_end(), str.bytes());
            }

            /**
             * \brief Compares the view and the string by its contents non-equality
             * 
             * \param str String to compare with
             * 
             * \return `true` if the view's data is differs from the string's; `false` otherwise
            */
            [[nodiscard]]
            auto operator != (string const& str) const -> bool
            {
                return !(*this == str);
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
             * \brief Checks if the view's data lexicographically less than the string's
             * 
             * \param str String to compare with
             * 
             * \return `true` if the view is lexicographically less than the string;
             * `false` otherwise
            */
            [[nodiscard]]
            auto operator < (string const& str) const -> bool
            {
                return *this < str.chars();
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
                    else if (auto sz = _charsize(ch); sz > 4) {
                        return false;
                    }
                    else while (--sz) {
                        if (++ch == bytes_end() || (*ch & 0xC0) != 0x80) return false;
                    }
                }
                return true;
            }

        private:

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
        };
        // !SECTION

        friend auto read (std::istream&) -> char_type;
        friend auto write (std::ostream&, char_type) -> void;
        friend auto operator >> (std::istream&, string&) -> std::istream&;

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
        string (string&& other) noexcept : _repr{ std::exchange(other._repr, nullptr) }, _end{ std::exchange(other._end, nullptr) } {}

        /**
         * \brief Constructs a string via given array of Unicode code points
         * 
         * \param data Initializer list
        */
        static auto from_unicode (std::initializer_list<char_type> data) -> string
        {
            string tmp;

            // Total size calculation
            size_type size = 0; for (auto ch : data) {
                size += _codebytes(ch);
            }

            // Span initialization
            tmp._repr = new uint8_t[size];
            tmp._end = tmp.bytes() + size;

            // Encoding into UTF-8
            auto dit = tmp.bytes(); for (auto ch : data) dit = _encode(dit, ch);

            return tmp;
        }

        /**
         * \brief Constrcuts a string via UTF-8 buffer stored in vector
         * 
         * \param vec UTF-8-encoded characters' vector to construct from
         * 
         * \note The reverse operation is `make_bytes()`
        */
        static auto from_bytes (std::vector<uint8_t> const& vec) -> string
        {
            string tmp;
            tmp._bufinit((void*)vec.data(), vec.size());

            return tmp;
        }

        /**
         * \brief Converting constructor from `std::string`
         * 
         * \param stds Source `std::string` to construct from
        */
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
         * \throw invalid_argument
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
         * \throw invalid_argument
        */
        [[nodiscard]]
        auto last (size_type N) const -> view
        {
            return chars().backward().truncate(0, N).forward();
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
         * \brief Creates and returns an `std::vector` object containing buffer bytes data
        */
        [[nodiscard]]
        auto make_bytes () const -> std::vector<uint8_t>
        {
            return std::vector<uint8_t>(bytes(), bytes_end());
        }

        /**
         * \brief Getting the code point of the character by index in the string
         *
         * \param index Index of the character in range `[0; length())`
         *
         * \note This is an `O(n)` operation as it requires iteration over every UTF-8 character from
         * the beginning of the string to `index`-th position
         * 
         * \throw invalid_argument
         * \throw out_of_range
        */
        [[nodiscard]]
        auto get (size_type index) const -> char_type
        {
            if (index < 0) throw invalid_argument{ "Negative character index" };

            return chars().begin()[index];
        }

        /**
         * \brief Getting the code point of the first character of the string
         * 
         * \throw out_of_range
        */
        [[nodiscard]]
        auto front () const -> char_type
        {
            return get(0);
        }

        /**
         * \brief Getting the code point of the last character of the string
         * 
         * \note This is an `O(1)` operation
         * 
         * \throw out_of_range
        */
        [[nodiscard]]
        auto back () const -> char_type
        {
            return *chars().backward().begin();
        }

        /**
         * \brief Predicate. Returns `true` if the string contains ASCII-only characters
         *
         * \note ASCII-subset of Unicode is presented by code points 0-127 (`0x00`-`0x7F`)
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
         * \param ch Character's code point
         *
         * \note ASCII-subset of Unicode is presented by code points 0-127 (`0x00`-`0x7F`)
        */
        [[nodiscard]]
        static auto is_ascii (char_type ch) -> bool
        {
            return ch < 0x80;
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
                if (is_ascii(*ptr)) *ptr = uint8_t(tolower(*ptr));
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
                if (is_ascii(*ptr)) *ptr = uint8_t(toupper(*ptr));
            }
            return *this;
        }

        /**
         * \brief Compares two strings by equality
         * 
         * \param str String to compare with
         * 
         * \return `true` if `*this` is equivalent to `str`; `false` otherwise
        */
        [[nodiscard]]
        auto operator == (string const& str) const -> bool
        {
            return (size() == str.size()) && std::equal(bytes(), bytes_end(), str.bytes());
        }

        /**
         * \brief Compares two strings by non-equality
         * 
         * \param str String to compare with
         * 
         * \return `true` if `*this` differs from `str`; `false` otherwise
        */
        [[nodiscard]]
        auto operator != (string const& str) const -> bool
        {
            return !(*this == str);
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
         * \brief Checks if the string's data lexicographically less than the other's
         * 
         * \param str String to compare with
         * 
         * \return `true` if the string is lexicographically less than the second;
         * `false` otherwise
        */
        [[nodiscard]]
        auto operator < (string const& str) const -> bool
        {
            return chars() < str.chars();
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
         * \brief Predicate operator. Returns `true` if string is not empty
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
         * \param ch Code point of appending character
         *
         * \return Reference to the modified string
        */
        auto push (char_type ch) -> string&
        {
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
         * \brief Removes the last character from the string and returns its code point
         * 
         * \throw out_of_range
        */
        auto pop () -> char_type
        {
            auto it = chars().backward().begin();
            _end = it._base();

            return *it;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param pos Inserting position
         * \param value Unicode character's code point
         *
         * \return Reference to the modified string
         * 
         * \throw invalid_argument
        */
        auto insert (size_type pos, char_type value) -> string&
        {
            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            _encode(_spread((chars().begin() + pos)._base(), size() + _codebytes(value)), value);
            return *this;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param iter Inserting position (by iterator)
         * \param value Unicode character's code point
         *
         * \return Reference to the modified string
         * 
         * \throw out_of_range
        */
        auto insert (view::iterator const& iter, char_type value) -> string&
        {
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
                if (auto range = find(vi); !! range) replace(range, other);
                else
                    break;
            }

            return *this;
        }

        /**
         * \brief Replaces all characters satisfying specified criteria by another
         * 
         * \param pred Checking predicate
         * \param value Replacing character's code point
         * 
         * \return Reference to the modified string
        */
        template <typename Functor>
        auto replace_all_if (Functor&& pred, char_type value) -> string&
        {
            return *this;
        }

        /**
         * \brief Replaces all occurences of the given character by another
         * 
         * \param what Character to replace
         * \param value Replacing character's code point
         * 
         * \return Reference to the modified string
        */
        auto replace_all (char_type what, char_type value) -> string&
        {
            return replace_all_if(
                [&what](char_type ch){ return ch == what; },
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
         * \brief Removes the characters in the given range
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
                std::copy_n(vi_en, bytes_end() - vi_en, vi_be);
                _end -= vi.size();
            }

            return *this;
        }

        /**
         * \brief Removes some characters starting from the given index
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
         * \brief Search for the given substring inside entire string
         * 
         * \param vi Substring to search (by its view)
         * 
         * \return View of first occurrence of the substring or `[end(); end())` if it does not found
        */
        [[nodiscard]]
        auto find (view const& vi) const -> view
        {
            auto it_1 = chars().find(vi);
            auto it_2{ it_1 };

            if (!! it_1) it_2._ptrbase = it_1._base() + vi.size();

            return { it_1, it_2 };
        }

        /**
         * \brief Counts the number of substring occurences
         * 
         * \param vi Substring to count (by its view)
         * 
         * \return Number of occurences
        */
        [[nodiscard]]
        auto count (view const& vi) const -> size_type
        {
            return chars().count(vi);
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
            return chars().count_if(pred);
        }

        /**
         * \brief Counts the number of occurences of the given characters
         * 
         * \param value Character to count
         * 
         * \return Number of occurences
        */
        [[nodiscard]]
        auto count (char_type value) const -> size_type
        {
            return chars().count(value);
        }

        /**
         * \brief Predicate. Returns `trie` if the string contains specified substring (by its view)
         * 
         * \param vi View to check the substring's containing
        */
        [[nodiscard]]
        auto contains (view const& vi) const -> bool
        {
            return chars().contains(vi);
        }

        /**
         * \brief Predicate. Returns `trie` if the string contains at least single specified character
         * 
         * \param value Character to check its containing (given by its code point)
        */
        [[nodiscard]]
        auto contains (char_type value) const -> bool
        {
            return chars().contains(value);
        }

        /**
         * \brief Predicate. Returns `trie` if the string contains characters satisfying specified criteria
         * 
         * \param pred Predicate to check
        */
        template <typename Functor>
        [[nodiscard]]
        auto contains_if (Functor&& pred) const -> bool
        {
            return chars().contains_if(pred);
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
                if (pred(*it)) erase({ it, it + 1 });
                else
                    ++it;
            }
            return *this;
        }

        /**
         * \brief Removes all occurrences of the character in the string
         * 
         * \param value Character to remove (presented by its code point)
         * 
         * \return Reference to the modified string
        */
        auto remove (char_type value) -> string&
        {
            return remove_if(
                [&value](char_type ch){ return ch == value; }
            );
        }

        /**
         * \brief Removes all occurences of the substring in the current string
         * 
         * \param vi Substring to remove (by its view)
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
         * \param other String to replace
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
         * \param other String to replace
         *
         * \return Reference to the modified string object
         * 
         * \throw invalid_argument
        */
        auto replace (size_type pos, size_type N, view const& other) -> string&
        {
            return replace(chars(pos, N), other);
        }

        /**
         * \brief Replaces a character by given index by other string
         *
         * \param pos Replacement position
         * \param other String to replace
         *
         * \return Reference to the modified string object
         * 
         * \throw invalid_argument
        */
        auto replace (size_type pos, view const& other) -> string&
        {
            return replace(pos, 1, other);
        }

        /**
         * \brief Replaces a character by an iterator by other string
         *
         * \param iter Replacement position (by an iterator)
         * \param other String to replace
         *
         * \return Reference to the modified string object
         * 
         * \throw out_of_range
        */
        auto replace (view::iterator const& iter, view const& other) -> string&
        {
            return replace({ iter, iter + 1 }, other);
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
         * \param pos Position of splitting
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
         * \return Characher's code point
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
         * \brief Counts the bytes to store the encoded UTF-8 character by its code point
         * 
         * \param value Character's code point
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
         * \brief Encodes an UTF-8 character
         * 
         * \param dest Destination pointer to the UTF-8 location inside the buffer
         * \param value Character's code point
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

    };

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

    // ANCHOR Char-by-char i/o
    /**
     * \brief Reads an UTF-8 character from an input stream
     * 
     * \param in Input stream to read from
     * 
     * \return Character's code point
    */
    [[nodiscard]]
    auto read (std::istream& in) -> string::char_type
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
     * \param value Character's code point
    */
    auto write (std::ostream& out, string::char_type value) -> void
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

        for (auto ch = read(is); is && !isspace(ch); ch = read(is))
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
            write(os, ch);
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
     * \details The value of `base` must be in range [2; 36]. Otherwise, an exception will be thrown
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
}

namespace std
{
    /**
     * \struct iterator_traits
     * 
     * \brief Iterator traits instantiation for the `string_view::iterator` class
    */
    template<>
    struct iterator_traits <utf::string_view::iterator>
    {
        using difference_type = utf::string::difference_type;
        using value_type = utf::string::char_type;
        using iterator_category = std::bidirectional_iterator_tag;
    };

    /**
     * \brief std::swap implementation for strings
     * 
     * \param s1 First string
     * \param s2 Second string
    */
    auto swap (utf::string& s1, utf::string& s2) noexcept -> void
    {
        s1.swap(s2);
    }
}

#endif
