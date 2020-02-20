#pragma once

#include <algorithm>
#include <cstdint>
#include <exception>
#include <functional>
#include <initializer_list>
#include <iostream>
#include <limits>
#include <string>
#include <utility>
#include <vector>

namespace utf
{
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
     * \version 0.1.0
     * \date 2020/01/30
    */
    class string
    {
    public:

        // ANCHOR Typedefs
        using size_type = ptrdiff_t;
        using difference_type = ptrdiff_t;
        using pointer = uint8_t*;
        using char_type = uint32_t;

        /// The special value. The exact meaning depends on context
        static constexpr auto npos = std::numeric_limits<size_type>::max();

    private:

        /*            size() == end - bytes()
         *  ╭—————————˄——————————╮
         * [x xx x xx xxx x xxxx x].  -- data
         *  ↑                      ↑
         *  repr == bytes()        end
        */

        pointer repr = nullptr, end = nullptr;

    public:

        // SECTION Inner types
        // ANCHOR Type: view
        /**
         * \class view
         * 
         * \brief An iterable, non-owning proxy type for string
         * 
         * \details Desribes an iterable range with two pointers and direction flag.
         * Forward direction means an iteration to the higher addresses; backward -- to the lower addresses.
         * The view doesn't provides any mutators to the original string
        */
        class view
        {
            friend class string;

            // An iterable range
            pointer forward_begin = nullptr, forward_end = nullptr;

            // Direcion flag
            bool is_forward;

            /* end(⇐)           begin(⇐)
             * ↓                 ↓
             * .[x xx x xx xxx x xxxx].
             *   ↑                    ↑
             *   begin(⇒)            end(⇒)
             *  forward_begin       forward_end
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
                pointer ptrbase = nullptr;

                // The view which created the iterator. Using for select direction and range control
                const view* parent = nullptr;

            public:

                iterator() = delete;
                iterator(iterator const&) = default;
                iterator(iterator&&) = default;

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto operator ++ () -> iterator&
                {
                    return parent->is_forward ? _forward_increase() : _forward_decrease();
                }

                /**
                 * \brief Offsets the iterator to the previous character consider direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto operator -- () -> iterator&
                {
                    return parent->is_forward ? _forward_decrease() : _forward_increase();
                }

                /**
                 * \brief Offsets the iterator to the next character consider direction
                 * 
                 * \return Previous iterator's state as copy of it
                */
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
                        for (; count--; prev = _base()) {
                            ++(*this);
                            if (prev == _base()) break;
                        }
                    else
                        for (; count++; prev = _base()) {
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

                // TODO Doc here
                auto operator + (difference_type count) const -> iterator
                {
                    iterator tmp = *this;
                    return tmp += count;
                }

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
                 * \return The number of increments needed to go from `other` to `*this`. If
                 * `*this` cannot be reached from `other`, `string::npos` will be returned
                 * 
                 * \note It also works with backward-directed views' iterators
                */
                auto operator - (iterator other) const -> difference_type
                {
                    difference_type count = 0;

                    if (other <= *this) {
                        for (; *this != other; ++other) ++count;
                    }
                    else {
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
                auto operator * () const -> char_type
                {
                    if (*this == parent->end()) {
                        throw out_of_range{ "Out of bounds iterator dereferencing" };
                    }
                    return string::_decode(ptrbase);
                }

                /**
                 * \brief Iterator dereferencing with specified offset
                 * 
                 * \param index Offset from current iterator
                 * 
                 * \return Code point of the character `*this + index`
                 * 
                 * \note This operator returns a value, not a reference
                */
                auto operator [] (difference_type index) const -> char_type
                {
                    return *(*this + index);
                }

                /**
                 * \brief Transorms an iterator into character's index in the parent view
                 * 
                 * \return Count of hops from the beginning of the view
                */
                auto as_index() const -> size_type
                {
                    return *this - view{*parent}.reverse().begin();
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
                    return ptrbase == other.ptrbase;
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
                 * equal each other by increasing `X` sequentally (more than 0 times)
                */
                [[nodiscard]]
                auto operator < (iterator const& other) const -> bool
                {
                    return parent->is_forward ? (ptrbase < other.ptrbase) : (ptrbase > other.ptrbase);
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

                [[nodiscard]]
                auto operator > (iterator const& other) const -> bool
                {
                    return !(*this <= other);
                }

                [[nodiscard]]
                auto operator >= (iterator const& other) const -> bool
                {
                    return !(*this < other);
                }

            private:
            
                /**
                 * \internal
                 * \brief Constructs the iterator from address bounded to specified view
                 * 
                 * \param dp Pointer to the character inside the view
                 * \param whos Parent view
                */
                iterator(pointer dp, const view* whos) : ptrbase{ dp }, parent{ whos } {};

                /**
                 * \internal
                 * \brief Offsets the iterator to the previous character in forward direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto _forward_decrease() -> iterator&
                {
                    if (ptrbase == parent->bytes() || ptrbase == parent->_forward_rend()._base()) {
                        ptrbase = parent->bytes() - 1;
                    }
                    else while (ptrbase && (*--ptrbase & 0xC0) == 0x80);

                    return *this;
                }

                /**
                 * \internal
                 * \brief Offsets the iterator to the next character in forward direction
                 * 
                 * \return Reference to the modified iterator
                */
                auto _forward_increase() -> iterator&
                {
                    if (ptrbase != parent->forward_end) {
                        ptrbase += string::_charsize(ptrbase);
                    }
                    return *this;
                }

                /**
                 * \internal
                 * \brief Getting the base pointer of the iterator
                */
                auto _base() const -> pointer
                {
                    return ptrbase;
                }
            };


            view() = delete;
            view(view const&) = default;
            view(view&&) = default;

            view(string& other) : forward_begin{ other.bytes() }, forward_end{ other.end }, is_forward{ true } {}

            // TODO Doc here
            view(iterator const& be, iterator const& en)
                : forward_begin{ std::min(be._base(), en._base()) },
                  forward_end{ std::max(be._base(), en._base()) },
                  is_forward{ true }
            {}

            /**
             * \brief Creates a view-based string as the copy of original
             * 
             * \return Copy of the original string in specified characters range
            */
            auto to_string() const -> string
            {
                return *this;
            }

            /**
             * \brief Turns the iterating direction over
             * 
             * \return Reference to the modfied view
            */
            auto reverse() -> view&
            {
                is_forward = !is_forward;
                return *this;
            }

            /**
             * \brief Returns the beginning iterator consider direction
            */
            auto begin() const -> iterator
            {
                return is_forward ? iterator{ bytes(), this } : _forward_rbegin();
            }

            /**
             * \brief Returns the ending iterator consider direction
            */
            auto end() const -> iterator
            {
                return is_forward ? iterator{ forward_end, this } : _forward_rend();
            }

            /**
             * \brief Returns the pointer to the beginning of the view's data
            */
            [[nodiscard]]
            auto bytes() const -> pointer
            {
                return forward_begin;
            }

            [[nodiscard]]
            auto size() const -> size_type
            {
                return forward_end - bytes();
            }

            [[nodiscard]]
            auto length() const -> size_type
            {
                return end() - begin();
            }

            /**
             * \brief Predicate. Returns `true` if the view's range has zero-size
            */
            [[nodiscard]]
            auto is_empty() const -> bool
            {
                return size() == 0;
            }


            auto find(view const& vi) const -> iterator
            {
                for (auto it = begin(); it != end(); ++it) {
                    if (it._base() + vi.size() < forward_end && !memcmp(it._base(), vi.bytes(), vi.size())) return it;
                }
                return end();
            }

            auto find(string const& what) const -> iterator
            {
                return find(what.chars());
            }

            auto find(std::function<bool(char_type)> const& pred) const -> iterator
            {
                for (auto it = begin(); it != end(); ++it) {
                    if (pred(*it)) return it;
                }
                return end();
            }

            auto find(char_type value) const -> iterator
            {
                return find(
                    [&value](char_type ch){ return value == ch; }
                );
            }

            [[nodiscard]]
            auto contains(view const& vi) const -> bool
            {
                return find(vi) != end();
            }

            [[nodiscard]]
            auto contains(string const& what) const -> bool
            {
                return contains(what.chars());
            }

            [[nodiscard]]
            auto contains(std::function<bool(char_type)> const& pred) const -> bool
            {
                return find(pred) != end();
            }

            [[nodiscard]]
            auto contains(char_type value) const -> bool
            {
                return find(value) != end();
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
            auto truncate(size_type off, size_type N) -> view&
            {
                if (off < 0) throw invalid_argument{ "Negative subspan offset" };
                if (N < 0) throw invalid_argument{ "Negative subspan length" };

                if (is_forward) {
                    forward_begin = (begin() + off)._base();
                    forward_end = (begin() + N)._base();
                }
                else {
                    forward_end = (iterator{ forward_end, this } + off)._base();
                    forward_begin = (iterator{ forward_end, this } + N)._base();
                }

                return *this;
            }

            friend auto operator << (std::ostream& os, view const& vi) -> std::ostream&
            {
                for (auto ch : vi) {
                    write(os, ch);
                }
                return os;
            }

            auto operator == (view const& other) const -> bool
            {
                return (size() == other.size()) && (memcmp(bytes(), other.bytes(), size()) == 0);
            }
            auto operator != (view const& other) const -> bool
            {
                return !(*this == other);
            }

            auto operator == (string const& str) const -> bool
            {
                return (size() == str.size()) && (memcmp(bytes(), str.bytes(), size()) == 0);
            }
            auto operator != (string const& str) const -> bool
            {
                return !(*this == str);
            }

            /**
             * \brief Predicate. Checks if the view contains only valid UTF-8 characters
            */
            auto is_valid() const -> bool
            {
                for (auto ch = bytes(); ch != forward_end; ++ch)
                {
                    if (is_ascii(*ch)) {
                        continue;
                    }
                    else if (auto sz = _charsize(ch); sz > 4) {
                        return false;
                    }
                    else while (--sz) {
                        if (++ch == forward_end || (*ch & 0xC0) != 0x80) return false;
                    }
                }
                return true;
            }

        private:
        
            /**
             * \internal
             * \brief Constructs the view via given string pointer (used by `chars()`)
             * 
             * \param base Pointer to the `string` object
            */
            explicit view(string const* base) : forward_begin{ base->bytes() }, forward_end{ base->end }, is_forward{ true } {}

            /**
             * \internal
             * \brief Returns the beginning iterator on backward iterating direction
             * 
             * \note Returning iterator points to the last character of the view
            */
            auto _forward_rbegin() const -> iterator
            {
                return iterator{ forward_end, this }._forward_decrease();
            }

            /**
             * \internal
             * \brief Returns the ending iterator on backward iterating direction
             * 
             * \note Returning iterator points to the previous byte of the first character of the view
            */
            auto _forward_rend() const -> iterator
            {
                return { bytes() - 1, this };
            }
        };
        // !SECTION

        friend auto read(std::istream&) -> char_type;
        friend auto write(std::ostream&, char_type) -> void;

        /**
         * \brief Default constructor
        */
        string() = default;

        /**
         * \brief Copy constructor
         * 
         * \param other String to copy
        */
        string(string const& other) : repr{ new uint8_t[other.size()] } {
            end = bytes() + other.size();
            memcpy(bytes(), other.bytes(), size());
        }

        /**
         * \brief Move constructor
         * 
         * \param other String to move from
        */
        string(string&& other) noexcept : repr{ std::exchange(other.repr, nullptr) }, end{ std::exchange(other.end, nullptr) } {}

        /**
         * \brief Constructs a string via given array of Unicode code points
         * 
         * \param data Initializer list
        */
        string(std::initializer_list<char_type> data) {
            size_type size = 0; for (auto ch : data) {
                size += _codebytes(ch);
            }

            repr = new uint8_t[size];
            end = bytes() + size;

            auto dit = bytes(); for (auto ch : data) dit = _encode(dit, ch);
        }

        /**
         * \brief Constrcuts a string via UTF-8 buffer stored in vector
         * 
         * \param vec UTF-8-encoded characters' vector to construct from
         * 
         * \note The reverse operation is `make_bytes()`
        */
        string(std::vector<uint8_t> const& vec) : repr{ new uint8_t[vec.size()] } {
            end = repr + vec.size();
            memcpy(bytes(), vec.data(), size());
        }

        /**
         * \brief Converting constructor from `std::string`
         * 
         * \param stds Source `std::string` to construct from
        */
        string(std::string const& stds) : repr{ new uint8_t[stds.size()] } {
            end = repr + stds.size();
            memcpy(bytes(), stds.data(), size());
        }

        /**
         * \brief Converting constructor from C-string
         * 
         * \param ascii Source C-string (`const char*`) to construct from
        */
        string(const char* ascii) { _bufinit((void*)ascii, strlen(ascii)); }

        /**
         * \brief Converting constructor from the view
         * 
         * \param vi View providing the set of characters to copy
        */
        string(view const& vi) : repr{ new uint8_t[vi.size()] } {
            end = bytes() + vi.size(); auto ptr = bytes();
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
            if (this != &other) {
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
            if (this != &other) {
                repr = std::exchange(other.repr, nullptr);
                end = std::exchange(other.end, nullptr);
            }
            return *this;
        }

        auto operator = (const char* ascii) -> string&
        {
            _bufinit((void*)ascii, strlen(ascii));
        }

        ~string() { delete[] bytes(); }

        /**
         * \brief Returns the copy of the original string
        */
        [[nodiscard]]
        auto clone() const -> string
        {
            return *this;
        }

        /**
         * \brief Creates an iterable object
         *
         * \return `view`-wrapper over current string
         *
         * \note This is an `O(1)` operation as it uses pre-known range
        */
        [[nodiscard]]
        auto chars() const -> view
        {
            return view{ this };
        }

        /**
         * \brief Creates an iterable object with specified range
         *
         * \param shift Starting character position
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over current string
         *
         * \note This is an `O(n)` operation
        */
        [[nodiscard]]
        auto chars(size_type shift, size_type N = npos) const -> view
        {
            return chars().truncate(shift, N);
        }

        /**
         * \brief Creates an iterable object of `N` characters from left side of the string
         *
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over current string
         *
         * \note This is an `O(n)` operation
        */
        [[nodiscard]]
        auto left(size_type N) const -> view
        {
            return chars(0, N);
        }

        /**
         * \brief Creates an iterable object of `N` characters from right side of the string
         *
         * \param N Number of slicing characters
         *
         * \return `view`-wrapper over current string
         *
         * \note This is an `O(n)` operation
        */
        [[nodiscard]]
        auto right(size_type N) const -> view
        {
            return chars().reverse().truncate(0, N).reverse();
        }

        /**
         * \brief Inserts characters from the string into the stream
         * 
         * \param os Reference to the output stream
         * \param str String to insert
         * 
         * \return Reference to the output stream
        */
        friend auto operator << (std::ostream& os, string const& str) -> std::ostream&
        {
            return os << str.chars();
        }

        /**
         * \brief Returns the pointer to the beginning of the string's data
        */
        [[nodiscard]]
        auto bytes() const -> pointer
        {
            return repr;
        }

        /**
         * \brief Creates and returns an `std::vector` object containing buffer bytes data
        */
        [[nodiscard]]
        auto make_bytes() const -> std::vector<uint8_t>
        {
            return std::vector<uint8_t>(bytes(), end);
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
        auto get(size_type index) const -> char_type
        {
            if (index < 0) throw invalid_argument{ "Negative character index" };

            return *(chars().begin() + index);
        }

        /**
         * \brief Getting the code point of the first character of the string
         * 
         * \throw out_of_range
        */
        [[nodiscard]]
        auto front() const -> char_type
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
        auto back() const -> char_type
        {
            return *chars().reverse().begin();
        }

        /**
         * \brief Predicate. Returns `true` if the string contains ASCII-only characters
         *
         * \note ASCII-subset of Unicode is presented by code points 0-127 (`0x00`-`0x7F`)
        */
        [[nodiscard]]
        auto is_ascii() const -> bool
        {
            for (auto ptr = bytes(); ptr != end; ++ptr) {
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
        static auto is_ascii(char_type ch) -> bool
        {
            return ch < 0x80;
        }

        /**
         * \brief Converts all ASCII characters in the string into lowercase
         * 
         * \return Reference to the modified string
        */
        auto to_lower_ascii() -> string&
        {
            for (auto ptr = bytes(); ptr != end; ++ptr) {
                if (is_ascii(*ptr)) *ptr = uint8_t(tolower(*ptr));
            }
            return *this;
        }

        /**
         * \brief Converts all ASCII characters in the string into uppercase
         * 
         * \return Reference to the modified string
        */
        auto to_upper_ascii() -> string&
        {
            for (auto ptr = bytes(); ptr != end; ++ptr) {
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
            return (size() == str.size()) && (memcmp(bytes(), str.bytes(), size()) == 0);
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
         * \return `true` if `*this` is equivalent to `sview`'s data; `false` otherwise
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
         * \return `true` if `*this` differs from `sview`'s data; `false` otherwise
        */
        [[nodiscard]]
        auto operator != (view const& vi) const -> bool
        {
            return vi != *this;
        }

        /**
         * \brief Predicate. Returns `true` if all of characters in the string are valid UTF-8-encoded
        */
        [[nodiscard]]
        auto is_valid() const -> bool
        {
            return chars().is_valid();
        }

        /**
         * \brief Predicate. Returns `true` if string does not contains any characters
        */
        [[nodiscard]]
        auto is_empty() const -> bool
        {
            return size() == 0;
        }

        /**
         * \brief Returns the number of Unicode characters in this string
         *
         * \warning This isn't equivalent to `size()`, which returns exactly the number of *bytes*.
         * Each UTF-8 character has a different size, from 1 to 4 bytes
         * \note This is an `O(n)` operation as it requires iteration over every UTF-8 character of the string
        */
        [[nodiscard]]
        auto length() const -> size_type
        {
            return chars().length();
        }

        /**
         * \brief Returns the size of the memory buffer used by the string
         *
         * \warning This isn't equivalent to `length()`, which returns the number of *characters*.
         * Each UTF-8 character has a different size, from 1 to 4 bytes
        */
        [[nodiscard]]
        auto size() const -> size_type
        {
            return end - bytes();
        }

        /**
         * \brief Appends a given Unicode character to the end of the string
         *
         * \param ch Code point of appending character
         *
         * \return Reference to the modified string
        */
        auto push(char_type ch) -> string& {
            _encode(_expanded_copy(size() + _codebytes(ch)), ch);
            return *this;
        }

        /**
         * \brief Appends a given string to the end of current
         *
         * \param other Appending string
         *
         * \return Reference to the modified string
        */
        auto push(string const& other) -> string&
        {
            memcpy(_expanded_copy(size() + other.size()), other.bytes(), other.size());
            return *this;
        }

        /**
         * \brief Removes the last character from the string and returns its code point
        */
        auto pop() -> char_type
        {
            auto it = chars().reverse().begin();
            end = it._base();

            return *it;
        }

        /**
         * \brief Inserts the Unicode character into the string
         *
         * \param pos Inserting position
         * \param ch Unicode character's code point
         *
         * \return Reference to the modified string
         * 
         * \throw invalid_argument
        */
        auto insert(size_type pos, char_type ch) -> string&
        {
            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            _encode(_spread((chars().begin() + pos)._base(), size() + _codebytes(ch)), ch);
            return *this;
        }

        /**
         * \brief Inserts another string into current
         *
         * \param pos Inserting position
         * \param other String to insert
         *
         * \return Reference to the modified string
         * 
         * \throw invalid_argument
        */
        auto insert(size_type pos, string const& other) -> string&
        {
            if (pos < 0) throw invalid_argument{ "Negative inserting position" };

            memcpy(_spread((chars().begin() + pos)._base(), size() + other.size()), other.bytes(), other.size());
            return *this;
        }

        /**
         * \brief Completely clears the string by deallocating its owned memory
         * 
         * \return Reference to the modified string
        */
        auto clear() -> string&
        {
            delete[] bytes();
            repr = end = nullptr;

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
        auto erase(view::iterator const& iter) -> string&
        {
            if (auto ptr = iter._base(); ptr < bytes() || ptr >= end) {
                throw out_of_range{ "Given iterator does not point into modifying string" };
            }
            else {
                auto sz = _charsize(ptr);
                memmove(ptr, ptr + sz, end - ptr - sz);

                end -= sz;
            }
            return *this;
        }

        /**
         * \brief Removes the characters in the given range
         *
         * \param vi View providing the range
         *
         * \return Reference to the modified string
        */
        auto erase(view const& vi) -> string&
        {
            memmove(vi.begin()._base(), vi.end()._base(), end - vi.end()._base());
            end -= vi.size();

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
        */
        auto erase(size_type pos, size_type N = 1) -> string&
        {
            return erase(chars(pos, N));

            /*     pos     N           end
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].      -- old state
             *  ↓    ↓ ←————————┘                          〉same buffer
             * [x xx x zzzz z ........].      -- new state
             *  ↑             ↑
             *  bytes()       end
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
        auto find(view const& vi) const -> view
        {
            auto it = chars().find(vi);
            return {it, it + vi.length()};
        }

        /**
         * \brief Search for the given substring inside entire string
         * 
         * \param what Substring to search
         * 
         * \return View of first occurrence of the substring or `[end(); end())` if it does not found
        */
        [[nodiscard]]
        auto find(string const& what) const -> view
        {
            return find(what.chars());
        }

        /**
         * \brief Predicate. Returns `trie` if the string contains specified substring (by its view)
         * 
         * \param vi View to check the substring's containing
        */
        [[nodiscard]]
        auto contains(view const& vi) const -> bool
        {
            return chars().contains(vi);
        }

        /**
         * \brief Predicate. Returns `trie` if the string contains specified substring
         * 
         * \param what Substring to check its containing
        */
        [[nodiscard]]
        auto contains(string const& what) const -> bool
        {
            return contains(what.chars());
        }

        // TODO Doc here
        [[nodiscard]]
        auto contains(char_type value) const -> bool
        {
            return chars().contains(value);
        }

        [[nodiscard]]
        auto contains(std::function<bool(char_type)> const& pred) const -> bool
        {
            return chars().contains(pred);
        }

        /**
         * \brief Removes all characters satisfying the predicate
         * 
         * \param pred Predicate to check
         * 
         * \return Reference to the modified string
        */
        auto remove(std::function<bool(char_type)> const& pred) -> string&
        {
            for (auto it = chars().begin(); it._base() != end;) {
                if (pred(*it)) {
                    erase({it, it + 1});
                }
                else ++it;
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
        auto remove(char_type value) -> string&
        {
            return remove(
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
        auto remove(view const& vi) -> string&
        {
            auto len = vi.length();

            for (auto it = chars().begin(); it._base() != end;) {
                if (auto rvi = view{it, it + len}; rvi == vi) {
                    erase(rvi);
                }
                else ++it;
            }
            return *this;
        }

        /**
         * \brief Removes all occurences of the substring in the current string
         * 
         * \param other Substring to remove
         * 
         * \return Reference to the modified string
        */
        auto remove(string const& other) -> string&
        {
            return remove(other.chars());
        }


        /**
         * \brief Replaces the characters in the given range by other string
         *
         * \param vi View providing the range
         * \param other String to replace
         *
         * \return Reference to the modified string object
        */
        auto replace(view const& vi, string const& other) -> string&
        {
            auto rsize = vi.size(), osize = other.size();
            auto ptrpos = vi.begin()._base(), tail = vi.end()._base();

            /*     pos     N           end
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].      -- old state
             *  ↓    ↓       ←——┘                          〉same buffer
             * [x xx x __ __ zzzz z ..].      -- new state
             *         ↑            ↑
             *         ptrpos       end
            */
            if (rsize > osize) {
                memmove(ptrpos + osize, tail, end - tail);
                end -= rsize - osize;
            }
            /*     pos     N           end
             *        \╭———˄——╮        ↓
             * [x xx x yy yyy y zzzz z].            -- old buffer
             *  ↓    ↓ └————→
             * [x xx x . ... __ ___ _ zzzz z].      -- new buffer
             *         ↑                     ↑
             *         ptrpos                end
            */
            else if (rsize < osize) {
                ptrpos = _spread(vi.begin()._base(), size() + osize - rsize);
            }

            memcpy(ptrpos, other.bytes(), osize);

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
        */
        auto replace(size_type pos, size_type N, string const& other) -> string&
        {
            return replace(chars(pos, N), other);
        }

        /**
         * \brief Reallocates the memory buffer used by a string
         *
         * \return Reference to the modified string object
        */
        auto shrink_to_fit() -> string&
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
        auto split_off(size_type pos) -> string
        {
            string tmp{ chars(pos) };
            erase(pos, npos);

            return tmp;
        }

        // TODO Doc here
        friend auto operator >> (std::istream& is, string& to) -> std::istream&
        {
            for (auto ch = read(is); !isspace(ch); ch = read(is)) to.push(ch);
            return is;
        }

        /**
         * \brief Swaps the contents of two string
         *
         * \param other String to exchange the contents with
         *
         * \note In actual, it swaps the memory pointers only, without reallocations
        */
        auto swap(string& other) noexcept -> void
        {
            std::swap(repr, other.repr);
            std::swap(end, other.end);
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
        auto _expanded_copy(size_type new_size) -> pointer
        {
            auto tmp = new uint8_t[new_size]; auto copy_bytes = size();

            memcpy(tmp, bytes(), copy_bytes);	// FIXME An UB caused of bytes() == nullptr in clear string
            delete[] bytes();

            repr = tmp; end = bytes() + new_size;

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
        auto _spread(pointer where, size_type new_size) -> pointer
        {
            auto shift = where - bytes();
            auto tail = _expanded_copy(new_size);

            where = bytes() + shift;
            memmove(where + (end - tail), where, tail - where);

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
        */
        auto _bufinit(void* buf, size_type bufsize) -> void
        {
            delete[] bytes();

            repr = new uint8_t[bufsize]; end = bytes() + bufsize;
            memcpy(bytes(), buf, bufsize);
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
        static auto _charsize(pointer where) -> size_type
        {
            if (!where) return 0;

            uint8_t tmp = *where; size_type res = 0;

            if (!(tmp & 0x80)) return 1;

            do {
                ++res; tmp <<= 1;
            } while (tmp & 0x80);

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
        static auto _decode(pointer where) -> char_type
        {
            if (!where) return 0;

            char_type result = 0;

            if (!(*where & 0x80)) return *where;
            else {
                result = *where & (0x7F >> _charsize(where));

                while ((*++where & 0xC0) == 0x80) {
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
        static auto _codebytes(char_type value) -> size_type
        {
            if (value < 0x80) return 1;
            else if (value < 0x800) return 2;
            else if (value < 0x10000) return 3;
            return 4;
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
        static auto _encode(pointer dest, char_type value) -> pointer
        {
            if (!dest) return nullptr;

            // One-byted subset [0; 127]
            if (value < 0x80) {
                *dest = value; return ++dest;
            }

            // Multibyte characters representation
            else {
                auto size = _codebytes(value), tmp = size;
                uint8_t mask = 0x80;

                while (--size) {
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
     * \brief UTF-8 `Byte Order Mark` Character
    */
    [[nodiscard]]
    constexpr auto BOM() -> string::char_type
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
    auto read(std::istream& in) -> string::char_type
    {
        uint8_t ch = in.get(); string::char_type result = 0;

        if (string::is_ascii(ch)) return ch;
        else {
            if ((ch & 0xE0) == 0xC0) {
                string::_encode(reinterpret_cast<uint8_t*>(&result), string::char_type(ch) << 8 | in.get());
                result >>= 16;
            }
            else if ((ch & 0xF0) == 0xE0) {
                string::_encode(reinterpret_cast<uint8_t*>(&result), string::char_type(ch) << 16 | in.get() | in.get());
                result >>= 8;
            }
            else {
                string::_encode(reinterpret_cast<uint8_t*>(&result), string::char_type(ch) << 24 | in.get() | in.get() | in.get());
            }
        }
        
        return result;
    }

    /**
     * \brief Writes an UTF-8 character into an output stream
     * 
     * \param out Output stream to write into
     * \param ch Charachter's code point
    */
    auto write(std::ostream& out, string::char_type ch) -> void
    {
        switch (string::_codebytes(ch)) {
        case 1: out << char(ch); break;
        case 2: out << uint8_t(ch >> 6 & 0x1F | 0xC0) << uint8_t(ch & 0x3F | 0x80); break;
        case 3: out << uint8_t(ch >> 12 & 0xF | 0xE0) << uint8_t(ch >> 6 & 0x3F | 0x80) << uint8_t(ch & 0x3F | 0x80); break;
        case 4: out << uint8_t(ch >> 18 & 0x7 | 0xF0) << uint8_t(ch >> 12 & 0x3F | 0x80) << uint8_t(ch >> 6 & 0x3F | 0x80) << uint8_t(ch & 0x3F | 0x80); break;
        }
    }
}

namespace std {
    auto swap(utf::string& s1, utf::string& s2) noexcept -> void
    {
        s1.swap(s2);
    }
}