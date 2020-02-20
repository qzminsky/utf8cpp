# utf8cpp
Simple UTF-8-based string class with some modern improvements

## Overview
The class `utf::string` describes a dynamically, array-based, contiguous storage of UTF-8-encoded characters set.

### Features
* Dynamic length;
* Methods chaining;
* Fixed [*"Unsigned size_type problem"*](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1227r1.html#motivation "What is this") — `utf::string::size_type` is `ptrdiff_t`, unlike STL's `size_t`;
* Non-owning inner type for viewing and iteration — `utf::string::view` (also `utf::string_view`)...
* ...and rights to view and change are completely divided between `string`s and `view`s by design!

### Installation
1. Download and copy the file `string.h` into your project headers' directory;
2. `#include` it;
3. Enjoy!

*Note that library requires C++17 support*

## Usage examples
* Creating the string:
```C++
// Constructing via const char*
utf::string MyString1{ "Amazing chest ahead" };

// Using std::initializer_list with integral code points
utf::string MyString2{ 'L',0xf6,'w','e','L',0xe9,'o','p','a','r','d' };

// Using vector of bytes (also UTF-8 representation)
utf::string MyString3{ std::vector<uint8_t>{'B','y','t','e','s'} };
```
* Iterating over characters (using C++20 init-for):
```C++
utf::string Line{ "Il buono, il brutto, il cattivo" };

for (auto view = Line.chars(); auto ch : view) {
  std::cout << ch << std::endl;   // Prints chars' code points
}
```
* Chaining:
```C++
utf::string Line{ "Mr Dursley was the director of a firm called Grunnings" };

// Remove all spaces
Line.clone().remove(isspace);
    /* or */
Line.clone().remove(' ');
    /* or */
Line.clone().remove([](utf::string::char_type ch) { return isspace(ch); });

// Cut the last word off
Line.chars(0, Line.chars().reverse().find(isspace).as_index()).to_string();
//  ↑                                                         ↑
//  no need to clone here — using the view and actually clone here
```

## Complexity
* Access to:
  * Single character:
    * `front()`, `back()` — *constant* / **O(1)**
    * *N*-th (`get(N)`) — *linear* / **O(N)**
    * Back character with removal (`pop()`) — *constant* / **O(1)**
  * Substring's view (by `chars(...)`, `left(...)`, `right(...)`) — *linear* / **O(N)**
  * Entire string's view (`chars()`) — *constant* / **O(1)**
* Insertion — *linear* / **O(N)**; requires extra memory reallocation
* Search (`find(...)`, `contains(...)`) / erasure (`erase(...)`, `remove(...)`) — *linear* / **O(N)**
* Length calculation — *linear* / **O(N)** as it requires iteration over every character in the string

Note that replacement (`replace(...)`) is more complicated. It behaves like insertion if the new substring is longer (by its `size()`) than the replacement. Otherwise, the operation does not requires an extra memory and behaves like erasure; both cases have *linear* / **O(N)** time complexity.
