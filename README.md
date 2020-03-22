![utf8cpp](https://user-images.githubusercontent.com/54913619/76170749-e8ae9b80-6195-11ea-8780-efbf16e64d9a.png)

![version](https://img.shields.io/badge/version-0.8-brightgreen)
[![license](https://img.shields.io/badge/license-MIT-blue)](LICENSE.md)
[![help](https://img.shields.io/badge/help-wiki-red)](../../wiki)
![cpp-version](https://img.shields.io/badge/C%2B%2B-≥17-blue)

Small UTF-8-based string library with some modern improvements

## Overview
The class `utf::string` describes a dynamically, array-based, contiguous storage of UTF-8-encoded characters set.

### Features
* Dynamic length;
* Methods chaining;
* Fixed [*"Unsigned size_type problem"*](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1227r1.html#motivation "What is this") — `utf::string::size_type` is `intmax_t`, unlike STL's `size_t`;
* Non-owning inner type for viewing and iteration — `utf::string::view` (also `utf::string_view`)...
* ...and rights to view and change are completely divided between `string`s and `view`s by design!

### Installation
1. ![download-solid](https://user-images.githubusercontent.com/54913619/76699933-4a559500-66c3-11ea-978a-48808ab0f852.png) [Download](https://github.com/qzminsky/utf8cpp/archive/v0.8.3.zip) the library source;
2. `#include` the `string.h` file in your C++ project;
3. Enjoy!

> ⚠️ *Note that the library requires C++17 support*

## Usage examples
* Creating the string:
```C++
// Constructing via const char*
utf::string MyString1{ "Amazing chest ahead" };

// Using std::initializer_list with integral code points
auto MyString2 = utf::string::from_unicode({ 'L',0xf6,'w','e','L',0xe9,'o','p','a','r','d' });

// Using vector of bytes (also UTF-8 representation)
auto MyString3 = utf::string::from_bytes({'B','y','t','e','s'});
```
* Iterating over the characters:
```C++
utf::string Line{ "Il buono, il brutto, il cattivo" };

// Using C++20 init-for
for (auto view = Line.chars(); auto ch : view)
{
  std::cout << ch << std::endl;   // prints chars' code points
}
```
* Chaining:
```C++
utf::string Line{ "Mr Dursley was the director of a firm called Grunnings" };

// Remove all spaces
Line.clone().remove(' ');
    /* or */
Line.clone().remove_if(utf::isspace);
    /* or */
Line.clone().remove_if([](utf::string::char_type ch) { return isspace(ch); });

// Cut the last word off
Line.first(Line.chars().reverse().find_if(isspace).as_index()).to_string();
//  ↑                                                         ↑
//  no need to clone here — using the view and actually clone here
```

## Complexity
* Access to the:
  * Single character:
    * `front()`, `back()` — *constant* / **O(1)**
    * *N*-th (`get(N)`) — *linear* / **O(N)**
    * Back character with removal (`pop()`) — *constant* / **O(1)**
  * Substring's view (by `chars(...)`, `first(...)`, `last(...)`) — *linear* / **O(N)**
  * Entire string's view (`chars()`) — *constant* / **O(1)**
* Insertion — *linear* / **O(N)**; requires extra memory reallocation
* Search (`find*(...)`, `contains*(...)`, `count*(...)`) / erasure (`erase(...)`, `remove*(...)`) — *linear* / **O(N)**
* Length calculation — *linear* / **O(N)** as it requires iteration over every character in the string

Note that a replacement (`replace*(...)`) is more complicated. It behaves like an insertion if the new substring is longer (by its `size()`) than the replacement. Otherwise, the operation does not requires an extra memory and behaves like an erasure; both cases have *linear* / **O(N)** time complexity.

## License
See the [LICENSE](LICENSE.md) file for license rights and limitations (MIT).
