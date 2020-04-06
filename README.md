![utf8cpp](https://user-images.githubusercontent.com/54913619/76170749-e8ae9b80-6195-11ea-8780-efbf16e64d9a.png)

![version](https://img.shields.io/badge/version-1.0-brightgreen)
[![license](https://img.shields.io/badge/license-MIT-blue)](LICENSE.md)
[![help](https://img.shields.io/badge/help-wiki-red)](../../wiki)
![cpp-version](https://img.shields.io/badge/C%2B%2B-≥17-blue)

Small UTF-8-based string library with some modern improvements

## Overview
The class `utf::string` describes a dynamic, contiguous storage of UTF-8-encoded characters set.

### Features
* Dynamic length;
* Methods chaining;
* Fixed [*"Unsigned size_type problem"*](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1227r1.html#motivation "What is this") — `utf::string::size_type` is `intmax_t`, unlike STL's `size_t`;
* Non-owning inner type for viewing and iteration — `utf::string::view` (also `utf::string_view`)...
* ...and rights to view and change are completely divided between `string`s and `view`s by design!

### Installation
1. ![download-solid](https://user-images.githubusercontent.com/54913619/76699933-4a559500-66c3-11ea-978a-48808ab0f852.png) [Download](https://github.com/qzminsky/utf8cpp/archive/v1.0.0.zip) the library source;
2. `#include` the `utf8string.hpp` file in your C++ project;
3. Enjoy!

> ⚠️ *Note that the library requires C++17 support*

## Usage examples
> *See more examples in the [`sample.cpp`](https://github.com/qzminsky/utf8cpp/blob/master/sample.cpp) source file*
* Creating the string:
```C++
// Constructing via const char*
utf::string MyString1 { "Amazing chest ahead" };

// Using std::initializer_list with integral code points
auto MyString2 = utf::string::from_unicode({ 'L',0xf6,'w','e', 'L',0xe9,'o','p','a','r','d' });

// From a vector of bytes (already encoded in UTF-8)
auto MyString3 = utf::string::from_bytes({ 'B','y','t','e','s' });

// As multiple copies of the character
utf::string MyString4 { 0xA2, 10 };   // == "¢¢¢¢¢¢¢¢¢¢"

// From an std::string
auto MyString5 = utf::string:from_std_string("Evil is evil");
```
* Iterating over the characters:
```C++
utf::string Line { "Il buono, il brutto, il cattivo" };

// Using C++20 init-for
for (auto view = Line.chars(); auto ch : view)
{
  std::cout << ch << std::endl;   // prints chars' code points
}
```
* Chaining:
```C++
utf::string Line { "Mr Dursley was the director of a firm called Grunnings" };

// Remove all spaces
Line.clone().remove(' ');
    /* or */
Line.clone().remove_if(utf::is_space /* handles over 20 different Unicode spaces */ );

// Cut the last word off
std::cout <<
Line.first(Line.chars().reverse().find_if(utf::is_space).as_forward_index()).to_string();
//  ↑                                                                       ↑
//  no need to clone here — just operating with the view and actually clone here
```
* Multi-pattern operations:
```C++
utf::string Line { "Stumbling everywhere" };

// Searches all occurences of every pattern in the parameter pack
   auto all_matches = Line.chars().matches("every", "everywhere", "around");
// ^^^^ - for substring-matching version the type is std::vector<view>
for (auto& vi : all_matches)
{
  std::cout << vi << std::endl;   // prints "every", "everywhere"
}

// Removes the longest found substrings
std::cout << Line.remove("every", "everywhere");  // prints "Stumbling ", not "Stumbling where"
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
