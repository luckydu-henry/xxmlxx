# xxmlxx
An **extremely light and efficient XML library** (probably the smallest one ever had) containing almost complete XML IO and handling features.

# Start
Include `xxmlxx.hpp` and compile `xxmldemo.cpp` for simple usages, **the only thing you need to have is a C++20 compatible compiler**.
This demo **runs successfully on Windows(MSVC17) and Linux(Ubuntu-24.04 g++14, clang++-19)**, so it's definitely platform independent.

# Features
1. Single header, very portable.
2. A special strucutre *"Forward Vector Tree"* implemented by me to access XML nodes in a cache-friendly way.
3. A special output algorithm which uses *"Dense Vector"* to convert tree data in to string document.
4. A document parser that combines C++ iterator and *Parser Combinator* techniques and an input algorithm that uses *Stack* to avoid traditional recursive.
5. Basic XML-1.0 declration tag, element tag, comment node, and self-closing tag completely support (satisfies all your "daily" (de)
serialization or transfer needs).

# Limitations
1. Not support CDATA.
2. Not support Entity and XML specific escape characters.
3. Haven't done UTF-8 testing.
4. Any other high-level features.

# Anyway.
Since **there are only ~850 lines of codes**, and the core concepts of this library is fairly simple, you can modify it and add your own features freely. I mean this library uses **MIT license** so feel free to hack.

But, I still hope you can send me issues or bug reports or pull requests whatever to make this library better.