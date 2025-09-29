# xxmlxx
An **extremely light and efficient XML library** (probably the smallest one ever had) containing almost complete XML IO and handling features.

# Use it as a library
Include `xxmlxx.hpp` to your project path, **the only thing you need to have is a C++20 compatible compiler**.
This demo **runs successfully on Windows(MSVC17) and Linux(Ubuntu-24.04 g++14, clang++-19)**, so it's definitely platform independent.

# Features
1. Single header, very portable.
2. A special strucutre *"Forward Vector Tree"* to access XML nodes in a cache-friendly way.
3. A special output algorithm which uses *"Dense Vector"* to convert tree data in to string document.
4. A document parser that combines C++ iterator and *Parser Combinator* techniques and an input algorithm that uses *Stack* to avoid traditional recursive.
5. Basic XML-1.0 declration tag, element tag, comment node, and self-closing tag completely support (satisfies all your "daily" (de)
serialization or transfer needs).
6. Direct iterator API to operate a DOM tree easily (v2 feature).
7. Fast node searching using a combination of binary search and linear search (v2 feature).
8. Customizable allocator (v2 feature). 

# Overview (v2 updated)

This little demo shows how to do simple xml layout:

```c++
    using tnc = xxmlxx::tree_node_category;
    xxmlxx::document_tree<> tree(R"(Hi Project="Test Project")");
    // Begin is always the root node.
    tree.begin()
        // Insert a child node on this node.
        .insert(tnc::element, R"(Hi0 Message="Hello world!")").rise() // Rise to parent.
        .insert(tnc::element, R"(Hi1 Message="Hello again!")")
            .insert(tnc::element, R"(My)")
                .insert(tnc::element, R"(My)")
                    .insert(tnc::text, R"(Hello there!)");
    tree.begin()
        .insert(tnc::element, R"(Hi2 Message="Hello again again!")").rise()
            .insert(tnc::element, R"(MyName)");

    // Dive to first child node.
    tree.begin().dive().attribute("Message", "Fixed message hello world!"); // Set attribute.
    std::cout << tree.to_string() << std::flush;
```

And it outputs:

```xml
<?xml version="1.0" encoding="utf-8"?>
<Hi Project="Test Project">
  <Hi0 Message="Fixed message hello world!"/>
  <Hi1 Message="Hello again!">
    <My>
      <My>Hello there!</My>
    </My>
  </Hi1>
  <Hi2 Message="Hello again again!"/>
  <MyName/>
</Hi>
```

What's more, **there are now three benchmark tests** for you to sense the IO performance of xxmlxx directly.

Here are results on my laptop *(11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz (8 CPUs), ~1.8GHz; 16384MB RAM; Windows 11; Visual Studio 17.14.11; Favor Speed Optimization)*:

```txt
(All durations are averaged)

Using benchmark file: UnityEditor.csproj (~10000 lines)
Tree from string time: 0.0741632s
Tree to string time: 0.081706s

Using benchmark file: UnityEngine.csproj (~6000 lines)
Tree from string time: 0.0184032s
Tree to string time: 0.0218365s

Using benchmark file: doomexe.vcxproj (~600 lines)
Tree from string time: 0.0006135s
Tree to string time: 0.0042135s
```

**Even the largest file can reach "microsecond" level IO** so you don't have to worry about its IO performance (you can't even sense it)

**In large file and middle sized file scenario**, xxmlxx's IO time cost **is slower than** TinyXML2's so **if you have higher IO speed requirement in large files you may want to use TinyXML2**.

But **in small sized file** especially **when there are lot of child node linked under a single parent node** xxmlxx's **input performance is much more better** than TinyXML2's. So **xxmlxx would be a good choice when you are dealing with small and not-complex xmls** (configuration or serialized data).

You can run `xxmldemo.cpp` to download test files and run your own benchmarks.

# Limitations
1. Not support CDATA.
2. Not support Entity and XML specific escape characters.
3. Haven't done UTF-8 testing.
4. Any other high-level features.

# Anyway.
Since **there are only ~1000 lines of codes**, and the core concepts of this library is fairly simple, you can modify it and add your own features freely. I mean this library uses **MIT license** so feel free to hack.

But, I still hope you can send me issues or bug reports or pull requests whatever to make this library better.