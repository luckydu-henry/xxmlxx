//
// MIT License
//
// Copyright (c) 2025 Henry Du
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
//

#include <iostream>
#include <fstream>
#include <filesystem>
#include <chrono>

// Uncomment macros to do your own test

// #define TEST_IO_BENCHMARK
// #define TEST_NEW_LAYOUT_API

// Now supports custom allocator
#include <memory_resource>
#include "xxmlxx.hpp"

int main(int argc, char** argv) {
    
#ifdef TEST_NEW_LAYOUT_API
    using tnc = xxmlxx::tree_node_category;
    constexpr bool enable_erase = false;
    
    xxmlxx::document_tree<> tree(R"(Hi Project="Test Project")");
    // Insert a child node on current node.
    // Rise to parent node.  
    tree.begin()
        .insert(tnc::element, R"(Hi0 Message="Hello world!")").rise()
        .insert(tnc::element, R"(Hi1 Message="Hello again!")")
            .insert(tnc::element, R"(My)")
                .insert(tnc::element, R"(My)")
                    .insert(tnc::text, R"(Hello there!)");
    tree.begin()
        .insert(tnc::element, R"(Hi2 Message="Hello again again!")").rise()
            .insert(tnc::element, R"(MyName)");

    // Dive to first child node.
    tree.begin().dive().attribute("Message", "Fixed message hello world!");
    
    // After this the cout would only output root node.
    if constexpr (enable_erase) {
        // Erase current node with all its children.
        tree.begin().erase();
    }

    std::cout << tree.to_string() << std::endl;
#endif

    // Download test files (remember to change their coding to UTF-8 without BOM manually):
    // 0. UnityEditor.csproj (Large size)   : https://github.com/Unity-Technologies/UnityCsReference/blob/master/Projects/CSharp/UnityEditor.csproj
    // 1. UnityEngine.csproj (Middle size)  : https://github.com/Unity-Technologies/UnityCsReference/blob/master/Projects/CSharp/UnityEngine.csproj
    // 2. doomexe.vcxproj    (Small size)   : https://github.com/id-Software/DOOM-3-BFG/blob/master/neo/doomexe.vcxproj
#ifdef TEST_IO_BENCHMARK
    // Pick your own benchmark.
    constexpr const char* test_names[] = { "UnityEditor.csproj", "UnityEngine.csproj", "doomexe.vcxproj" };
    constexpr const char* dest_names[] = { "UnityEditor.out.csproj", "UnityEngine.out.csproj", "doomexe.out.vcxproj" };
    constexpr std::size_t test_index = 0;
    
    std::ifstream xml_file(test_names[test_index]);
    std::string   xml_content{std::istreambuf_iterator<char>(xml_file),std::istreambuf_iterator<char>()};
    std::ofstream xml_file_out(dest_names[test_index]);

    std::pmr::unsynchronized_pool_resource pool{
        std::pmr::pool_options{.max_blocks_per_chunk = 8192, .largest_required_pool_block = 8192 }
    };
    
    xxmlxx::document_parser parser(xml_content);
    xxmlxx::document_tree<std::pmr::polymorphic_allocator<char>> tree("", 1024, &pool);

    std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();
    auto result = parser.to_tree(tree);
    std::chrono::high_resolution_clock::time_point middle = std::chrono::high_resolution_clock::now();
    
    if (!result) {
        std::cout << result.error();
    }
    else {
        tree.format_to(std::ostreambuf_iterator<char>(xml_file_out));
        std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
    
        std::chrono::duration<double> read_time = middle - begin;
        std::chrono::duration<double> write_time = end - middle;

        std::cout << std::format("\n\n-------------------------------------------------------------------------------------------------------------------------\n"
            "Tree from string time: {}\nTree to string time: {}\n", read_time, write_time);
    }
#endif
}