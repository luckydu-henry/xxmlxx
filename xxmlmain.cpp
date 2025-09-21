// Quick demo that reads in a xml document to document_tree and convert this tree to string immediately.
#include <iostream>
#include <fstream>
#include <chrono>

#define BENCHMARK_XXMLXX

#ifdef BENCHMARK_XXMLXX
#include "xxmlxx.hpp"
#elif defined BENCHMARK_TINYXML2
#include "tinyxml2.h"
#endif

#include <memory_resource>

#include "xxmlxx.hpp"

int main(int argc, char** argv) {

#ifdef BENCHMARK_XXMLXX
    std::ifstream xml_file("UnityEditor.csproj");
    std::string   xml_content{std::istreambuf_iterator<char>(xml_file),std::istreambuf_iterator<char>()};


    std::pmr::unsynchronized_pool_resource pool{
        std::pmr::pool_options{.max_blocks_per_chunk = 8192, .largest_required_pool_block = 8192 }
    };
    
    xxmlxx::document_parser parser(xml_content);
    xxmlxx::document_tree<xxmlxx::tree_node_capacity_index::standard,
    std::pmr::polymorphic_allocator<xxmlxx::tree_node<xxmlxx::tree_node_capacity_index::standard>>> tree("", 65536, &pool);

    std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();
    auto result = parser.to_tree(tree);
    
    std::chrono::high_resolution_clock::time_point middle = std::chrono::high_resolution_clock::now();
    
    if (!result) {
        std::cout << result.error();
    }
    else {
        std::string str;
        tree.format_to(std::back_inserter(str), 1 << 24,
            std::pmr::polymorphic_allocator<char>(&pool),
            std::pmr::polymorphic_allocator<std::basic_string<char, std::char_traits<char>, std::pmr::polymorphic_allocator<char>>>(&pool));
        
        std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
    
        std::chrono::duration<double> read_time = middle - begin;
        std::chrono::duration<double> write_time = end - middle;
    
        std::cout << str;
        std::cout << std::format("\n\n-------------------------------------------------------------------------------------------------------------------------\n"
            "Tree from string time: {}\n Tree to string time: {}\n", read_time, write_time);
    }
    
#elif defined BENCHMARK_TINYXML2
    tinyxml2::XMLDocument doc;
    std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();
    auto error = doc.LoadFile("UnityEditor.csproj");
    std::chrono::high_resolution_clock::time_point middle = std::chrono::high_resolution_clock::now();
    if (error != tinyxml2::XML_SUCCESS) {
        std::cout << "XML load failed!\n";
    }
    else {
        doc.SaveFile("UnityEditorOut.csproj");
        std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();

        std::chrono::duration<double> read_time = middle - begin;
        std::chrono::duration<double> write_time = end - middle;

        std::cout << std::format("\n\n-------------------------------------------------------------------------------------------------------------------------\n"
            "Tree from file time: {}\n Tree to file time: {}\n", read_time, write_time);
    }
#endif

    

    
}