// Quick demo that reads in a xml document to document_tree and convert this tree to string immediately.
#include <iostream>
#include <fstream>
#include <chrono>

#define BENCHMARK_XXMLXX

// #define BENCHMARK_TINYXML2
#ifdef BENCHMARK_XXMLXX
#include "xxmlxx.hpp"
#elif defined BENCHMARK_TINYXML2
#include "tinyxml2.h"
#endif

#include <memory_resource>

#include "xxmlxx.hpp"

int main(int argc, char** argv) {
#ifdef BENCHMARK_XXMLXX
    std::ifstream xml_file("doomexe.vcxproj");
    std::string   xml_content{std::istreambuf_iterator<char>(xml_file),std::istreambuf_iterator<char>()};
    std::ofstream xml_file_out("doomexeout.vcxproj");

    std::pmr::unsynchronized_pool_resource pool{
        std::pmr::pool_options{.max_blocks_per_chunk = 8192, .largest_required_pool_block = 8192 }
    };
    
    xxmlxx::document_parser parser(xml_content);
    xxmlxx::document_tree<xxmlxx::tree_node_capacity_level::big> tree("", 65536);

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

        std::cout << "Sorted tree structure: " << std::endl;
        
        for (auto& i : tree) {
            std::cout << std::format("index {:d} 's parent index is {:d}\n", &i - tree.begin(), i.parent_id()) << std::flush;
        }
        
        std::cout << std::format("\n\n-------------------------------------------------------------------------------------------------------------------------\n"
            "Tree from string time: {}\n Tree to string time: {}\n", read_time, write_time);
    }
    
#elif defined BENCHMARK_TINYXML2
    tinyxml2::XMLDocument doc;
    std::chrono::high_resolution_clock::time_point begin = std::chrono::high_resolution_clock::now();
    auto error = doc.LoadFile("doomexe.vcxproj");
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