// Quick demo that reads in a xml document to document_tree and convert this tree to string immediately.
#include <iostream>
#include "xxmlxx.hpp"


int main(int argc, char** argv) {
    std::string xml_content = R"(
<?xml version="1.0" encoding="utf-8"?>

<Myself Name="Henry Du">


  <Gender>Man!</Gender>

  <Local Country="China" City = "Peking!"/>
  <GithubPage>https://github.com/luckydu-henry/</GithubPage>
  <Emails>


            <Google>wotsukoroga94@gmail.com</Google>
            <QQ>dududu_721@qq.com</QQ>
    <Outlook>xidhyu@outlook.com</Outlook>


    </Emails>

</Myself>)";

    xxmlxx::document_parser parser(xml_content);
    xxmlxx::document_tree   tree("");
    auto result = parser.to_tree(tree);

    if (!result) {
        std::cout << result.error();
    } else {

        std::cout << tree.to_string();
    }

}