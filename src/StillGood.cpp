#include <iostream>
#include <stdexcept>
#include <stdio.h>
#include <fstream>
#include <string>
#include <algorithm>
#include "../json/single_include/nlohmann/json.hpp"
using json = nlohmann::json;

/**
 * execute a system command, returning any data written to stdout in the process
 * @param cmd: the full system command to execute
 * @returns: a string containing the any data written to stdout while executing cmd
 * @credit: https://stackoverflow.com/a/478960
 */
std::string getCmdString(std::string cmd) {
    char buffer[128];
    std::string result = "";
    FILE* pipe = popen(cmd.c_str(), "r");
    if (!pipe) throw std::runtime_error("popen() failed!");
    try {
        while (fgets(buffer, sizeof buffer, pipe) != NULL)
            result += buffer;
    }
    catch (...) {
        pclose(pipe);
        throw;
    }
    pclose(pipe);
    return result;
}

int main(int argc, char* argv[]) {
	//cmd args check
	if (argc != 2) {
		throw std::runtime_error("Incorrect number of args specified. Usage: StillGood.cpp inputCodeFile|inputJSONFile");
	}
	std::string ast;
	if (strcmp(argv[1]+std::max(0,(int)strlen(argv[1])-3), ".sg") == 0) {
		//run the specified code file through Haskell to get an AST that we can work with
		ast = getCmdString("stack exec -- StillGood " + std::string(argv[1]));
	}
	else {
		//read the pre-compiled JSON data from the specified JSON file
		std::ifstream ifs(argv[1]);
		ast = std::string((std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>()));
	}
	std::cout << "ast: " << ast << std::endl;
	//parse the AST as JSON
	json jast = json::parse(ast);
	std::cout << "json: " << jast.dump() << std::endl;
	//convert the JSON to LLVM

	return 0;
}
