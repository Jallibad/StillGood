#include <iostream>
#include <sstream>
#include <stdexcept>
#include <stdio.h>
#include <string>

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
		throw std::runtime_error("Incorrect number of args specified. Usage: StillGood.cpp inputCodeFile");
	}
	//run the specified code file through Haskell to get an AST that we can work with
	std::string ast = getCmdString("stack exec -- StillGood " + std::string(argv[1]));
	std::cout << ast;
	return 0;
}
