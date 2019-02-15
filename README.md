# StillGood  
  
Install Instructions:  
-To install Haskell, see https://docs.haskellstack.org/en/stable/README/  
-To install Cygwin for C++, see https://cygwin.com/install.html  
  
Usage:  
-Navigate to the project's root directory  
-Run "stack build" to build the Haskell portion of the codebase  
-Run "gcc -o StillGood.out StillGood.cpp" to build the C++ portion of the codebase  
-Run "./StillGood.out codeFileName" to have the specified code file convert to an AST and then compile to LLVM