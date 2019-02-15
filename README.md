# StillGood  
  
Install Instructions:  
-To install Haskell, see https://docs.haskellstack.org/en/stable/README/  
-To install C++ through Cygwin, see https://cygwin.com/install.html  
-To setup the project, run "git clone --recurse-submodules git://github.com/Jallibad/StillGood.git"  
  
Usage:  
-Navigate to the project's root directory  
-Ensure that you have checked out and updated all submodules by running "git submodule update --recursive --remote"  
-Run "stack build" to build the Haskell portion of the codebase  
-Run "gcc -o StillGood.out StillGood.cpp" to build the C++ portion of the codebase  
-Run "./StillGood.out codeFileName" to have the specified code file converted to an AST and then compiled to LLVM