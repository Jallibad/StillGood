<!-- # StillGood

StillGood is a new programming language, designed to capitalize on the strengths of both functional and object oriented programming paradigms.

## Setup

(setup for developers, for now)

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repository.
3. In repository, run `stack build`.
4. Run `stack exec StillGood filename` where 'filename' is your test program.
5. Alternatively you can run `stack ghci` to work with GHC's interactive environment.
======= -->

# StillGood  
  
StillGood is a new programming language, designed to capitalize on the strengths of both functional and object oriented programming paradigms.

Install Instructions:  
-To install Haskell, see https://docs.haskellstack.org/en/stable/README/  
-To install C++ through Cygwin, see https://cygwin.com/install.html  
-To install LLVM, install one of the pre-built binaries or build from source http://releases.llvm.org/download.html or just use a snapshot build http://llvm.org/builds/
-Finally, to setup the project, run "git clone --recurse-submodules git://github.com/Jallibad/StillGood.git"  
  
Usage:  
-Navigate to the project's root directory  
-Ensure that you have checked out and updated all submodules by running "git submodule update --recursive --remote"  
-Run "stack build" to build the Haskell portion of the codebase  
-Run "gcc -o StillGood.out StillGood.cpp" to build the C++ portion of the codebase  
-Run "./StillGood.out codeFileName" to have the specified code file converted to an AST and then compiled to LLVM
