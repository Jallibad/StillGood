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

Requirements:  
-Python 3.6+ (https://www.python.org/downloads/)  
-Haskell (https://docs.haskellstack.org/en/stable/README/)  
  
Install Instructions:  
run "pip install llvmlite"  
-run "git clone git://github.com/Jallibad/StillGood.git"  
  
Usage:  
-Navigate to the project's root directory  
<<<<<<< HEAD
-Ensure that you have checked out and updated all submodules by running "git submodule update --recursive --remote"  
-Run "stack build" to build the Haskell portion of the codebase  
-Run stack exec StillGood filename where 'filename' is your test program. Alternatively, you can run stack ghci to work with GHC's interactive environment.  
-Run "python StillGood.py inputCodeFile|inputJSONFile" to compile the specified code file or pre-compiled AST JSON file to LLVM
