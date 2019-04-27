# StillGood

## Introduction

#### What IS StillGood?
StillGood is a new programming language designed to capitalize on the strengths
of both functional and object-oriented programming paradigms. It is based on 
lambda calculus and features Haskell style function application.

StillGood features a Hindley-Milner type system. It is statically typed with
type inference and parametric polymorphism.

StillGood is built in both Haskell and Python. Haskell is used for parsing and
lexing via the Megaparsec library. Python is used for code generation and compilation
via LLVM.

Currently, basic functionality is working. A simple function will compile and execute.
Future work intends to build off of this and implement more features.

## Setting up your Environment

To use StillGood, you need Stack and LLVM, and Python
1. Download the install latest version of Python:
https://www.python.org/downloads/
2. Download and install the latest version of the Haskell stack
https://docs.haskellstack.org/en/stable/README/
3. Get the latest release of LLVM here:
http://releases.llvm.org/download.html#8.0.0
4. Follow the guide for building and installing LLVM
https://llvm.org/docs/CMake.html 
5. Navigate to the project's root directory
6. Ensure that you have checked out and updated all submodules by running 
"git submodule update --recursive --remote"
7. Run "pip install llvmlite" to get the Python LLVM package
8. Run "stack build" to build the Haskell portion of the codebase

All StillGood code files use the extention ".sg". When you're ready to compile,

-Run "python StillGood.py inputCodeFile|inputJSONFile [outputFile]" 
to compile the specified code file or pre-compiled AST JSON file to the specified output file, 
or to run the compiled code in Python if no output file was provided/

This will generate an executable which can be run from the command line.

## Syntax

(Rename this section) Show operators, formatting, line endings (or lack thereof)

## Sample Program

Todo