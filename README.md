# StillGood  
  
Install Instructions:  
-To install Haskel, see https://docs.haskellstack.org/en/stable/README/  
-To install Cygwin for C++, see https://cygwin.com/install.html  
  
Usage:  
-Navigate to the project's root directory  
-Run "stack build" to build the Haskel portion  
-Run "stack exec -- StillGood inputFileName" to generate the AST for a single plaintext input file  
  
Upcoming:  
-Genearting LLVM code from the AST provided by Haskel