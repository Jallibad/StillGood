# StillGood  
  
Requirements:  
-Python 3.6+ (https://www.python.org/downloads/)  
-Haskell (https://docs.haskellstack.org/en/stable/README/)  
  
Install Instructions:  
run "pip install llvmlite"  
-run "git clone git://github.com/Jallibad/StillGood.git"  
  
Usage:  
-Navigate to the project's root directory  
-Run "stack build" to build the Haskell portion of the codebase  
-Run stack exec StillGood filename where 'filename' is your test program. Alternatively, you can run stack ghci to work with GHC's interactive environment.  
-Run "python StillGood.py inputCodeFile|inputJSONFile" to compile the specified code file or pre-compiled AST JSON file to LLVM