# StillGood  
  
StillGood is a new programming language, designed to capitalize on the strengths of both functional and object oriented programming paradigms.  
  
## Requirements  
-Python 3.6+ (https://www.python.org/downloads/)  
-Haskell (https://docs.haskellstack.org/en/stable/README/)  
  
## Install Instructions  
-run "pip install llvmlite"  
-run "git clone git://github.com/Jallibad/StillGood.git"  
  
## Usage  
-Navigate to the project's root directory  
-Ensure that you have checked out and updated all submodules by running "git submodule update --recursive --remote"  
-Run "stack build" to build the Haskell portion of the codebase  
-Run "stack exec StillGood filename" where 'filename' is your test program. Alternatively, you can run stack ghci to work with GHC's interactive environment.  
-Run "python StillGood.py inputCodeFile|inputJSONFile [outputFile]" to compile the specified code file or pre-compiled AST JSON file to the specified output file, or to run the compiled code in Python if no output file was provided  
  
For experimenting with llvm ir, check out http://ellcc.org/demo/index.cgi  
  
## Contributing  
  
StillGood is still in its infancy, and is depending on the community's help to grow into a fully fledged programming language.  
We encourage you to [submit any issues you encounter](https://github.com/Jallibad/StillGood/issues/new) and contribute to StillGood!  
To contribute, [fork the repo](https://github.com/Jallibad/StillGood/fork), comment on an issue, and submit a pull request to the [master](https://github.com/Jallibad/StillGood/tree/master) branch.  
For help getting started with contributing, check the usage steps outlined above, or reach out to us with your questions or concerns.  
  
## Code of Conduct  
  
In the interest of fostering an open and welcoming environment, StillGood pledges to be an inclusive and harassment-free experience for  all, regardless of age, body size, disability, ethnicity, gender identity and expression, level of experience, educational background, socio-economic status, nationality, personal appearance, race, religion, or sexual identity and orientation.  
  
To this end, the StillGood community adheres to the [RCOS Code of Conduct](CODE_OF_CONDUCT.md).  
It is vital that all contributors read and respect the Code of Conduct.  
  
## License  
  
StillGood is and always will be Free and Open Source software, and is released under the [Apache License 2.0](LICENSE.md).  