import sys, json, subprocess

def getASTFromHaskell():
     """
     Run the main Haskell routine on the input code file name in a subprocess, yielding the resulting AST
     Returns the bytestring representation of a json encoded Abstract Syntax Tree
     """
     return subprocess.check_output("stack exec -- StillGood {0}".format(sys.argv[1]), shell=True)

def getASTFromFile():
    """
     Read the contents of the input file name, storing the contained AST
     Returns the string representation of a json encoded Abstract Syntax Tree
     """
    return open(sys.argv[1]).read()


def naiveAstToLLVM(jast):
    """
    Convert the input json encoded AST to LLVM code, using a naive method for testing purposes
    JSON jast: the AST in JSON form produced by the main Haskell routine
    Returns a string containing the LLVM code matching the input json encoded AST
    """
    #json indexing
    function = jast["function"]
    body = jast["body"] 
    funcBody = function["body"]
    
    #extract function information by keyword
    funcArg = function["argument"]
    funcName = funcBody["identifier"]
    funcContents = body["contents"]
    print("full jast: {0}\nargument: {1}, name: {2}, contents: {3}".format(jast,funcArg,funcName,funcContents))
    
    #build up the llvm code
    return "{0} {1}({2}) {{\nreturn {3};\n}}".format("int",funcName,funcArg,funcContents)
    

def exitError(s):
    """
    Display the specified error message and exit the application
    string s: the error string to display on exit
    """
    print("Error: {0}".format(s).format(len(sys.argv)-1), file=sys.stderr)
    sys.exit(1)

def main():
    if (len(sys.argv) != 2):
        exitError("1 command line argument expected, {0} received. Usage: python StillGood.py inputCodeFile|inputJSONFile")
    # if we specified a StillGood file, run it through Haskell to get the AST. Otherwise, read the AST directly from the file
    ast = getASTFromHaskell() if sys.argv[1][-3:] == ".sg" else getASTFromFile()
    jast = json.loads(ast)
    # now convert the ast to llvm code
    print(naiveAstToLLVM(jast))

if __name__ == "__main__":
    main()
    