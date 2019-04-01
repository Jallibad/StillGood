import sys, json, subprocess
from llvmlite import ir
from unittest.test.testmock.testpatch import function

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

def astToLLVM(jast):
    """
    Convert the input json encoded AST to LLVM code properly, using llvm-lite
    JSON jast: the AST in JSON form produced by the main Haskell routine
    Returns an ir module containing the LLVM code matching the input json encoded AST
    """
    #json indexing
    function = jast["function"]
    body = jast["body"] 
    funcBody = function["body"]
    
    #extract function information by keyword
    funcArgs = function["argument"].split(",")
    funcName = funcBody["identifier"]
    funcContents = body["contents"]
    
    # define llvm types
    l_int = ir.IntType(32)  # TODO: replace hard-coded int with a type extracted from the AST, once type info is merged in
    l_funcType = ir.FunctionType(l_int, [*([l_int]*len(funcArgs))])
    
    # create a module for the output
    l_module = ir.Module(name=__file__)
    # declare our new function
    l_func = ir.Function(l_module, l_funcType, name=funcName)

    # function entry point
    block = l_func.append_basic_block(name="entry")
    # create a builder for constructing the function code
    builder = ir.IRBuilder(block)
    # return value
    
    retVal = builder.fadd(l_func.args[0],l_int(funcContents),"retVal")
    builder.ret(retVal)
    
    # Print the module IR
    return l_module

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
    print(astToLLVM(jast))

if __name__ == "__main__":
    main()
    