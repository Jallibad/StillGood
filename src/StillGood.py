import sys, json, subprocess
from llvmlite import ir
import llvmlite.binding as llvm
from unittest.test.testmock.testpatch import function

from ctypes import CFUNCTYPE, c_double, c_int32

llvm.initialize()
llvm.initialize_native_target()
llvm.initialize_native_asmprinter()

def exitError(s):
    """
    Display the specified error message and exit the application
    string s: the error string to display on exit
    """
    print("Error: {0}".format(s).format(len(sys.argv)-1), file=sys.stderr)
    sys.exit(1)

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

def getTypeFromStr(s):
    """
    determine the type of the input string
    string s: the string whose type we wish to get
    Returns the type of the input string
    """
    try: 
        int(s)
        return "int"
    except ValueError:
        return "string"

def naiveAstToLLVM(jast):
    """
    Convert the input json encoded AST to LLVM code, using a naive method for testing purposes
    JSON jast: the AST in JSON form produced by the main Haskell routine
    Returns a string containing the LLVM code matching the input json encoded AST
    """
    # json indexing
    function = jast["function"]
    body = jast["body"] 
    funcBody = function["body"]
    
    # extract function information by keyword
    funcArg = function["argument"]
    funcName = funcBody["identifier"]
    funcContents = body["contents"]
    print("full jast: {0}\nargument: {1}, name: {2}, contents: {3}".format(jast,funcArg,funcName,funcContents))
    
    # build up the llvm code
    return "{0} {1}({2}) {{\nreturn {3};\n}}".format("int",funcName,funcArg,funcContents)

def astToLLVM(jast):
    """
    Convert the input json encoded AST to LLVM code properly, using llvm-lite
    JSON jast: the AST in JSON form produced by the main Haskell routine
    Returns the new function name, and an ir module containing the LLVM code matching the input json encoded AST
    """
    # json indexing
    function = jast["function"]
    body = jast["body"] 
    funcBody = function["body"]
    
    # extract function information by keyword
    funcArgs = function["argument"].split(",")
    # TODO: hard-code function name to "main" temporarily so we can call StillGood methods directly during testing
    funcName = "main" #funcBody["identifier"]
    funcContents = body["contents"]
    
    # define llvm types
    l_int = ir.IntType(32)  # TODO: replace hard-coded int with a type extracted from the AST, once type info is merged in
    l_funcType = ir.FunctionType(l_int, [*([l_int]*len(funcArgs))]) # match number of function arguments
    
    # create a module for the output
    l_module = ir.Module(name=__file__)
    # declare our new function
    l_func = ir.Function(l_module, l_funcType, name=funcName)

    # function entry point
    block = l_func.append_basic_block(name="entry")
    # create a builder for constructing the function code
    builder = ir.IRBuilder(block)
    # return value
    contentType = getTypeFromStr(funcContents)
    if (contentType == "int"):
        builder.ret(l_int(funcContents))
    
    # Print the module IR
    return funcName, l_module

def create_execution_engine():
    """
    Create an ExecutionEngine suitable for JIT code generation on
    the host CPU.  The engine is reusable for an arbitrary number of modules.
    Source: https://llvmlite.readthedocs.io/en/latest/user-guide/binding/examples.html
    """
    # Create a target machine representing the host
    target = llvm.Target.from_default_triple()
    target_machine = target.create_target_machine()
    # And an execution engine with an empty backing module
    backing_mod = llvm.parse_assembly("")
    engine = llvm.create_mcjit_compiler(backing_mod, target_machine)
    return engine
    
def compile_ir(engine, llvm_ir):
    """
    Compile the LLVM IR string with the given engine.
    The compiled module object is returned.
    Source: https://llvmlite.readthedocs.io/en/latest/user-guide/binding/examples.html
    """
    # Create a LLVM module object from the IR
    mod = llvm.parse_assembly(llvm_ir)
    mod.verify()
    # Now add the module and make sure it is ready for execution
    engine.add_module(mod)
    engine.finalize_object()
    engine.run_static_constructors()
    return mod

def compile_module(engine, mod):
    """
    Compile the LLVM IR module with the given engine.
    The compiled module object is returned.
    """
    return compile_ir(engine,str(mod))

def main():
    if (not len(sys.argv) in [2,3]):
        exitError("1 command line argument expected, {0} received. Usage: python StillGood.py inputCodeFile|inputJSONFile [outputFile]")
    # if we specified a StillGood file, run it through Haskell to get the AST. Otherwise, read the AST directly from the file
    ast = getASTFromHaskell() if sys.argv[1][-3:] == ".sg" else getASTFromFile()
    jast = json.loads(ast)
    # now convert the ast to llvm code
    funcName, llvm_ir = astToLLVM(jast)
    
    engine = create_execution_engine()
    mod = compile_module(engine, llvm_ir)
    
    if (len(sys.argv) == 3):
        # if the user specified an output file name, write the generated llvm code to that file
        # manually add target to .ll output so we can compile it to a proper executable
        modOut = str(mod)
        tripleLocQ1 = modOut.find("target triple = ")+16
        tripleLocQ2 = modOut.find('"',tripleLocQ1+1)
        # extract our platform architecture from clang's version info
        properTarget = subprocess.check_output("clang --version".format(sys.argv[1]), shell=True).decode("utf-8").split("\n")[1][8:]
        modOut = modOut[:tripleLocQ1+1] + properTarget + modOut[tripleLocQ2:]
        with open(sys.argv[2]+".ll","w") as f:
            f.write(modOut)
        
        # now compile the .ll to a .obj with llc
        subprocess.run("llc -filetype=obj " + sys.argv[2] + ".ll")
        # finally, compile the .obj to an executable with clang
        subprocess.run("clang " + sys.argv[2] + ".obj -o " + sys.argv[2] + ".exe")
    else:
        # no output file was specified, so run the generated code directly in Python via ctypes instead
        # Look up the function pointer (a Python int) 
        func_ptr = engine.get_function_address(funcName)
        
        # Run the function and test the output
        cfunc = CFUNCTYPE(c_int32, c_int32)(func_ptr)
        testArg = 8
        res = cfunc(testArg)
        print("{0}({1}) = {2}".format(funcName,testArg, res))

if __name__ == "__main__":
    main()
    