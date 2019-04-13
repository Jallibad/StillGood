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
    # create a module for the output
    l_module = ir.Module(name=__file__)

    curBlock = jast
    parents = []
    knownFuncs = ["print"]
    funcs = []
    # traverse the AST matching functions to their corresponding body contents
    """
    expression:
    Will contain a function.
    If it is built in, will have the tag "BuiltIn" and its "contents"
    Otherwise, will contain 3 fields: "function", "tag", "body"
    
    function:
    Will contain 2 fields: "annotation", "expression"
    
    body:
    Will contain 2 fields: "annotation", "expression"
    
    "Arrow" tag:
    Defines an input and output
    
    "Constructor" tag:
    Defines a type
    
    "Application" tag:
    Defines a function
    
    "BuiltIn" tag:
    Defines a function literal
    
    Perhaps the best way to go is look at the tag first, then decide what to do next
    
    """
    try:
        if (curBlock.get("Right")):
            curBlock = curBlock["Right"]["expression"]
            while(True):
                if (curBlock.get("function")):
                    parents.append(curBlock)
                    curBlock = curBlock["function"]["expression"]
                    if (curBlock["tag"] == "BuiltIn"):
                        if (curBlock["contents"] in knownFuncs):
                            funcs.append([curBlock["contents"]])
                else:
                    curBlock = parents.pop()
                    curBlock = curBlock["body"]["expression"]
                    if (curBlock["tag"] == "BuiltIn"):
                        funcs[-1].append(curBlock["contents"])
        else: #error occurred
            pass
    except:
        print("finished parsing AST. discovered code:",funcs)
    # define llvm types
    l_int = ir.IntType(32)  # TODO: replace hard-coded int with a type extracted from the AST, once type info is merged in
    l_funcType = ir.FunctionType(l_int, [])
    #l_funcType = ir.FunctionType(l_int, [*([l_int]*len(funcArgs))]) # match number of function arguments
    # declare our new function
    funcName = "main"
    l_func = ir.Function(l_module, l_funcType, name=funcName)
    
    # function entry point
    block = l_func.append_basic_block(name="entry")
    # create a builder for constructing the function code
    builder = ir.IRBuilder(block)
    
    #add printing support if our code uses it anywhere
    if ("print" == f[0] for f in funcs):
        # Source: https://blog.usejournal.com/writing-your-own-programming-language-and-compiler-with-python-a468970ae6df
        voidptr_ty = ir.IntType(8).as_pointer()
        fmt = "%i \n\0"
        c_fmt = ir.Constant(ir.ArrayType(ir.IntType(8), len(fmt)), bytearray(fmt.encode("utf8")))
        global_fmt = ir.GlobalVariable(l_module, c_fmt.type, name="fstr")
        global_fmt.linkage = 'internal'
        global_fmt.global_constant = True
        global_fmt.initializer = c_fmt
        fmt_arg = builder.bitcast(global_fmt, voidptr_ty)
        printf_ty = ir.FunctionType(ir.IntType(32), [voidptr_ty], var_arg=True)
        printf = ir.Function(l_module, printf_ty, name="printf")
        
    # now add the code from our ast
    for f in funcs:
        if (f[0] == "print"):
            if (getTypeFromStr(f[1]) == "int"):
                builder.call(printf,[fmt_arg,ir.Constant(ir.IntType(32), int(f[1]))])
            else:
                #TODO: printing non-int primitives
                pass

    # return 0
    builder.ret(l_int(0))
    
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
        print("Extracting platform architecture from clang")
        try:
            properTarget = subprocess.check_output("clang --version".format(sys.argv[1]), shell=True).decode("utf-8").split("\n")[1][8:]
        except:
            exitError("Unable to extract platform architecture from clang. Please make sure you have clang installed properly")
        modOut = modOut[:tripleLocQ1+1] + properTarget + modOut[tripleLocQ2:]
        print("Writing llvm code to " + sys.argv[2]+".ll")
        with open(sys.argv[2]+".ll","w") as f:
            f.write(modOut)
        
        # now compile the .ll to a .obj with llc
        print("Compiling llvm code to " + sys.argv[2] + ".obj")
        try:
            subprocess.run("llc -filetype=obj " + sys.argv[2] + ".ll")
        except:
            exitError("Unable to compile llvm code with llc. Please make sure you have llvm installed properly")
        # finally, compile the .obj to an executable with clang
        print("Compiling obj file to " + sys.argv[2] + ".exe")
        try:
            subprocess.run("clang " + sys.argv[2] + ".obj -o " + sys.argv[2] + ".exe")
        except:
            exitError("Unable to create executable file with clang. Please make sure you have clang installed properly")
        print("Compilation complete! Final executable written to " + sys.argv[2] + ".exe")
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
    