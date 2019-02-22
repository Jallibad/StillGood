import sys, json, subprocess

def getASTFromHaskell():
    return subprocess.check_output("stack exec -- StillGood {0}".format(sys.argv[1]), shell=True)

def getASTFromFile():
    return open(sys.argv[1]).read()

def exitError(s):
    print("Error: {0}".format(s).format(len(sys.argv)-1), file=sys.stderr)
    sys.exit(1)

def main():
    if (len(sys.argv) != 2):
        exitError("1 command line argument expected, {0} received. Usage: StillGood.cpp inputCodeFile|inputJSONFile")
    #if we specified a StillGood file, run it through Haskell to get the AST. Otherwise, read the AST directly from the file
    ast = getASTFromHaskell() if sys.argv[1][-3:] == ".sg" else getASTFromFile()
    jast = json.loads(ast)
    print(ast,"\n",jast)
    

if __name__ == "__main__":
    main()
    