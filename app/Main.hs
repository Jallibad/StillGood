module Main where

import AST.Parsable (parser)
import qualified AST.Parsable as AST (Parser)
import AST.Expression (Expression)
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString, writeFile)
import Data.ByteString.Lazy.Char8 (putStrLn)
import HindleyMilner (addType)
import Options.Applicative
import ParseErrorBundleJSON ()
import Prelude hiding (putStrLn, writeFile)
import Text.Megaparsec

main :: IO ()
main = compile =<< execParser opts
	where
		opts = info (options <**> helper)
			(fullDesc
			<> header "Compiler for StillGood.")

compile :: Opts -> IO ()
compile (Opts inFile outFile) = readFile inFile >>= maybe putStrLn writeFile outFile . sourceToJSON inFile

-- |@sourceToJSON source filename@ converts a source file to a JSON encoded ByteString
sourceToJSON :: String -> String -> ByteString
sourceToJSON = (either encode (encode . addType mempty) .) . runParser (parser :: AST.Parser Expression)

data Opts = Opts
	{ optInput :: String
	, optOutput :: Maybe String }

-- commands that can be run currently:
-- stack exec StillGood test.txt
-- stack exec StillGood test.txt -- -o out
-- stack exec StillGood -- --help
options :: Parser Opts
options = Opts
	<$> argument str
		(metavar "filename"
		<> help "Program file" )
	<*> optional (strOption
		( long "output"
		<> short 'o'
		<> metavar "filename"
		-- <> value "out" -- this makes the output file not optional
		<> help "Write output to filename" ))
