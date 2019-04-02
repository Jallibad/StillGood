module Main where

import AST.Parsable (parser)
import AST.Types (Expression)
import Data.Aeson
import Data.ByteString.Lazy (ByteString, writeFile)
import qualified Data.ByteString.Lazy.Char8 as BS
import ParseErrorBundleJSON ()
import Options.Applicative
import Text.Megaparsec

main :: IO ()
main = compile =<< execParser opts
	where
		opts = info (options <**> helper)
			(fullDesc
			<> header "Compiler for StillGood.")

compile :: Opts -> IO()
compile (Opts inFile outFile) = case outFile of
		Nothing -> readFile inFile >>= BS.putStrLn . sourceToJSON inFile
		Just outfile -> readFile inFile >>= Data.ByteString.Lazy.writeFile outfile . sourceToJSON inFile  -- if outfile, then writeFile

-- |@sourceToJSON source filename@ converts a source file to a JSON encoded ByteString

sourceToJSON :: String -> String -> ByteString
sourceToJSON = (either encode encode .) . runParser (parser @Expression)

data SGParser = SGParser {
	optInput :: String
}

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
	<*> (optional $ strOption
		( long "output"
		<> short 'o'
		<> metavar "filename"
		-- <> value "out" -- this makes the output file not optional
		<> help "Write output to filename" ))
