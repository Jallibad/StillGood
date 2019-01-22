module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Parsable
import ParseErrorBundleJSON ()
import System.Environment (getArgs)
import Text.Megaparsec
import Types (Expression)

main :: IO ()
main = getArgs >>= \case
		[filename] -> readFile filename >>= BS.putStrLn . sourceToJSON filename
		_ -> putStrLn "Usage: ./StillGood filename"

-- |@sourceToJSON source filename@ converts a source file to a JSON encoded ByteString
sourceToJSON :: String -> String -> ByteString
sourceToJSON = (either encode encode .) . runParser (parser :: Parser Expression)