module Main where

import Control.Monad (void)
import Control.Applicative (liftA2)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HM (fromList)
import Data.Text (pack)
import Data.Void (Void)
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = getArgs >>= \case
		[filename] -> do
			contents <- readFile filename
			BS.putStrLn $ either (BS.pack . errorBundlePretty) encode $ runParser expression filename contents
		_anythingElse -> putStrLn "Usage: ./StillGood filename"

-- TODO Refine
type Identifier = String

identifierToJSON :: Identifier -> Value
identifierToJSON = String . pack

data Expression = Variable Identifier
				| Lambda Identifier Expression
				| Application Expression Expression
				| BuiltIn String
				deriving (Show)

-- TODO: replace with deriving Generic
instance ToJSON Expression where
	toJSON = Object . HM.fromList . \case
		(Variable x)			-> [("etype", String "variable"), ("identifier", identifierToJSON x)]
		(Lambda i e)			-> [("etype", String "lambda"), ("identifier", identifierToJSON i), ("expression", toJSON e)]
		(Application func arg)	-> [("etype", String "application"), ("func", toJSON func), ("arg", toJSON arg)]
		(BuiltIn t)				-> [("etype", String "builtin"), ("text", String $ pack t)]

type Parser = Parsec Void String

-- |The space consumer combinator consumes at least one character of whitespace, or a comment (TODO)
sc :: Parser ()
sc = space1

-- |The consume trailing space combinator modifies a parser to consume any potential trailing whitespace
cts :: Parser a -> Parser a
cts = (<* optional sc)

-- |Parses any valid Identifier [A-Za-z][A-Za-z]*
identifier :: Parser Identifier
identifier = cts $ liftA2 (:) letterChar $ many alphaNumChar

-- |Modifies a parser to surround the contents with parentheses
parens :: Parser a -> Parser a
parens = between (cts $ single '(') (cts $ single ')')

variable :: Parser Expression
variable = cts $ Variable <$> identifier

lambdaDeclarator :: Parser ()
lambdaDeclarator = void $ cts $ single '\\'

lambdaArgSeparator :: Parser ()
lambdaArgSeparator = void $ cts $ chunk "->"

lambda :: Parser Expression
lambda = cts $ do
	lambdaDeclarator
	var <- identifier
	lambdaArgSeparator
	Lambda var <$> expression

application :: Parser Expression
application = cts $ Application <$> expression <* sc <*> expression

builtIn :: Parser Expression
builtIn = BuiltIn <$> (some digitChar <|> chunk "+" <|> chunk "-")

expression :: Parser Expression
expression = foldl1 Application <$> some term
	where term =
			parens expression <|>
			lambda <|>
			variable