module Parsable
	( Parsable
	, Parser
	, parser
	) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Foldable (foldr')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Types

type Parser = Parsec Void String

-- |The space consumer combinator consumes at least one character of whitespace, or a comment (TODO)
sc :: Parser ()
sc = space1

-- |The consume trailing space combinator modifies a parser to consume any potential trailing whitespace
cts :: Parser a -> Parser a
cts = (<* optional sc)

class Parsable a where
	parser :: Parser a

instance Parsable Identifier where
	parser = label "identifier" $ cts $ fmap Identifier $ liftA2 (:) letterChar $ many alphaNumChar

-- |Modifies a parser to surround the contents with parentheses
parens :: Parser a -> Parser a
parens = between (cts $ single '(') (cts $ single ')')

variable :: Parser Expression
variable = cts $ Variable <$> parser

-- |A parser matching the declarator '\' at the start of a lambda
lambdaDeclarator :: Parser ()
lambdaDeclarator = void $ cts $ single '\\'

-- |A parser matching the separator "->" between the arguments of a lambda and the body
lambdaBodySeparator :: Parser ()
lambdaBodySeparator = label "lambda separator \"->\"" $ void $ cts $ chunk "->"

lambda :: Parser Expression
lambda = combineArgsAndBody args body
	where
		combineArgsAndBody :: Parser [Identifier] -> Parser Expression -> Parser Expression
		combineArgsAndBody = liftA2 (flip $ foldr' Lambda)
		
		args :: Parser [Identifier]
		args = lambdaDeclarator *> some parser

		body :: Parser Expression
		body = lambdaBodySeparator *> parser

builtIn :: Parser Expression
builtIn = BuiltIn <$> (some digitChar <|> chunk "+" <|> chunk "-")

instance Parsable Expression where
	parser = label "expression" $ foldl1 Application <$> some term
		where
			term =
				parens parser <|>
				lambda <|>
				builtIn <|>
				variable