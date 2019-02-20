module AST.Parsable
	( Parsable
	, Parser
	, parseExpression
	, parser
	) where

import AST.Identifier
import AST.Types
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Foldable (foldr')
import Data.Void (Void)
import HindleyMilner.Type (Type)
import Text.Megaparsec
import Text.Megaparsec.Char

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

instance Parsable Type where
	parser = undefined

-- typeDeclarator :: Parser ()
-- typeDeclarator = void $ cts $ string "::"

-- explicitType :: Parser Expression
-- explicitType = ExplicitType <$> parser <* typeDeclarator <*> parser

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
		args :: Parser [Identifier]
		args = lambdaDeclarator *> some parser
		
		body :: Parser Expression
		body = lambdaBodySeparator *> parser
		
		-- |Takes a list of variable names and an expression, then eta-expands a Lambda wrapper for each one
		combineArgsAndBody :: Parser [Identifier] -> Parser Expression -> Parser Expression
		combineArgsAndBody = liftA2 $ flip $ foldr' Lambda

builtIn :: Parser Expression
builtIn = BuiltIn <$> (some digitChar <|> chunk "+" <|> chunk "-")

instance Parsable Expression where
	parser = label "expression" $ foldl1 Application <$> some term
		where term =
			parens parser <|>
			-- explicitType <|>
			lambda <|>
			builtIn <|>
			variable
	
parseExpression :: String -> Either (ParseErrorBundle String Void) Expression
parseExpression = runParser parser ""