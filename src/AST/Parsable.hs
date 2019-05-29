module AST.Parsable
	( Parsable
	, Parser
	, ParserT
	-- , parseExpression
	-- , parseTypes
	, cts
	, parser
	) where

import AST.Expression
import AST.Identifier
import AST.Precedence
-- import HindleyMilner.Type (Type)
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Foldable (foldr')
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = ParserT Identity
type ParserT = ParsecT Void String

-- |The space consumer combinator consumes at least one character of whitespace, or a comment
sc :: ParserT m ()
sc = L.space space1 lineCmnt blockCmnt where
	lineCmnt = L.skipLineComment "--"
  	blockCmnt = L.skipBlockComment "/*" "*/"

-- |The consume trailing space combinator modifies a parser to consume any potential trailing whitespace
cts :: ParserT m a -> ParserT m a
cts = (<* optional sc)

class Parsable a where
	parser :: ParserT m a

instance Parsable Identifier where
	parser = label "identifier" $ cts $ fmap Identifier $ liftA2 (:) letterChar $ many alphaNumChar

-- |Modifies a parser to surround the contents with parentheses
parens :: ParserT m a -> ParserT m a
parens = between (cts $ single '(') (cts $ single ')')

variable :: ParserT m Expression
variable = cts $ Variable <$> parser

-- |A parser matching the declarator '\' at the start of a lambda
lambdaDeclarator :: ParserT m ()
lambdaDeclarator = void $ cts $ single '\\'

-- |A parser matching the separator "->" between the arguments of a lambda and the body
lambdaBodySeparator :: ParserT m ()
lambdaBodySeparator = label "lambda separator \"->\"" $ void $ cts $ chunk "->"

lambda :: ParserT m Expression
lambda = combine args body
	where
		args :: ParserT m [Identifier]
		args = lambdaDeclarator *> some parser
		body :: ParserT m Expression
		body = lambdaBodySeparator *> parser
		-- |Takes a list of variable names and an expression, then eta-expands a Lambda wrapper for each one
		combine :: ParserT m [Identifier] -> ParserT m Expression -> ParserT m Expression
		combine = liftA2 $ flip $ foldr' Lambda

builtIn :: ParserT m Expression
builtIn = BuiltIn <$> cts (
			some digitChar <|>
			chunk "print" <|>
			chunk "seq" <|>
			chunk "+" <|>
			chunk "add" <|>
			chunk "-" <|>
			chunk "sub" <|>
			chunk "*" <|>
			chunk "^" <|>
			chunk ";" )

term :: ParserT m Expression
term =	parens parser <|>
		-- explicitType <|>
		lambda <|>
		builtIn <|>
		variable

instance Parsable Expression where
	parser = label "expression" $ quickAndDirty <$> some term