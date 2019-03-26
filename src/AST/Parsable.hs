module AST.Parsable
	( Parsable
	, Parser
	, parseExpression
	, parseTypes
	, parser
	) where

import AST.Identifier
import AST.Precedence
import AST.Types
import HindleyMilner.Type (Type, TypeError, Scheme, empty)
import HindleyMilner.Infer (runInfer)
import HindleyMilner (infer) -- file, not in same package
import Control.Applicative (liftA2)
import Control.Monad (void)
import Data.Foldable (foldr')
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- |The space consumer combinator consumes at least one character of whitespace, or a comment
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt where
	lineCmnt = L.skipLineComment "--"
  	blockCmnt = L.skipBlockComment "/*" "*/"

-- |The consume trailing space combinator modifies a parser to consume any potential trailing whitespace
cts :: Parser a -> Parser a
cts = (<* optional sc)

class Parsable a where
	parser :: Parser a

instance Parsable Identifier where
	parser = label "identifier" $ cts $ fmap Identifier $ liftA2 (:) letterChar $ many alphaNumChar

instance Parsable Type where
	parser = undefined -- label "type" $ cts $

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
builtIn = BuiltIn <$> cts (
			some digitChar <|>
			chunk "+" <|>
			chunk "-" <|>
			chunk "*" <|>
			chunk "^" <|>
			chunk ";" )

term :: Parser Expression
term =	parens parser <|>
		-- explicitType <|>
		lambda <|>
		builtIn <|>
		variable

instance Parsable Expression where
	parser = label "expression" $ apply <$> some term


parseTypes :: Expression -> Either TypeError Scheme -- should return expressions with type built in
parseTypes e =
	let env = HindleyMilner.Type.empty in (runInfer (infer env e)) -- does the issue have to do with left and right?

-- I think what I need to do is here
parseExpression :: String -> String -> Either (ParseErrorBundle String Void) Expression
parseExpression src e =
	let ret1 = runParser parser src e in case ret1 of
		Left _ -> ret1
		Right _ -> ret1
			-- let ret2 = (parseTypes res) in case ret2 of
			-- 	Left _ -> undefined -- ideally ret2...
			-- 	Right _ -> ret1
