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
import HindleyMilner.Type (Type, TypeError, empty)
import HindleyMilner.Infer (getExplicitState)
import HindleyMilner (inferExplicitType) -- file, not in same package
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

-- examples:
-- parseTypes (Lambda "x" (AST.Types.Variable "x"))
-- parseTypes (Lambda "y" (Application (Lambda "x" (AST.Types.Variable "x")) (AST.Types.Variable "y")))
parseTypes :: Expression -> Either TypeError Expression -- should return expressions with type built in
parseTypes = getExplicitState . inferExplicitType HindleyMilner.Type.empty

-- doesn't get 'forall' types yet
-- Given expression as a string, parse and infer types
parseExpression :: String -> String -> Either (ParseErrorBundle String Void) Expression
parseExpression src e = case ret1 of
		Left _ -> ret1
		Right e1 -> -- ret1 -- should run type inference here (using parseTypes, when finished)
			case parseTypes e1 of
				Left _ -> ret1 -- Left (ParseErrorBundle "error" (void ""))
				Right e2 -> Right e2
	where
		ret1 = runParser parser src e