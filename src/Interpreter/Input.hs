module Interpreter.Input where

import AST.Identifier (Identifier)
import AST.Parsable (ParserT, cts, parser)
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Control.Monad.Trans.Except (Except, runExcept)
import Data.Void (Void)
import HindleyMilner (TypeError, addType')
import Interpreter.Program
import Interpreter.RuntimeExpression
import Text.Megaparsec hiding (getInput)

data Input
	= AddExp Identifier RExpr
	| RunExp RExpr
	| PrintType RExpr
	| Quit

type ExpReader = ParserT (ReaderT Program (Except TypeError))
data ExpReaderError
	= TypeError TypeError
	| ParseError String
	deriving (Show)

collapseError :: Either TypeError (Either (ParseErrorBundle String Void) a) -> Either ExpReaderError a
collapseError (Left e) = Left $ TypeError e
collapseError (Right (Left e)) = Left $ ParseError $ errorBundlePretty e
collapseError (Right (Right x)) = Right x

-- |@runExpressionReader reader input program@ runs the ExpReader @reader@ with an input and an initial program state
runExpressionReader :: ExpReader a -> String -> Program -> Either ExpReaderError a
runExpressionReader = ((((collapseError . runExcept) .) . runReaderT) .) . flip runParserT ""

getExp :: ExpReader RExpr
getExp = do	
	expr <- parser
	env <- asks toEnv
	lift $ lift $ do
		withType <- addType' env expr
		typedExpToRuntime withType

addExp :: ExpReader Input
addExp = do
	cts $ chunk ":a"
	identifier <- parser
	cts $ chunk "="
	AddExp identifier <$> getExp

runExp :: ExpReader Input
runExp = RunExp <$> getExp

printType :: ExpReader Input
printType = cts (chunk ":t") >> PrintType <$> getExp

quit :: ExpReader Input
quit = do
	cts $ chunk ":q"
	return Quit

getInput :: ExpReader Input
getInput = addExp <|> printType <|> runExp <|> quit