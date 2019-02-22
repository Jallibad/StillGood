module AST.Precedence where

-- import AST.Identifier
import AST.Types
-- import Control.Arrow
import Control.Monad (MonadPlus)
import Data.Foldable
import Data.Sequence
import HindleyMilner.Type (numArgs)

type ApplicationRule = forall t. (Foldable t, MonadPlus t) => t Expression -> Expression

data Associativity = L | R deriving (Show)
data Precedence = Prefix | Infix {level :: Int, assoc :: Associativity} deriving (Show)

rpn :: ApplicationRule
rpn = head . foldl foldingFunction []
	where
		foldingFunction xs (ExplicitType (numArgs -> a) f@(BuiltIn _)) = apply (f : Prelude.take a xs) : Prelude.drop a xs
		foldingFunction _ _ = undefined

apply :: ApplicationRule
apply = foldl1 Application

applyWithPrecedence :: ApplicationRule
applyWithPrecedence = rpn . shuntingYardAlgorithm . toList

precedence :: Expression -> Precedence
precedence (BuiltIn x) = case x of
	"+"		-> Infix 1 L
	"-"		-> Infix 1 L
	"*"		-> Infix 2 L
	"/"		-> Infix 2 L
	"^"		-> Infix 3 R
	";"		-> Infix 4 L
	_		-> Prefix
precedence (ExplicitType _ x) = precedence x
precedence _ = Prefix

popToQueue :: [(Expression, b)] -> Seq Expression -> Seq Expression
popToQueue stack queue = queue >< fromList (Prelude.reverse $ map fst stack)

shuntingYardAlgorithm :: [Expression] -> Seq Expression
shuntingYardAlgorithm = sya [] empty . map (\x -> (x, precedence x))

sya :: [(Expression, Int)] -> Seq Expression -> [(Expression, Precedence)] -> Seq Expression
sya stack queue ((expression, prec) : tokenList) = case prec of
	Prefix		-> sya stack (queue |> expression) tokenList
	Infix n _a	-> sya ((expression, n) : remainder) (popToQueue parentOperators queue) tokenList
		where (parentOperators, remainder) = break ((>n) . snd) stack
sya stack queue [] = popToQueue stack queue