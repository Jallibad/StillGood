module AST.Precedence
	( apply
	, applyWithPrecedence
	, isPrefix
	, sya
	, takeExpression
	) where

import AST.Expression
import AST.ShuntingYard hiding (popToQueue)
import Control.Arrow
import Control.Monad (MonadPlus)
import Data.Foldable
import Data.Function
import Data.Sequence
import HindleyMilner.Type (numArgs)
import Data.Tuple

type ApplicationRule = forall t. (Foldable t, MonadPlus t) => t Expression -> Expression

data Associativity = L | R deriving (Eq, Show)
data Precedence = Prefix | Infix {level :: Int, assoc :: Associativity} deriving (Eq, Show)

instance Ord Precedence where
	compare Prefix	Prefix	= EQ
	compare Prefix	_		= LT
	compare _		Prefix	= GT
	compare (Infix n1 _) (Infix n2 _) = compare n1 n2

rpnApply :: Expression -> [Expression] -> Expression
rpnApply op [] = op
rpnApply op (x:xs) = Application (rpnApply op xs) x

-- apply foldingFunction to list of expressions using foldl starting with empty list
rpn :: ApplicationRule
rpn = head . foldl foldingFunction []
	where
		foldingFunction xs (ExplicitType (numArgs -> argNum) f@(BuiltIn _))
			| argNum > 0	= let (a,b) = Prelude.splitAt argNum xs in rpnApply f a : b
			| otherwise		= f : xs
		foldingFunction _ _ = undefined

-- applies Application to two expressions?
apply :: ApplicationRule
apply = foldl1 Application

applyWithPrecedence :: ApplicationRule
applyWithPrecedence = rpn . shuntingYardAlgorithm handleInput . toList

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

isPrefix :: Expression -> Bool
isPrefix (precedence -> Prefix) = True
isPrefix _ = False

-- takePrefix :: Stack -> (Expression, Stack)
-- takePrefix 

handleInput :: OperatorHandler
handleInput input op = case precedence op of
	Prefix -> second (|> op) input
	prec@(Infix _ _) -> swap $ uncurry (&) $ thing input
		where
			thing :: ShuntingYard -> ((Stack, Stack), (Stack, Stack) -> (Line, Stack))
			thing = break ((>= prec) . precedence) *** replaceOperators op

popToQueue :: [(Expression, b)] -> Seq Expression -> Seq Expression
popToQueue stack queue = queue >< fromList (Prelude.reverse $ map fst stack)

sya :: [(Expression, Int)] -> Seq Expression -> [(Expression, Precedence)] -> Seq Expression
sya stack queue ((expression, prec) : tokenList) = case prec of
	Prefix		-> sya stack (queue |> expression) tokenList
	Infix n _a	-> sya ((expression, n) : remainder) (popToQueue parentOperators queue) tokenList
		where (parentOperators, remainder) = break ((>n) . snd) stack
sya stack queue [] = popToQueue stack queue

takeExpression :: (Stack, Stack) -> Maybe (((Stack, Stack), Line), Expression)
takeExpression (_, []) = Nothing
takeExpression (_stack, _input) = undefined