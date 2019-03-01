module AST.Precedence where

-- import AST.Identifier
import AST.ShuntingYard hiding (popToQueue)
import AST.Types
import Control.Arrow
import Control.Monad (MonadPlus)
import Data.Foldable
import Data.Function
import Data.Sequence
import HindleyMilner.Type (Type(Arrow), numArgs)
import Data.Tuple

type ApplicationRule = forall t. (Foldable t, MonadPlus t) => t Expression -> Expression

data Associativity = L | R deriving (Eq, Show)
data Precedence = Prefix | Infix {level :: Int, assoc :: Associativity} deriving (Eq, Show)

instance Ord Precedence where
	compare Prefix	Prefix	= EQ
	compare Prefix	_		= LT
	compare _		Prefix	= GT
	compare (Infix n1 _) (Infix n2 _) = compare n1 n2

rpn :: ApplicationRule
rpn = head . foldl foldingFunction []
	where
		foldingFunction xs (ExplicitType (numArgs -> a) f@(BuiltIn _)) = apply (f : Prelude.take a xs) : Prelude.drop a xs
		foldingFunction _ _ = undefined

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


-- pretty sure this is all wrong, I need to work on this [TODO]
rpnApply :: Expression -> [Expression] -> Expression
rpnApply op exprs =
	case Data.Foldable.length exprs of
		0 -> undefined -- error
		1 -> Application op $ head exprs -- one arg left
		_ -> Application (rpnApply op $ tail exprs) $ head exprs

-- this needs to apply to various kinds of types, where an operator can be treated as an operand
rpn2 :: [Expression] -> [Expression] -> Expression
rpn2 stack (rpnExpr : rpnExprList) = case rpnExpr of
	-- ExplicitType (HindleyMilner.Type.Variable x) v@(BuiltIn _) -> -- should be BuiltIn Variable type 
	-- 	rpn2 (rpnExpr : stack) rpnExprList -- add to stack
	ExplicitType (Arrow a b) op@(BuiltIn _)	-> rpn2 (rpnApply op args : remainder) rpnExprList
		where (args, remainder) = (Prelude.take (numArgs (Arrow a b)) stack, Prelude.drop (numArgs (Arrow a b)) stack)  -- BuiltIn operator
	BuiltIn _ -> rpn2 (rpnExpr : stack) rpnExprList
	_ -> undefined -- error
rpn2 stack [] = case Data.Foldable.length stack of
	0 -> undefined -- error
	_ -> head stack -- there should only be one item in stack

takeExpression :: (Stack, Stack) -> Maybe (((Stack, Stack), Line), Expression)
takeExpression (_, []) = Nothing
takeExpression (_stack, _input) = undefined
