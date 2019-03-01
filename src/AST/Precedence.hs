module AST.Precedence where

-- import AST.Identifier
import AST.Types
-- import Control.Arrow
import Control.Monad (MonadPlus)
import Data.Foldable
import Data.Sequence
import HindleyMilner.Type (Type(Arrow), numArgs)

type ApplicationRule = forall t. (Foldable t, MonadPlus t) => t Expression -> Expression

data Associativity = L | R deriving (Show)
data Precedence = Prefix | Infix {level :: Int, assoc :: Associativity} deriving (Show)

-- the rpn rule folds list based on foldingfunction and takes head...
rpn :: ApplicationRule
rpn = head . foldl foldingFunction []
	where
		foldingFunction xs (ExplicitType (numArgs -> a) f@(BuiltIn _)) = apply (f : Prelude.take a xs) : Prelude.drop a xs
		foldingFunction _ _ = undefined

-- applies Application to two expressions?
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

-- given 3, 4, + as op=+, args=4,3 -> (Application (Application + 3) 4)
-- given 5, 2, 4, max as op=max, args = 4,2,5 -> (Application (Application (Application max 5) 2) 4)
-- how do I know op is infix? how can I use foldl?
rpn_apply :: Expression -> [Expression] -> Expression
rpn_apply op exprs =
	case (Data.Foldable.length exprs) of
		0 -> undefined -- error
		1 -> (Application op (head exprs)) -- one arg left
		_ -> (Application (rpn_apply op (tail exprs)) (head exprs))

rpn2 :: [Expression] -> [Expression] -> Expression
rpn2 stack (rpnExpr : rpnExprList) = case rpnExpr of
	-- ExplicitType (HindleyMilner.Type.Variable x) v@(BuiltIn _) -> -- should be BuiltIn Variable type 
	-- 			rpn2 (rpnExpr : stack) rpnExprList -- add to stack
	ExplicitType (Arrow a b) op@(BuiltIn _)	->
				rpn2 ((rpn_apply op args) : remainder) rpnExprList
					where (args, remainder) = (Prelude.take (numArgs (Arrow a b)) stack, Prelude.drop (numArgs (Arrow a b)) stack)  -- BuiltIn operator
	BuiltIn _ -> rpn2 (rpnExpr : stack) rpnExprList -- what about constants like 3
	_ -> undefined -- error
rpn2 stack [] = case (Data.Foldable.length stack) of
	0 -> undefined -- error
	_ -> head stack -- there should only be one item in stack

-- rpn2 [] [(ExplicitType (Arrow (HindleyMilner.Type.Variable "a") (HindleyMilner.Type.Variable "b")) (BuiltIn "3")), (ExplicitType (Arrow (HindleyMilner.Type.Variable "a") (HindleyMilner.Type.Variable "b")) (BuiltIn "4")), (ExplicitType (Arrow (HindleyMilner.Type.Variable "a") (HindleyMilner.Type.Variable "b")) (BuiltIn "+"))]




