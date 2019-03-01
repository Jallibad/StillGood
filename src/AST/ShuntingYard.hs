module AST.ShuntingYard where

import AST.Types
import Control.Arrow ((***))
import Data.Sequence

popToQueue :: Stack -> Line -> Line
popToQueue stack queue = queue >< fromList (Prelude.reverse stack)

replaceOperators :: Operator -> Line -> (Stack, Stack) -> (Line, Stack)
replaceOperators op = (*** (op :)) . flip popToQueue

shuntingYardAlgorithm :: OperatorHandler -> Stack -> Line
shuntingYardAlgorithm handleInput = uncurry popToQueue . foldl handleInput emptyYard

emptyYard :: ShuntingYard
emptyYard = ([], empty)

type Operator = Expression
type Line = Seq Operator
type Stack = [Operator]
type ShuntingYard = (Stack, Line)
type OperatorHandler = ShuntingYard -> Operator -> ShuntingYard