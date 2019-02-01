module HindleyMilner.Type where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import Types (Identifier)

data TypeError
	= UnificationFail Type Type
	| InfiniteType Identifier Type
	| UnboundVariable String
	deriving (Show)

data Type
	= Variable Identifier
	| Constructor String
	| Arrow Type Type
	deriving (Show, Eq, Ord)

data Scheme = Forall [Identifier] Type deriving (Show)

type Environment = Map Identifier Scheme

empty :: Environment
empty = Map.empty