module HindleyMilner.Type where

import Data.Map.Strict (Map)
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

newtype Environment = Environment (Map Identifier Scheme)