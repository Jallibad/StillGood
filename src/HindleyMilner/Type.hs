module HindleyMilner.Type where

import AST.Identifier
import Data.Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)
import GHC.Generics (Generic)

data TypeError
	= UnificationFail Type Type
	| InfiniteType Identifier Type
	| UnboundVariable Identifier
	deriving (Show)

data Type
	= Variable Identifier
	| Constructor Identifier
	| Arrow Type Type
	deriving (Generic, Show, Eq, Ord)
instance ToJSON Type where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Type

numArgs :: Integral a => Type -> a
numArgs (Variable _) = 0
numArgs (Constructor _) = 0
numArgs (Arrow _ a) = numArgs a + 1

data Scheme = Forall [Identifier] Type deriving (Show)

type Environment = Map Identifier Scheme

empty :: Environment
empty = Map.empty