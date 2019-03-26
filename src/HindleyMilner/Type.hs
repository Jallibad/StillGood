module HindleyMilner.Type where

import AST.Identifier
-- import Control.Arrow
import Data.Aeson
import Data.Functor.Foldable
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

data TypeF a
	= VariableF Identifier
	| ConstructorF Identifier
	| ArrowF a a
	deriving (Generic, Show, Eq, Ord, Functor)

newtype Term f = In {out :: f (Term f)}

type instance Base Type = TypeF
instance Recursive Type where
	project (Variable i) = VariableF i
	project (Constructor i) = ConstructorF i
	project (Arrow a b) = ArrowF a b
	-- project 

typeInt :: Type
typeInt = Constructor (Identifier "Int")

numArgs :: Integral a => Type -> a
numArgs (Variable _) = 0
numArgs (Constructor _) = 0
numArgs (Arrow _ a) = numArgs a + 1

data Scheme = Forall [Identifier] Type deriving (Show)

type Environment = Map Identifier Scheme

empty :: Environment
empty = Map.empty
