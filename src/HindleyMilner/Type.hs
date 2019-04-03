module HindleyMilner.Type where

import AST.Identifier
import Data.Aeson
import Data.Functor.Foldable
import GHC.Generics (Generic)

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

type instance Base Type = TypeF

instance Recursive Type where
	project (Variable i) = VariableF i
	project (Constructor i) = ConstructorF i
	project (Arrow a b) = ArrowF a b

typeCata :: (Identifier -> a) -> (Identifier -> a) -> (a -> a -> a) -> Type -> a
typeCata v c a = cata $ \case
	(VariableF x) -> v x
	(ConstructorF x) -> c x
	(t1 `ArrowF` t2) -> a t1 t2

typeInt :: Type
typeInt = Constructor $ Identifier "Int"

numArgs :: Integral a => Type -> a
numArgs = typeCata (const 0) (const 0) (const succ)