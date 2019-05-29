{-# LANGUAGE TemplateHaskell #-}

module HindleyMilner.Type
	( Type (..)
	, TypeF (..)
	, numArgs
	, typeCata
	, typeInt
	, typeIO
	) where

import AST.Identifier (Identifier)
import Data.Aeson
import Data.Functor.Foldable (cata, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.String (IsString)
import GHC.Generics (Generic)

-- |Represents the type signature of a StillGood expression
data Type
	= Variable Identifier
	| Constructor Identifier
	| Arrow {input :: Type, output :: Type}
	-- TODO Add application in the future to support type level functions
	deriving (Generic, Eq)

instance ToJSON Type where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Type

makeBaseFunctor ''Type

toString :: (Semigroup a, IsString a) => (Identifier -> a) -> Type -> a
toString f = para $ \case
	VariableF x -> "V@" <> f x
	ConstructorF x -> "C@" <> f x
	ArrowF (Arrow _ _, input) (_, output) -> "(" <> input <> ") -> " <> output
	ArrowF (_, input) (_, output) -> input <> " -> " <> output

instance Show Type where
	show = toString show

typeCata :: (Identifier -> a) -> (Identifier -> a) -> (a -> a -> a) -> Type -> a
typeCata v c a = cata $ \case
	(VariableF x) -> v x
	(ConstructorF x) -> c x
	(t1 `ArrowF` t2) -> a t1 t2

typeInt :: Type
typeInt = Constructor "Int"

typeIO :: Type
typeIO = Constructor "IO"

numArgs :: Integral a => Type -> a
numArgs = typeCata (const 0) (const 0) (const succ)

data LinearType b
	= LinearVariable Identifier
	| LinearConstructor Identifier
	| LinearArrow b b
	deriving (Generic, Show, Eq, Ord, Functor)
