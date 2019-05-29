module HindleyMilner.TypeError
	( TypeError (..)
	) where

import AST.Identifier (Identifier)
import Data.Aeson
import GHC.Generics (Generic)
import HindleyMilner.Type (Type)

-- |An enumeration of the different methods of type errors
data TypeError
	= UnificationFail Type Type
	| InfiniteType Identifier Type
	| TooManyArguments -- TODO Add context details
	| UnboundVariable Identifier
	| InvalidBuiltIn String
	| Deprecated
	deriving (Generic, Show)

instance ToJSON TypeError where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON TypeError