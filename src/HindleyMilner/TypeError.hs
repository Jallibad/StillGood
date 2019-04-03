module HindleyMilner.TypeError
	( TypeError (..)
	) where

import AST.Identifier
import Data.Aeson
import GHC.Generics (Generic)
import HindleyMilner.Type

-- |An enumeration of the different methods of type errors
data TypeError
	= UnificationFail Type Type
	| InfiniteType Identifier Type
	| TooManyArguments -- TODO Add context details
	| UnboundVariable Identifier
	deriving (Generic, Show)
instance ToJSON TypeError where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON TypeError