module AST.Identifier where

import Data.Aeson
import Data.String (IsString, fromString)
import GHC.Generics (Generic)

newtype Identifier = Identifier String deriving (Generic, Eq, Ord)
deriving instance Show Identifier
instance IsString Identifier where
	fromString = Identifier
instance ToJSON Identifier where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Identifier