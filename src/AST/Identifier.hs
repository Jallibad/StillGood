module AST.Identifier
	( Identifier (..)
	) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.String (IsString, fromString)
import Data.Text (unpack)
import GHC.Generics (Generic)

-- |Identifier represents a valid variable name in StillGood. A newtype wrapper
-- around String currently, probably to be changed to Text in the future
newtype Identifier = Identifier String deriving (Generic, Eq, Ord)

instance Show Identifier where
	show (Identifier x) = '"' : x ++ "\""
instance IsString Identifier where
	fromString = Identifier
instance ToJSON Identifier where
	toJSON (Identifier s) = toJSON s
	toEncoding (Identifier s) = toEncoding s
instance FromJSON Identifier where
	parseJSON (String s) = pure $ Identifier $ unpack s
	parseJSON invalid = typeMismatch "Identifier" invalid