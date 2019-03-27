module AST.Identifier where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.String (IsString, fromString)
import Data.Text (unpack)
import GHC.Generics (Generic)

newtype Identifier = Identifier String deriving (Generic, Eq, Ord)
deriving instance Show Identifier
instance IsString Identifier where
	fromString = Identifier
instance ToJSON Identifier where
	toJSON (Identifier s) = toJSON s
	toEncoding (Identifier s) = toEncoding s
instance FromJSON Identifier where
	parseJSON (String s) = pure $ Identifier $ unpack s
	parseJSON invalid = typeMismatch "Identifier" invalid