module Types where

import Data.Aeson
import GHC.Generics (Generic)

newtype Identifier = Identifier String deriving (Generic)
deriving instance Show Identifier
instance ToJSON Identifier where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Identifier

data Expression = Variable {identifier :: Identifier}
				| Lambda {argument :: Identifier, body :: Expression}
				| Application {function :: Expression, body :: Expression}
				| BuiltIn String
				deriving (Generic, Show)
instance ToJSON Expression where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Expression