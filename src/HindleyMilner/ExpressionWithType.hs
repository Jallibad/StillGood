module HindleyMilner.ExpressionWithType
	( ExpressionWithType (..)
	) where

import AST.Types
import Data.Aeson
import GHC.Generics (Generic)
import HindleyMilner.Substitution
import HindleyMilner.Type

data ExpressionWithType = ExpressionWithType {expression :: ExpressionF ExpressionWithType, annotation :: Type}
	deriving (Generic, Show)

instance ToJSON ExpressionWithType where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON ExpressionWithType

instance Substitutable ExpressionWithType where
	apply s (ExpressionWithType e t) = ExpressionWithType (apply s e) (apply s t)
	freeVars (ExpressionWithType e _) = freeVars e
	changeVariables f (ExpressionWithType e t) = flip ExpressionWithType t <$> changeVariables f e