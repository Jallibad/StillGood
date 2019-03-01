module AST.Types where

import AST.Identifier
import Data.Aeson
import GHC.Generics (Generic)
import HindleyMilner.Type (Type)

data Expression = Variable {identifier :: Identifier}
				| Lambda {argument :: Identifier, body :: Expression}
				| Application {function :: Expression, body :: Expression}
				| ExplicitType {annotation :: Type, expression :: Expression}
				| BuiltIn String
				deriving (Generic, Show)
instance ToJSON Expression where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Expression

data ExpressionF a = VariableF {identifier :: Identifier}
				| LambdaF {argument :: Identifier, body :: a}
				| ApplicationF {function :: a, body :: a}
				| ExplicitTypeF {annotation :: Type, expression :: a}
				| BuiltInF String
				deriving (Generic, Show, Functor)

newtype Term f = In {out :: f (Term f)}
-- instance ToJSON ExpressionF a where
-- 	toEncoding = genericToEncoding defaultOptions
-- instance FromJSON Expression