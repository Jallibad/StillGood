module AST.Types where

import AST.Identifier
import Data.Aeson
import Data.Functor.Foldable
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

type instance Base Expression = ExpressionF
instance Recursive Expression where
	project (Variable i) = VariableF i
	project (Lambda a b) = LambdaF a b
	project (Application f b) = ApplicationF f b
	project (ExplicitType a e) = ExplicitTypeF a e
	project (BuiltIn x) = BuiltInF x