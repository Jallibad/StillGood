module AST.Expression
	( Expression (..)
	, ExpressionF (..)
	) where

import AST.Identifier
import Data.Aeson
import Data.Functor.Foldable
import GHC.Generics (Generic)
import HindleyMilner.Type (Type)

-- |A node in the syntax tree, potentially containing subexpressions.
data Expression
	-- |A variable with an identifier, must be in scope during type inference.
	= Variable {identifier :: Identifier}
	-- |A lambda containing an argument identifier and a subexpression for the body.
	-- In the future pattern matching should probably desugar to this.
	| Lambda {argument :: Identifier, body :: Expression}
	-- |An expression (function) with another expression (body) being applied to it.
	| Application {function :: Expression, body :: Expression}
	-- |Should be deprecated in the near future
	| ExplicitType {annotation' :: Type, expression :: Expression}
	-- |A string built into the compiler, contains language primitives.
	-- Should probably be broken into different types
	| BuiltIn String
	deriving (Generic, Show, Eq)
	
instance ToJSON Expression where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Expression

data ExpressionF a
	= VariableF {identifier :: Identifier}
	| LambdaF {argument :: Identifier, body :: a}
	| ApplicationF {function :: a, body :: a}
	-- |Should be deprecated in the near future
	| ExplicitTypeF {annotation' :: Type, expression :: a}
	| BuiltInF String
	deriving (Generic, Show, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (ExpressionF a) where
	toJSON (VariableF i) = object ["tag" .= String "Variable", "identifier" .= i]
	toJSON (LambdaF a b) = object ["tag" .= String "Lambda", "argument" .= a, "body" .= b]
	toJSON (ApplicationF f b) = object ["tag" .= String "Application", "function" .= f, "body" .= b]
	toJSON (ExplicitTypeF _ _) = undefined
	toJSON (BuiltInF i) = object ["tag" .= String "BuiltIn", "contents" .= i]
instance FromJSON a => FromJSON (ExpressionF a)

type instance Base Expression = ExpressionF
instance Recursive Expression where
	project (Variable i) = VariableF i
	project (Lambda a b) = LambdaF a b
	project (Application f b) = ApplicationF f b
	project (ExplicitType a e) = ExplicitTypeF a e
	project (BuiltIn x) = BuiltInF x