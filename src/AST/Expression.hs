{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module AST.Expression
	( Expression (..)
	, ExpressionF (..)
	, replaceVar
	) where

import AST.Identifier
import Data.Aeson
import Data.Functor.Foldable (embed, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import GHC.Generics (Generic, Rep)
import HindleyMilner.Type (Type (..))

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

makeBaseFunctor ''Expression
	
instance ToJSON Expression where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Expression

deriving instance Generic (ExpressionF a)
deriving instance Show a => Show (ExpressionF a)
instance ToJSON a => ToJSON (ExpressionF a) where
	toJSON (VariableF i) = object ["tag" .= String "Variable", "identifier" .= i]
	toJSON (LambdaF a b) = object ["tag" .= String "Lambda", "argument" .= a, "body" .= b]
	toJSON (ApplicationF f b) = object ["tag" .= String "Application", "function" .= f, "body" .= b]
	toJSON (ExplicitTypeF _ _) = undefined
	toJSON (BuiltInF i) = object ["tag" .= String "BuiltIn", "contents" .= i]
instance (GFromJSON Zero (Rep (ExpressionF a)), FromJSON a) => FromJSON (ExpressionF a)

replaceVar :: Identifier -> Expression -> Expression -> Expression
replaceVar var val = para $ \case
	(VariableF var') | var == var' -> val
	(LambdaF newVar (orig, replaced)) -> Lambda newVar $ if var == newVar then orig else replaced
	x -> embed $ fmap fst x