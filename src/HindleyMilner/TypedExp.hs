-- {-# LANGUAGE TemplateHaskell #-}

module HindleyMilner.TypedExp
	( ExpSubsTyped
	, TypedExp (..)
	, replaceVar
	) where

import AST.Expression (ExpressionF (..))
import AST.Identifier (Identifier)
import Data.Aeson
import GHC.Generics (Generic)
import HindleyMilner.Substitution (Substitutable (..))
import HindleyMilner.Type (Type)

type ExpSubsTyped = ExpressionF TypedExp

-- |An expression and its type, subexpressions also explicitly have their own types.
data TypedExp =
	TypedExp {expression :: ExpSubsTyped, annotation :: Type}
	deriving (Generic, Show)

-- makeBaseFunctor ''TypedExp

-- TODO: Add typechecking
replaceVar :: Identifier -> TypedExp -> TypedExp -> TypedExp
replaceVar var val (TypedExp (VariableF var') _) | var == var' = val
replaceVar var val orig@(TypedExp (LambdaF newVar body) t)
	| var == newVar = orig
	| otherwise = TypedExp (LambdaF newVar (replaceVar var val body)) t
replaceVar var val (TypedExp (ApplicationF func arg) t) = TypedExp (ApplicationF (replaceVar var val func) (replaceVar var val arg)) t
replaceVar _ _ x = x
-- replaceVar var val (TypedExp x t) = TypedExp (replaceVar var val x) t

	-- (LambdaF newVar (orig, replaced)) -> Lambda newVar $ if var == newVar then orig else replaced
	-- x -> embed $ fmap fst x

instance ToJSON TypedExp where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON TypedExp

instance Substitutable TypedExp where
	apply s (TypedExp e t) = TypedExp (apply s e) (apply s t)
	freeVars = freeVars . expression
	changeVariables f (TypedExp e t) = flip TypedExp t <$> changeVariables f e