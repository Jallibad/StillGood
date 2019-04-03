{-# LANGUAGE TemplateHaskell #-}

module HindleyMilner.Scheme
	( Scheme (..)
	, generalize
	, reconstructScheme

	-- * Lenses
	, body
	, typeVars
	) where

import AST.Identifier
import Control.Arrow ((&&&), (***))
import Control.Lens
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type

data Scheme = Forall {_typeVars :: [Identifier], _body :: Type} deriving (Show)
makeLenses ''Scheme

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Substitutable a => a -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env

-- |Reconstructs a scheme given a type, a type variable list, and a type variable substitution lookup
reconstructScheme :: Monad f => Type -> ([Identifier], Identifier -> f Identifier) -> f Scheme
reconstructScheme = (uncurry fmap .) . (Forall ***) . flip changeVariables

applyUnbound :: Subst -> Scheme -> Type -> Type
applyUnbound subst = apply . removeSubstitutions subst . _typeVars

instance Substitutable Scheme where
	-- apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	apply subst scheme = (body %~ applyUnbound subst scheme) scheme
	freeVars = uncurry Set.difference . (freeVars . _body &&& Set.fromList . _typeVars)
	changeVariables = undefined