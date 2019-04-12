{-# LANGUAGE TemplateHaskell #-}

module HindleyMilner.Scheme
	( Scheme
	, generalize
	, instantiate
	, reconstructScheme
	-- * Lenses
	, body
	, typeVars
	) where

import AST.Identifier (Identifier)
import Control.Arrow ((&&&), (***))
import Control.Lens
import Data.Set (Set)
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type (Type)

data Scheme = Forall {_typeVars :: Set Identifier, _body :: Type} deriving (Show)
makeLenses ''Scheme

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Substitutable a => a -> Type -> Scheme
generalize substs t = Forall (freeVars t `Set.difference` freeVars substs) t

-- |Instantiate the type variables in a scheme with fresh ones
instantiate :: Applicative f => Scheme -> f Type -> f Type
instantiate (Forall as t) fresh = flip apply t <$> freshSubst fresh (Set.toList as)

-- |Reconstructs a scheme given a type, a type variable list, and a type variable substitution lookup
reconstructScheme :: Monad f => Type -> (Set Identifier, Identifier -> f Identifier) -> f Scheme
reconstructScheme = (uncurry fmap .) . (***) Forall . flip changeVariables

applyUnbound :: (Substitutable a, Substitutable b) => Subst -> a -> b -> b
applyUnbound subst = apply . removeSubstitutions subst . freeVars

instance Substitutable Scheme where
	-- apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	apply subst scheme = (body %~ applyUnbound subst scheme) scheme
	freeVars = uncurry Set.difference . (freeVars . _body &&& _typeVars)
	changeVariables = undefined