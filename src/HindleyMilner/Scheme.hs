module HindleyMilner.Scheme
	( Scheme (..)
	, generalize
	, reconstructScheme
	) where

import AST.Identifier
import Control.Arrow ((***))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type

data Scheme = Forall [Identifier] Type deriving (Show)

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Substitutable a => a -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env

-- |Reconstructs a scheme given a type, a type variable list, and a type variable substitution lookup
reconstructScheme :: Monad f => Type -> ([Identifier], Identifier -> f Identifier) -> f Scheme
reconstructScheme body = uncurry fmap . (Forall *** flip changeVariables body)

instance Substitutable Scheme where
	apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	freeVars (Forall as t) = freeVars t `Set.difference` Set.fromList as
	changeVariables = undefined