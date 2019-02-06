module HindleyMilner.Scheme where

import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Environment -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env