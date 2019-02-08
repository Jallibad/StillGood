module HindleyMilner.Scheme where

import AST.Identifier
import Control.Arrow ((***))
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Environment -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env

-- |Reconstructs a scheme given a type, a type variable list, and a type variable substitution lookup
reconstructScheme :: Monad f => Type -> ([Identifier], Identifier -> f Identifier) -> f Scheme
reconstructScheme body = uncurry fmap . (Forall *** flip changeVariables body)