module HindleyMilner.Substitution where

import Data.Foldable (foldr')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import HindleyMilner.Type
import Types (Identifier)

type Subst = Map Identifier Type

nullSubst :: Subst
nullSubst = Map.empty

class Substitutable a where
	apply :: Subst -> a -> a
	freeVars :: a -> Set Identifier

instance Substitutable Type where
	apply _ (Constructor a) = Constructor a
	apply s t@(HindleyMilner.Type.Variable a) = Map.findWithDefault t a s
	apply s (t1 `Arrow` t2) = apply s t1 `Arrow` apply s t2

	freeVars (Constructor _) = Set.empty
	freeVars (HindleyMilner.Type.Variable a) = Set.singleton a
	freeVars (t1 `Arrow` t2) = freeVars t1 `Set.union` freeVars t2

instance Substitutable Scheme where
	apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	freeVars (Forall as t) = freeVars t `Set.difference` Set.fromList as

instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
	apply = fmap . apply
	freeVars = foldr' (Set.union . freeVars) Set.empty

instance Substitutable Environment where
	apply s (Environment env) = Environment $ fmap (apply s) env
	freeVars (Environment env) = freeVars $ Map.elems env