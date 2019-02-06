module HindleyMilner.Substitution where

import AST.Identifier
import Control.Applicative (liftA2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import HindleyMilner.Type

type Subst = Map Identifier Type

-- |Apply substitution 1, then substitution 2
compose :: Subst -> Subst -> Subst
compose s1 = Map.union s1 . fmap (apply s1)

nullSubst :: Subst
nullSubst = Map.empty

-- |Instances of this class have potentially nested subexpressions, this helps us generically traverse them
class Substitutable a where
	-- |Apply a substitution to a type, potentially recursively
	apply :: Subst -> a -> a
	-- |Query for free variables, potentially recursively
	freeVars :: a -> Set Identifier
	-- |Query for occurence of specified free variable in expression
	occurs :: Substitutable a => Identifier -> a -> Bool
	occurs a = Set.member a . freeVars
	changeVariables :: Applicative f => (Identifier -> f Identifier) -> a -> f a

instance Substitutable Type where
	-- A type constructor contains nothing substitutable (base case)
	apply _ (Constructor a) = Constructor a
	-- A type variable can be substituted if in the map (base case)
	apply s t@(Variable a) = Map.findWithDefault t a s
	-- In function application the function and argument should both be substituted recursively
	apply s (t1 `Arrow` t2) = apply s t1 `Arrow` apply s t2

	-- A type constructor contains no free variables
	freeVars (Constructor _) = Set.empty
	-- A type variable is by itself a single free variable
	freeVars (Variable a) = Set.singleton a
	-- A function application contains two subexpression with free variables
	freeVars (t1 `Arrow` t2) = freeVars t1 `Set.union` freeVars t2

	changeVariables :: Applicative f => (Identifier -> f Identifier) -> Type -> f Type
	changeVariables f (a `Arrow` b) = liftA2 Arrow (changeVariables f a) (changeVariables f b)
	changeVariables _ (Constructor a) = pure $ Constructor a
	changeVariables f (Variable a) = Variable <$> f a

instance Substitutable Scheme where
	apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	freeVars (Forall as t) = freeVars t `Set.difference` Set.fromList as

-- An instance allowing substituting each element from a polymorphic class of containers such as [] or Map
-- i.e. `apply subst [e1, e2, e3]` or `apply subst $ Map.fromList [e1, e2, e3]`
instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where
	apply = fmap . apply
	freeVars = foldr (Set.union . freeVars) Set.empty
	-- changeVariables f xs = joinchangeVariables f <$> xs

-- instance Substitutable Environment where
-- 	apply s (Environment env) = Environment $ fmap (apply s) env
-- 	freeVars (Environment env) = freeVars $ Map.elems env