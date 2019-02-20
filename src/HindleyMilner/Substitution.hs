module HindleyMilner.Substitution where

import AST.Identifier
import Control.Applicative (liftA2)
import Control.Arrow-- ((>>>))
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
	-- |Uses the given lookup function to update each type variable.
	-- The type variable `f` in the function signature is intended to hold errors, perhaps in Maybe or Either
	changeVariables :: Monad m => (Identifier -> m Identifier) -> a -> m a

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

	changeVariables :: Monad m => (Identifier -> m Identifier) -> Type -> m Type
	changeVariables f (a `Arrow` b) = liftA2 Arrow (changeVariables f a) (changeVariables f b)
	changeVariables _ (Constructor a) = pure $ Constructor a
	changeVariables f (Variable a) = Variable <$> f a

instance Substitutable Scheme where
	apply s (Forall as t) = Forall as $ apply (foldr Map.delete s as) t
	freeVars (Forall as t) = freeVars t `Set.difference` Set.fromList as
	changeVariables = undefined

-- An instance allowing substituting each element from a polymorphic class of containers such as [] or Map
-- i.e. `apply subst [e1, e2, e3]` or `apply subst $ Map.fromList [e1, e2, e3]`
instance (Traversable f, Substitutable a) => Substitutable (f a) where
	apply = fmap . apply
	freeVars = foldr (Set.union . freeVars) Set.empty
	changeVariables = mapM . changeVariables

-- instance Substitutable Environment where
-- 	apply s (Environment env) = Environment $ fmap (apply s) env
-- 	freeVars (Environment env) = freeVars $ Map.elems env

type TypeVarAssocList a = [(Identifier, a)]

-- |Looks up a type variable in an associative list, returns a TypeError if not in the list
lookupF :: Identifier -> TypeVarAssocList a -> Either TypeError a
lookupF = uncurry (.) . (flip maybe Right . Left . UnboundVariable &&& lookup)

-- |Looks up a type variable in an associative list, returns a TypeError if not in the list
lookupF' :: Identifier -> TypeVarAssocList a -> Either TypeError a
lookupF' x = lookup x >>> \case
	Nothing	-> Left $ UnboundVariable x
	Just y	-> Right y