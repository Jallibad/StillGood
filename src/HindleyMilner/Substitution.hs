module HindleyMilner.Substitution
	( Subst
	, Substitutable (..)
	, compose
	, nullSubst
	, removeSubstitutions
	) where

import AST.Identifier (Identifier)
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

-- |Removes collections of Identifiers from the substitution Map
removeSubstitutions :: Foldable f => Subst -> f Identifier -> Subst
removeSubstitutions = foldr Map.delete

-- |Instances of this class have potentially nested subexpressions, this helps us generically traverse them
class Substitutable a where
	-- |Apply a substitution to a type, potentially recursively
	apply :: Subst -> a -> a
	apply' :: (Subst, a) -> a
	apply' = uncurry apply
	-- |Query for free variables, potentially recursively
	freeVars :: a -> Set Identifier
	-- |Query for occurence of specified free variable in expression
	occurs :: Substitutable a => Identifier -> a -> Bool
	occurs a = Set.member a . freeVars
	-- |Uses the given lookup function to update each type variable.
	-- The type variable `f` in the function signature is intended to hold errors, perhaps in Maybe or Either
	changeVariables :: Monad m => (Identifier -> m Identifier) -> a -> m a

substituteIdentifier :: Subst -> Identifier -> Type
substituteIdentifier s a = Map.findWithDefault (Variable a) a s

instance Substitutable Type where
	-- A type variable can be substituted if in the map (base case)
	-- A type constructor contains nothing substitutable (base case)
	-- In function application the function and argument should both be substituted recursively
	apply s = typeCata (substituteIdentifier s) Constructor Arrow
	-- A type variable is by itself a single free variable
	-- A type constructor contains no free variables
	-- A function application contains two subexpression with free variables
	freeVars = typeCata Set.singleton (const Set.empty) Set.union
	changeVariables f = typeCata (fmap Variable . f) (pure . Constructor) (liftA2 Arrow)

-- An instance allowing substituting each element from a polymorphic class of containers such as [] or Map
-- i.e. `apply subst [e1, e2, e3]` or `apply subst $ Map.fromList [e1, e2, e3]`
instance (Traversable f, Substitutable a) => Substitutable (f a) where
	apply = fmap . apply
	freeVars = foldr (Set.union . freeVars) Set.empty
	changeVariables = mapM . changeVariables

-- instance Substitutable Environment where
-- 	apply s (Environment env) = Environment $ fmap (apply s) env
-- 	freeVars (Environment env) = freeVars $ Map.elems env