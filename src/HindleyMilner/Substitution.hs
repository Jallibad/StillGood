module HindleyMilner.Substitution
	( Subst
	, Substitutable (..)
	, freshSubst
	, removeSubstitutions
	, single
	) where

import AST.Identifier (Identifier)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import HindleyMilner.Type (Type (..), typeCata)

-- |A type representing the known replacements between type variables and the their bound types
newtype Subst = Subst {unsubst :: Map Identifier Type}

instance Semigroup Subst where
	(<>) s@(Subst m) = Subst . Map.union m . fmap (apply s) . unsubst
instance Monoid Subst where
	mempty = Subst Map.empty

-- |A substitution with a single Identifier and associated Type replacement
single :: Identifier -> Type -> Subst
single v t
	| t == Variable v = mempty
	| otherwise = Subst $ Map.singleton v t

makeSubst :: [Identifier] -> [Type] -> Subst
makeSubst = ((Subst . Map.fromList) .) . zip

freshSubst :: Applicative f => f Type -> [Identifier] -> f Subst
freshSubst = (uncurry fmap .) . (&&&) makeSubst . flip (replicateM . length)

-- |Removes collections of Identifiers from the substitution Map
removeSubstitutions :: Foldable f => Subst -> f Identifier -> Subst
removeSubstitutions = (Subst .) . foldr Map.delete . unsubst

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

instance Substitutable Type where
	-- A type variable can be substituted if in the map (base case)
	-- A type constructor contains nothing substitutable (base case)
	-- In function application the function and argument should both be substituted recursively
	apply (Subst s) = typeCata substitute Constructor Arrow
		where substitute a = Map.findWithDefault (Variable a) a s

	-- A type variable is by itself a single free variable
	-- A type constructor contains no free variables
	-- A function application contains two subexpression with free variables
	freeVars = typeCata Set.singleton (const Set.empty) Set.union

	-- A type variable should be replaced
	-- A type constructor should be left alone
	-- Both parts of a function's type should be replaced recursively
	changeVariables f = typeCata (fmap Variable . f) (pure . Constructor) (liftA2 Arrow)

-- An instance allowing substituting each element from a polymorphic class of containers such as [] or Map
-- i.e. `apply subst [e1, e2, e3]` or `apply subst $ Map.fromList [e1, e2, e3]`
instance (Traversable f, Substitutable a) => Substitutable (f a) where
	apply = fmap . apply
	freeVars = foldr (Set.union . freeVars) Set.empty
	changeVariables = mapM . changeVariables