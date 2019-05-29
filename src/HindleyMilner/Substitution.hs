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
import Control.Monad.Trans.Except
import Data.Map.Merge.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import HindleyMilner.Type (Type (..), typeCata)
import HindleyMilner.TypeError (TypeError (..))

-- |A type representing the known replacements between type variables and the their bound types
newtype Subst = Subst {unsubst :: Except TypeError (Map Identifier Type)}

throwIfMatched :: WhenMatched (Except TypeError) k Type Type Type
throwIfMatched = zipWithAMatched $ const $ (throwE .) . UnificationFail

combiningFunction :: Ord k => Map k Type -> Map k Type -> Except TypeError (Map k Type)
combiningFunction = mergeA preserveMissing preserveMissing throwIfMatched

instance Semigroup Subst where
	s1 <> s2 = Subst $ unsubst s1 >>= ((apply s1 <$> unsubst s2) >>=) . combiningFunction

instance Monoid Subst where
	mempty = Subst $ return Map.empty

-- |A substitution with a single Identifier and associated Type replacement
single :: Identifier -> Type -> Subst
single v t
	| t == Variable v = mempty
	| otherwise = Subst $ return $ Map.singleton v t

makeSubst :: [Identifier] -> [Type] -> Subst
makeSubst = ((Subst . return . Map.fromList) .) . zip

freshSubst :: Applicative f => f Type -> [Identifier] -> f Subst
freshSubst = (uncurry fmap .) . (&&&) makeSubst . flip (replicateM . length)

-- |Removes collections of Identifiers from the substitution lookup
removeSubstitutions :: Foldable f => Subst -> f Identifier -> Subst
removeSubstitutions (Subst s) is = Subst $ flip (foldr Map.delete) is <$> s

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
		where
			substitute a = either (const $ Variable a) id $ runExcept $ Map.findWithDefault (Variable a) a <$> s

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