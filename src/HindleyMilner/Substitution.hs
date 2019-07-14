module HindleyMilner.Substitution
	( Subst
	, Substitutable (..)
	, (!?)
	, coalesceTypeError
	, freshSubst
	, lookupSubst
	, removeSubstitutions
	, unify
	) where

import AST.Identifier (Identifier)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (Except, ExceptT, runExcept, throwE)
import Control.Monad.Loops (iterateUntilM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts (IsList (..))
import HindleyMilner.Type (Type (..), typeCata)
import HindleyMilner.TypeError (TypeError (..))

-- |A type representing the known replacements between type variables and the their bound types
newtype Subst = Subst {unsubst :: Except TypeError (Map Identifier Type)}
	deriving newtype Eq

instance Show Subst where
	show = either show show . runExcept . unsubst

instance IsList Subst where
	type Item Subst = (Identifier, Type)
	fromList = Subst . return . fromList
	toList = either (const []) toList . runExcept . unsubst

lookupSubst :: Subst -> Identifier -> Maybe Type
lookupSubst (Subst s) t = case runExcept s of
	Right m -> m Map.!? t
	_ -> Nothing

(!?) :: Subst -> Identifier -> Maybe Type
(!?) = lookupSubst

insert :: Identifier -> Type -> Subst -> Subst
insert tvar t (Subst s) = Subst $ Map.insert tvar t <$> s

coalesceTypeError :: Except TypeError Subst -> Subst
coalesceTypeError = either (Subst . throwE) id . runExcept

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

instance Semigroup Subst where
	s1 <> s2 = coalesceTypeError $ Map.foldrWithKey combine s1 <$> (unsubst s2 >>= apply s1)

combine :: Identifier -> Type -> Subst -> Subst
combine tvar t subst = maybe
	(insert tvar t subst)
	((<>) subst . coalesceTypeError . unify t)
	$ subst `lookupSubst` tvar

instance Monoid Subst where
	mempty = Subst $ return Map.empty

-- |Attempts to create a substitution between the given identifier
-- and type, failing if the identifier occurs in the type.
bind :: Monad m => Identifier -> Type -> ExceptT TypeError m Subst
bind a t = if occurs a t
	then throwE $ InfiniteType a t
	else pure $ single a t

-- |Attempt to unify the given types. Fails if the given types are not alpha equivalent
unify :: Monad m => Type -> Type -> ExceptT TypeError m Subst
unify (l1 `Arrow` r1) (l2 `Arrow` r2) = liftA2 (<>) (unify l1 l2) (unify r1 r2)
unify (Variable a) (Variable b) | a <= b = return $ single b (Variable a)
	-- | otherwise = return $ single a (Variable b)
unify (Variable a) t = bind a t
unify t (Variable a) = bind a t
unify (Constructor a) (Constructor b) | a == b = return mempty
unify t1 t2 = throwE $ UnificationFail t1 t2

repeatedlyApply :: Substitutable a => (Subst -> a -> Except TypeError (Bool, a)) -> Subst -> a -> Except TypeError a
repeatedlyApply f s = fmap snd . iterateUntilM fst (f s . snd) . (False,)

-- |Instances of this class have potentially nested subexpressions, this helps us generically traverse them
class Substitutable a where
	-- |Apply a substitution to a type, potentially recursively
	apply :: Substitutable a => Subst -> a -> Except TypeError a
	apply' :: Substitutable a => (Subst, a) -> Except TypeError a
	apply' = uncurry apply
	-- |Query for free variables, potentially recursively
	freeVars :: a -> Set Identifier
	-- |Query for occurence of specified free variable in expression
	occurs :: Substitutable a => Identifier -> a -> Bool
	occurs a = Set.member a . freeVars
	-- |Uses the given lookup function to update each type variable.
	-- The type variable `m` in the function signature is intended to hold errors, perhaps in Maybe or Either
	changeVariables :: Monad m => (Identifier -> m Identifier) -> a -> m a

instance Substitutable Type where
	apply = repeatedlyApply $ \s -> typeCata (variable s) constructor (liftA2 arrow)
		where
			-- A type variable can be substituted if in the map (base case)
			variable :: Subst -> Identifier -> Except TypeError (Bool, Type)
			variable s a = Map.findWithDefault (True, Variable a) a . fmap (False,) <$> unsubst s
			-- A type constructor contains nothing substitutable (base case)
			constructor :: Identifier -> Except TypeError (Bool, Type)
			constructor = return . (True,) . Constructor
			-- In function application the function and argument should both be substituted recursively
			arrow :: (Bool, Type) -> (Bool, Type) -> (Bool, Type)
			arrow (b1, t1) (b2, t2) = (b1 || b2, Arrow t1 t2)

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
	apply = (sequenceA .) . fmap . apply
	freeVars = foldr (Set.union . freeVars) Set.empty
	changeVariables = mapM . changeVariables