module HindleyMilner.Infer
	( Infer
	, bind
	, fresh
	, instantiateFresh
	, unify
	, unwrapInfer
	) where

import AST.Identifier (Identifier)
import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans.Except (Except, ExceptT, mapExceptT, runExceptT, throwE)
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HindleyMilner.Scheme (Scheme, instantiate)
import HindleyMilner.Substitution
import HindleyMilner.Type (Type (..))
import HindleyMilner.TypeError (TypeError (..))
import HindleyMilner.Utility (freshVars)

import Debug.Trace

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

instance Semigroup Subst where
	-- s1 <> s2 = Subst $ unsubst s1 >>= ((unsubst s2 >>= apply s1) >>=) . combiningFunction
	(<>) = compose

compose s1 s2 = trace (mconcat ["Composing ", show s1, " and ", show s2]) $ compose' s1 s2

compose' :: Subst -> Subst -> Subst
compose' s1 s2 = thing (Subst $ unsubst s1 >>= apply s2) $ unsubst s2

thing :: Subst -> Except TypeError (Map Identifier Type) -> Subst
thing = (coalesceTypeError .) . fmap . Map.foldrWithKey combine

combine :: Identifier -> Type -> Subst -> Subst
combine tvar t subst = maybe
	(insert tvar t subst)
	(compose subst . coalesceTypeError . unify t)
	$ subst `lookupSubst` tvar

instance Monoid Subst where
	mempty = empty

-- |Attempts to create a substitution between the given identifier
-- and type, failing if the identifier occurs in the type.
bind :: Monad m => Identifier -> Type -> ExceptT TypeError m Subst
bind a t = if occurs a t
	then throwE $ InfiniteType a t
	else pure $ single a t

unify t1 t2 = trace (mconcat ["Unifying ", show t1, " and ", show t2]) $ unify' t1 t2

-- |Attempt to unify the given types. Fails if the given types are not alpha equivalent
unify' :: Monad m => Type -> Type -> ExceptT TypeError m Subst
unify' (l1 `Arrow` r1) (l2 `Arrow` r2) = liftA2 (<>) (unify l1 l2) (unify r1 r2)
unify' (Variable a) (Variable b) | a <= b = return $ single b (Variable a)
	-- | otherwise = return $ single a (Variable b)
unify' (Variable a) t = bind a t
unify' t (Variable a) = bind a t
unify' (Constructor a) (Constructor b) | a == b = return mempty
unify' t1 t2 = throwE $ UnificationFail t1 t2

initUnique :: Unique
initUnique = Unique 0

-- |Takes an Infer and runs it, possibly throwing a TypeError
unwrapInfer :: Infer a -> Either TypeError a
unwrapInfer = flip evalState initUnique . runExceptT

fresh :: Infer Type
fresh = do
	s <- get
	put s{count = count s + 1}
	pure $ Variable $ freshVars !! count s

instantiateFresh :: Scheme -> Infer Type
instantiateFresh = join . fmap exceptToExceptT . flip instantiate fresh
	where
		exceptToExceptT = mapExceptT $ return . runIdentity