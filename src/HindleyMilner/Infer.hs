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
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT, throwE)
import Data.Functor.Identity (runIdentity)
import HindleyMilner.Scheme (Scheme, instantiate)
import HindleyMilner.Substitution (Subst, occurs, single)
import HindleyMilner.Type (Type (..))
import HindleyMilner.TypeError (TypeError (..))
import HindleyMilner.Utility (freshVars)

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

-- |Attempts to create a substitution between the given identifier
-- and type, failing if the identifier occurs in the type.
bind :: Identifier -> Type -> Infer Subst
bind a t = if occurs a t
	then throwE $ InfiniteType a t
	else pure $ single a t

-- |Attempt to unify the given types. Fails if the given types are not alpha equivalent
unify :: Type -> Type -> Infer Subst
unify (l1 `Arrow` r1) (l2 `Arrow` r2) = liftA2 (<>) (unify l1 l2) (unify r1 r2)
unify (Variable a) t = bind a t
unify t (Variable a) = bind a t
unify (Constructor a) (Constructor b) | a == b = pure mempty
unify t1 t2 = throwE $ UnificationFail t1 t2

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