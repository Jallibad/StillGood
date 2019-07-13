module HindleyMilner.Infer
	( Infer
	, fresh
	, instantiateFresh
	, unwrapInfer
	) where

import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT)
import Data.Functor.Identity (runIdentity)
import HindleyMilner.Scheme (Scheme, instantiate)
import HindleyMilner.Type (Type (..))
import HindleyMilner.TypeError (TypeError (..))
import HindleyMilner.Utility (freshVars)

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

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
instantiateFresh = (exceptToExceptT =<<) . flip instantiate fresh
	where
		exceptToExceptT = mapExceptT $ return . runIdentity