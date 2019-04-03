module HindleyMilner.Infer
	( ExpressionWithType (..)
	, Infer
	, fresh
	, unwrapInfer
	, instantiate
	, getExplicitState
	) where

import AST.Expression
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import HindleyMilner.ExpressionWithType
import HindleyMilner.Substitution
import HindleyMilner.Scheme
import HindleyMilner.Type
import HindleyMilner.TypeError
import HindleyMilner.Utility

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

initUnique :: Unique
initUnique = Unique 0

-- |Takes an Infer and runs it, possibly throwing a TypeError
unwrapInfer :: Infer a -> Either TypeError a
unwrapInfer = flip evalState initUnique . runExceptT

-- get Subst, Type, and an Expression using ExplicitType
getExplicitState :: Infer (Subst, Type, Expression) -> Either TypeError Expression
getExplicitState m = (\(_, _, e) -> e) <$> unwrapInfer m

fresh :: Infer Type
fresh = do
	s <- get
	put s{count = count s + 1}
	pure $ HindleyMilner.Type.Variable $ freshVars !! count s

-- f as = Map.fromList . zip as <$> mapM (const fresh) as

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = flip apply t . Map.fromList . zip as <$> mapM (const fresh) as