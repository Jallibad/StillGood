module HindleyMilner where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types
import HindleyMilner.Type
import HindleyMilner.Substitution
import HindleyMilner.Infer

extend :: Environment -> (Identifier, Scheme) -> Environment
extend env (x, s) = Map.insert x s env

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
	as' <- mapM (const fresh) as
	let s = Map.fromList $ zip as as'
	pure $ apply s t

occurs :: Substitutable a => Identifier -> a -> Bool
occurs a = Set.member a . freeVars

unify :: Type -> Type -> Infer Subst
unify (l1 `Arrow` r1) (l2 `Arrow` r2) = liftA2 compose (unify l1 l2) (unify r1 r2)
unify (HindleyMilner.Type.Variable a) t = bind a t
unify t (HindleyMilner.Type.Variable a) = bind a t
unify (Constructor a) (Constructor b) | a == b = pure nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: Identifier -> Type -> Infer Subst
bind a t
	| t == HindleyMilner.Type.Variable a = pure nullSubst
	| occurs a t = throwError $ InfiniteType a t
	| otherwise = pure $ Map.singleton a t	

fresh :: Infer Type
fresh = do
	s <- get
	put s{count = count s + 1}
	pure $ HindleyMilner.Type.Variable $ Identifier (letters !! count s)

infer :: Environment -> Expression -> Infer (Subst, Type)
infer env = \case
	Types.Variable x -> lookupEnv env x
	Lambda argument body -> do
		tv <- fresh
		let env' = env `extend` (argument, Forall [] tv)
		(s, t) <- infer env' body
		pure (s, apply s tv `Arrow` t)
	Application function body -> do
		tv <- fresh
		(s1, t1) <- infer env function
		(s2, t2) <- infer (apply s1 env) body
		s3 <- unify (apply s2 t1) (Arrow t2 tv)
		pure (s3 `compose` s2 `compose` s1, apply s3 tv)
	_ -> undefined

lookupEnv :: Environment -> Identifier -> Infer (Subst, Type)
lookupEnv env x = case Map.lookup x env of
	Nothing -> throwError $ UnboundVariable $ show x
	Just s -> do
		t <- instantiate s
		pure (nullSubst, t)