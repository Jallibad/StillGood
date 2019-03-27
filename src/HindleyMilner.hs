module HindleyMilner where

import AST.Identifier
import AST.Types
import Control.Applicative (liftA2)
import Control.Arrow (second)
import Control.Monad.Except
import Data.Functor.Foldable
import qualified Data.Map.Strict as Map
import HindleyMilner.Type
import HindleyMilner.Substitution
import HindleyMilner.Infer

extend :: Environment -> (Identifier, Scheme) -> Environment
extend env (x, s) = Map.insert x s env

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

-- can I return a typed expression instead??
infer :: Environment -> Expression -> Infer (Subst, Type)
infer env = \case
	BuiltIn _ -> pure ([], typeInt) -- for now, BuiltIn could be other types though
	AST.Types.Variable x -> lookupEnv env x
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

inferType :: Environment -> Expression -> Infer (Subst, ExpressionWithType)
inferType env = cata $ \case
	(BuiltInF x) -> pure ([], ExpressionWithType (BuiltInF x) typeInt)
	(AST.Types.VariableF x) -> second (ExpressionWithType $ AST.Types.VariableF x) <$> lookupEnv env x
	(LambdaF argument body) -> do
		(s, body') <- body
		outerType <- flip Arrow (HindleyMilner.Infer.annotation body') <$> fresh
		pure (s, ExpressionWithType (LambdaF argument body') outerType)
	(ApplicationF function body) -> do
		(s1, function') <- function
		(s2, body') <- body
		case HindleyMilner.Infer.annotation function' of
			(Arrow t1 t2) -> do
				s3 <- unify t1 $ HindleyMilner.Infer.annotation body'
				pure (s3 `compose` s2 `compose` s1, ExpressionWithType (ApplicationF function' body') t2)
			_ -> throwError TooManyArguments
	(ExplicitTypeF _ _) -> undefined

-- try to type and expression, return explicit type
inferExplicitType :: Environment -> Expression -> Infer (Subst, Type, Expression)
inferExplicitType env e = case e of
	BuiltIn c -> pure ([], typeInt, ExplicitType typeInt (BuiltIn c)) -- what if stored in the state?
	AST.Types.Variable x -> do
		(s, t) <- lookupEnv env x -- how should I deal with error?
		pure (s, t, ExplicitType t (AST.Types.Variable x))
	Lambda argument body -> do
		tv <- fresh
		let env' = env `extend` (argument, Forall [] tv)
		(s, t, b) <- inferExplicitType env' body
		-- (args, argt) <- (lookupEnv env argument)
		let returnType = apply s tv `Arrow` t
		pure (s, returnType, ExplicitType returnType (Lambda argument b)) -- I need to figute out argument
	Application function body -> do
		tv <- fresh
		(s1, t1, f) <- inferExplicitType env function
		(s2, t2, b) <- inferExplicitType (apply s1 env) body
		s3 <- unify (apply s2 t1) (Arrow t2 tv)
		pure (s3 `compose` s2 `compose` s1, apply s3 tv, ExplicitType (apply s3 tv)  (Application f b))
	_ -> undefined

lookupEnv :: Environment -> Identifier -> Infer (Subst, Type)
lookupEnv env x = case Map.lookup x env of
	Nothing -> throwError $ UnboundVariable x
	Just s -> do
		t <- instantiate s
		pure (nullSubst, t)
