module HindleyMilner where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Types
import HindleyMilner.Type
import HindleyMilner.Substitution

newtype Unique = Unique {count :: Int}

initUnique :: Unique
initUnique = Unique 0

extend :: Environment -> (Identifier, Scheme) -> Environment
extend (Environment env) (x, s) = Environment $ Map.insert x s env

type Infer a = ExceptT TypeError (State Unique) a

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
	Left err -> Left err
	Right res -> Right $ closeOver res

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = fmap (apply s1) s2 `Map.union` s1

empty :: Environment
empty = Environment Map.empty

closeOver :: (Map Identifier Type, Type) -> Scheme
closeOver (sub, ty) = normalize $ generalize empty $ apply sub ty

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
	as' <- mapM (const fresh) as
	let s = Map.fromList $ zip as as'
	pure $ apply s t

generalize :: Environment -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env

ord :: Type -> [(Identifier, Identifier)]
ord body = zip (Set.toList $ freeVars body) (fmap Identifier letters)

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (snd <$> ord body) (normtype body)
	where
		normtype :: Type -> Type
		normtype (a `Arrow` b) = Arrow (normtype a) (normtype b)
		normtype (Constructor a) = Constructor a
		normtype (HindleyMilner.Type.Variable a) = case lookup a (ord body) of
			Just x -> HindleyMilner.Type.Variable x
			Nothing -> error "type variable not in signature"

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
lookupEnv (Environment env) x = case Map.lookup x env of
	Nothing -> throwError $ UnboundVariable $ show x
	Just s -> do
		t <- instantiate s
		pure (nullSubst, t)