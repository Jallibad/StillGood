module HindleyMilner.Infer where

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type
import Types

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

initUnique :: Unique
initUnique = Unique 0

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
	Left err -> Left err
	Right res -> Right $ closeOver res

closeOver :: (Map Identifier Type, Type) -> Scheme
closeOver = normalize . generalize empty . uncurry apply

-- |Add a `forall` to the type, binding the variables that we don't currently have substitutions for
generalize :: Environment -> Type -> Scheme
generalize env t = Forall as t
	where as = Set.toList $ freeVars t `Set.difference` freeVars env

ord :: Type -> [(Identifier, Identifier)]
ord body = zip (Set.toList $ freeVars body) (fmap Identifier letters)

normtype :: [(Identifier, Identifier)] -> Type -> Type
normtype l (a `Arrow` b) = Arrow (normtype l a) (normtype l b)
normtype _ (Constructor a) = Constructor a
normtype l (HindleyMilner.Type.Variable a) = case lookup a l of
	Just x -> HindleyMilner.Type.Variable x
	Nothing -> error "type variable not in signature"

-- |Reset the bound type variables (maybe we've bound ['a','d','z']) to our list of fresh variables ['a','b','c']
-- example: @normalize (Forall [Identifier "b"] $ HindleyMilner.Type.Variable $ Identifier "b")@
normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (snd <$> subst) (normtype subst body)
	where subst = ord body