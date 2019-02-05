module HindleyMilner.Infer where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Type
import Identifier

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

-- |Uses the given lookup function to replace each type variable with a corresponding one.
-- Intended to be specialized to Maybe
replaceVariables :: Applicative f => (Identifier -> f Identifier) -> Type -> f Type
replaceVariables f (a `Arrow` b) = liftA2 Arrow (replaceVariables f a) (replaceVariables f b)
replaceVariables _ (Constructor a) = pure $ Constructor a
replaceVariables f (Variable a) = Variable <$> f a

-- |Reset the bound type variables (maybe we've bound ['a','d','z']) to our list of fresh variables ['a','b','c']
-- example: @normalize (Forall [Identifier "b"] $ Variable $ Identifier "b")@
normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (snd <$> subst) replacement
	where
		subst = ord body
		Just replacement = replaceVariables (`lookup` subst) body