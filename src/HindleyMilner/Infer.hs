module HindleyMilner.Infer where

import AST.Identifier
import Control.Applicative (liftA2)
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Scheme
import HindleyMilner.Type

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

type TypeVarAssocList a = [(Identifier, a)]
type TypeVarLookupFunction = Identifier -> Either TypeError Identifier

initUnique :: Unique
initUnique = Unique 0

freshVars :: [Identifier]
freshVars = Identifier <$> ([1..] >>= flip replicateM ['a'..'z'])

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = join $ case flip evalState initUnique $ runExceptT m of
	Left err -> Left err
	Right res -> Right $ closeOver res

closeOver :: (Map Identifier Type, Type) -> Either TypeError Scheme
closeOver = normalize . generalize empty . uncurry apply

-- |Looks up a type variable in an associative list, returns a TypeError if not in the list
lookupF :: Identifier -> TypeVarAssocList a -> Either TypeError a
lookupF x = lookup x >>> \case
	Nothing	-> Left $ UnboundVariable x
	Just y	-> Right y

makeAssocList :: Substitutable a => a -> TypeVarAssocList Identifier
makeAssocList = flip zip freshVars . Set.toList . freeVars

makeMap :: Substitutable a => a -> Map Identifier Identifier
makeMap = Map.fromList . makeAssocList

boundVarsAndLookup :: Substitutable a => a -> ([Identifier], TypeVarLookupFunction)
boundVarsAndLookup = (fmap snd &&& flip lookupF) . makeAssocList

-- |Uses the given lookup function to update each type variable.
-- The type variable `f` in the function signature is intended to hold errors, perhaps in Maybe or Either
replaceVariables :: Applicative f => (Identifier -> f Identifier) -> Type -> f Type
replaceVariables f (a `Arrow` b) = liftA2 Arrow (replaceVariables f a) (replaceVariables f b)
replaceVariables _ (Constructor a) = pure $ Constructor a
replaceVariables f (Variable a) = Variable <$> f a

-- |Reconstructs a scheme given a type, a type variable list, and a type variable substitution lookup
reconstructScheme :: Applicative f => Type -> ([Identifier], Identifier -> f Identifier) -> f Scheme
reconstructScheme body = uncurry fmap . (Forall *** flip replaceVariables body)

-- |Reset the bound type variables (maybe we've bound ['a','d','z']) to our list of fresh variables ['a','b','c']
-- example: @normalize (Forall [Identifier "b"] $ Variable $ Identifier "b")@
normalize :: Scheme -> Either TypeError Scheme
normalize (Forall _ body) = reconstructScheme body $ boundVarsAndLookup body

fresh :: Infer Type
fresh = do
	s <- get
	put s{count = count s + 1}
	pure $ HindleyMilner.Type.Variable $ freshVars !! count s

-- f as = Map.fromList . zip as <$> mapM (const fresh) as

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
	s <- Map.fromList . zip as <$> mapM (const fresh) as
	pure $ apply s t