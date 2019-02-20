module HindleyMilner.Infer
	( Infer
	, fresh
	, instantiate
	, runInfer
	) where

import AST.Identifier
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HindleyMilner.Substitution
import HindleyMilner.Scheme
import HindleyMilner.Type

newtype Unique = Unique {count :: Int}

type Infer a = ExceptT TypeError (State Unique) a

type TypeVarLookupFunction = Identifier -> Either TypeError Identifier

initUnique :: Unique
initUnique = Unique 0

freshVars :: [Identifier]
freshVars = Identifier <$> ([1..] >>= flip replicateM ['a'..'z'])

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = join $ case flip evalState initUnique $ runExceptT m of
	Left err -> Left err
	Right res -> Right $ closeOver res

closeOver :: (Subst, Type) -> Either TypeError Scheme
closeOver = normalize . generalize empty . uncurry apply

makeAssocList :: Substitutable a => a -> TypeVarAssocList Identifier
makeAssocList = flip zip freshVars . Set.toList . freeVars

-- makeMap :: Substitutable a => a -> Map Identifier Identifier
-- makeMap = Map.fromList . makeAssocList

boundVarsAndLookup :: Substitutable a => a -> ([Identifier], TypeVarLookupFunction)
boundVarsAndLookup = (fmap snd &&& flip lookupF) . makeAssocList

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
instantiate (Forall as t) = flip apply t . Map.fromList . zip as <$> mapM (const fresh) as