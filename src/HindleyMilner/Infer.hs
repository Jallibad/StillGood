module HindleyMilner.Infer
	( ExpressionWithType (..)
	, Infer
	, fresh
	, unwrapInfer
	, instantiate
	, runInfer
	, getExplicitState
	) where

import AST.Identifier
import AST.Types
import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HindleyMilner.Environment
import HindleyMilner.ExpressionWithType
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

unwrapInfer :: Infer a -> Either TypeError a
unwrapInfer = flip evalState initUnique . runExceptT

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = join $ case unwrapInfer m of
	Left err -> Left err
	Right res -> Right $ closeOver res

-- runInfer' :: Infer (Subst, (a, Type)) -> Either TypeError (a, Scheme)
-- breakInfer :: Infer a -> Either TypeError a
-- breakInfer m = case thing m of
-- 	Left err -> Left err


-- get Subst, Type, and an Expression using ExplicitType
getExplicitState :: Infer (Subst, Type, Expression) -> Either TypeError Expression
getExplicitState m = (\(_, _, e) -> e) <$> unwrapInfer m

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