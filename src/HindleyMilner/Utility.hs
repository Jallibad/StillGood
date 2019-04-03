module HindleyMilner.Utility
	( freshVars
	, normalize
	) where

import AST.Identifier
import Control.Arrow ((&&&))
import Control.Monad.State (replicateM)
import qualified Data.Set as Set
import HindleyMilner.Scheme (Scheme (..), reconstructScheme)
import HindleyMilner.Substitution
import HindleyMilner.Type
import HindleyMilner.TypeError

type TypeVarAssocList a = [(Identifier, a)]
-- type TypeException = Either TypeError
type TypeVarLookupFunction a = Identifier -> Either TypeError a

-- |An infinite list of unique type variables
freshVars :: [Identifier]
freshVars = Identifier <$> ([1..] >>= flip replicateM ['a'..'z'])

makeAssocList :: Substitutable a => a -> TypeVarAssocList Identifier
makeAssocList = flip zip freshVars . Set.toList . freeVars

boundVarsAndLookup :: [(Identifier, a)] -> ([a], TypeVarLookupFunction a)
boundVarsAndLookup = fmap snd &&& flip lookupF

-- |Reset the bound type variables (maybe we've bound @['a','d','z']@) to our list of fresh variables @['a','b','c']@
-- example: @normalize (Forall [Identifier "b"] $ Variable $ Identifier "b")@
normalize :: Type -> Either TypeError Scheme
normalize = uncurry ($) . (reconstructScheme &&& boundVarsAndLookup . makeAssocList)

-- |Adds an UnboundVariable error with the corresponding Identifier if the second
-- argument is nothing, otherwise passes the value through the right
addError :: Identifier -> Maybe a -> Either TypeError a
addError = flip maybe Right . Left . UnboundVariable

-- |Looks up a type variable in an associative list, returns a TypeError if not in the list
lookupF :: Identifier -> TypeVarAssocList a -> Either TypeError a
lookupF = uncurry (.) . (addError &&& lookup)