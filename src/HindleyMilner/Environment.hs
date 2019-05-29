module HindleyMilner.Environment
	( Environment
	, (!)
	, (!?)
	, addNewVar
	) where

import AST.Identifier (Identifier)
import Data.Map.Strict (Map, (!), (!?), insert)
import HindleyMilner.Scheme (Scheme (..))
import HindleyMilner.Type (Type)

type Environment = Map Identifier Scheme

addNewVar :: Identifier -> Type -> Environment -> Environment
addNewVar var = insert var . Forall mempty