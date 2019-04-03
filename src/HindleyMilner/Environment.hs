module HindleyMilner.Environment
	( Environment
	, empty
	) where

import AST.Identifier (Identifier)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HindleyMilner.Scheme

type Environment = Map Identifier Scheme

empty :: Environment
empty = Map.empty