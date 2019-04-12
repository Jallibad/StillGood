module HindleyMilner.Environment
	( Environment
	, (!?)
	) where

import AST.Identifier (Identifier)
import Data.Map.Strict (Map, (!?))
import HindleyMilner.Scheme (Scheme)

type Environment = Map Identifier Scheme