module Interpreter.Program where

import AST.Identifier (Identifier)
import Control.Monad.Reader (ReaderT, asks, withReaderT)
import Control.Monad.STM (STM)
import Data.Map.Strict (Map)
import HindleyMilner.Environment (Environment)
import HindleyMilner.Scheme (makeScheme)
-- import HindleyMilner.TypedExp (TypedExp, annotation)
import Interpreter.RuntimeExpression

-- type Program = Map Identifier TypedExp
type Program = Map Identifier RExpr

toEnv :: Program -> Environment
toEnv = fmap $ makeScheme . getType

getEnv :: ReaderT Program STM Environment
getEnv = asks toEnv

withEnv :: ReaderT Environment m a -> ReaderT Program m a
withEnv = withReaderT toEnv