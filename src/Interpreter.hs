module Interpreter where

import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad.Loops (iterateWhile)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.STM (atomically)
import Data.Functor (($>), void)
import Data.Map.Strict (insert)
import Interpreter.Input (Input (..), getInput, runExpressionReader)
import Interpreter.Program (Program)
import Interpreter.ReductionStrategy (normalOrder, runReduction)
import Interpreter.RuntimeExpression (getType)

handleInput :: Input -> ReaderT (TVar Program) IO Bool
handleInput (AddExp identifier expr) = do
	prog <- ask
	lift $ atomically $ modifyTVar' prog (insert identifier expr)
	return True
handleInput (PrintType expr) = do
	lift $ print $ getType expr
	return True
handleInput (RunExp expr) = do
	reduced <- runReduction $ normalOrder expr
	lift $ either
		print
		print
		reduced
	return True
handleInput Quit = return False

interpreter :: IO ()
interpreter = do
	prog <- newTVarIO mempty
	void $ iterateWhile id $ do
		input <- getLine
		currProg <- readTVarIO prog
		either
			(($> True) . print)
			(flip runReaderT prog . handleInput)
			(runExpressionReader getInput input currProg)