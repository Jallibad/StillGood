module Interpreter.ReductionStrategy
	( ReductionContext
	, ReductionStrategy
	, normalOrder
	, runReduction
	) where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader-- (ReaderT, ask, lift)
import Control.Monad.STM-- (STM)
import Control.Monad.Trans.Except-- (ExceptT)
import Data.Map.Strict ((!), insert)
import HindleyMilner (TypeError (..))
import Interpreter.Program (Program)
import Interpreter.RuntimeExpression-- (RExpr (..), RExprF (..), Thunk)

newtype ReductionContext a = ReductionContext (ReaderT (TVar Program) (ExceptT TypeError STM) a)
	deriving newtype (Applicative, Functor, Monad, MonadReader (TVar Program))

runReduction :: ReductionContext a -> ReaderT (TVar Program) IO (Either TypeError a)
runReduction (ReductionContext r) = mapReaderT (atomically . runExceptT) r

stmToReduction :: STM a -> ReductionContext a
stmToReduction = ReductionContext . lift . lift

readProgram :: ReductionContext Program
readProgram = ask >>= stmToReduction . readTVar

modifyProgram :: (Program -> Program) -> ReductionContext ()
modifyProgram = (ask >>=) . (stmToReduction .) . flip modifyTVar'

type ReductionStrategy = RExpr -> ReductionContext RExpr

normalOrder :: ReductionStrategy
normalOrder (Variable x _) = do
	prog <- readProgram
	reduced <- normalOrder $ prog ! x
	modifyProgram $ insert x reduced
	return reduced
normalOrder (Application (Lambda param _ body) arg) =
	stmToReduction (replaceVar param arg body) >>= normalOrder
normalOrder (Application (Application (BuiltIn "+" _) (NumericConstant x)) (NumericConstant y)) =
	return $ NumericConstant $ x + y
normalOrder (Application func arg) =
	normalOrder func >>= normalOrder . flip Application arg
normalOrder x = return x