{-# LANGUAGE TemplateHaskell #-}

module Interpreter.RuntimeExpression where

import AST.Expression (ExpressionF)
import qualified AST.Expression as AST
import AST.Identifier (Identifier)
import Control.Concurrent.STM.TVar
import Control.Monad.Reader-- (ReaderT, asks, liftM2)
import Control.Monad.STM
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Char (isDigit)
import Data.Function (on)
import Data.Functor.Foldable-- (embed, para)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import HindleyMilner (TypeError (..))
import HindleyMilner.Type (Type (Arrow, Constructor), typeInt)
import HindleyMilner.TypedExp (TypedExp (..))

newtype Thunk = Thunk (TVar RExpr)
instance Show Thunk where
	show = const "Thunk"

data RExpr
	= Variable {identifier :: Identifier, annotation :: Type}
	| Suspension {thunk :: Thunk, annotation :: Type}
	| Lambda {argument :: Identifier, argType :: Type, body :: RExpr}
	| Application {function :: RExpr, arg :: RExpr}
	| BuiltIn {contents :: String, annotation :: Type}
	| NumericConstant Integer
	deriving (Show)

makeBaseFunctor ''RExpr

typedExpToRuntime :: Monad m => TypedExp -> ExceptT TypeError m RExpr
typedExpToRuntime (TypedExp e t) = toRuntime e t

toRuntime :: Monad m => ExpressionF TypedExp -> Type -> ExceptT TypeError m RExpr
toRuntime (AST.VariableF v) t = return $ Variable v t
toRuntime (AST.LambdaF param body) (Arrow at _) = Lambda param at <$> typedExpToRuntime body
toRuntime (AST.LambdaF _ _) _ = throwE TooManyArguments
toRuntime (AST.ApplicationF f x) _ = on (liftM2 Application) typedExpToRuntime f x
toRuntime (AST.ExplicitTypeF _ _) _ = throwE Deprecated
toRuntime (AST.BuiltInF x) (Constructor "Int")
	| all isDigit x = return $ NumericConstant $ read x
	| otherwise = throwE $ InvalidBuiltIn x
toRuntime (AST.BuiltInF "+") t = return $ BuiltIn "+" t
toRuntime (AST.BuiltInF x) _ = throwE $ InvalidBuiltIn x

getType :: RExpr -> Type
getType = cata $ \case
	LambdaF _ argType body -> Arrow argType body
	ApplicationF function _ -> case function of
			Arrow _ t -> t
			_ -> error "Invalid Type (this should not happen)"
	NumericConstantF _ -> typeInt
	x -> annotationF x

replaceVar :: Identifier -> RExpr -> RExpr -> STM RExpr
replaceVar var val = para $ \case
	(VariableF var' _) | var == var' -> return val
	(VariableF v t) -> return $ Variable v t
	(SuspensionF _thunk _) -> undefined
	(LambdaF newVar t (orig, _)) | var == newVar -> return $ Lambda newVar t orig
	(LambdaF newVar t (_, replaced)) -> Lambda newVar t <$> replaced
	(ApplicationF (_, f) (_, x)) -> liftM2 Application f x
	(BuiltInF v t) -> return $ BuiltIn v t
	(NumericConstantF x) -> return $ NumericConstant x