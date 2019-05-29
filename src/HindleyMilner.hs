module HindleyMilner
	( TypedExp
	, TypeError (..)
	, addType
	, addType'
	, addTypeUnsafe
	, test
	) where

import Debug.Trace
import AST.Expression (Expression (..), ExpressionF (..))
import Control.Monad.Trans.Except
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Either (fromRight)
import Data.Functor.Foldable (para)
import HindleyMilner.Environment (Environment, (!?), addNewVar)
import HindleyMilner.Infer
import HindleyMilner.Scheme (instantiate)
import HindleyMilner.Substitution (Subst, apply')
import HindleyMilner.Type (Type (..), typeInt, typeIO)
import HindleyMilner.TypeError (TypeError (..))

inferType :: Environment -> Expression -> Infer (Subst, TypedExp)
inferType env = para $ \case
	-- If we see a builtin add a hardcoded type to it
	-- TODO Support type inference on other builtins (needed for operator parsing)
	(BuiltInF "print") -> return (mempty, TypedExp (BuiltInF "print") $ Arrow typeInt typeIO)
	(BuiltInF "+") -> return (mempty, TypedExp (BuiltInF "+") $ Arrow typeInt $ Arrow typeInt typeInt)
	(BuiltInF "seq") -> return (mempty, TypedExp (BuiltInF "seq") $ Arrow typeIO $ Arrow typeIO typeIO)
	(BuiltInF x) -> return (mempty, TypedExp (BuiltInF x) typeInt)

	-- If we see a variable look it up from our environment
	-- (VariableF x) -> second (TypedExp $ VariableF x) <$> lookupEnv env x
	(VariableF x) -> maybe
		-- If the variable isn't found throw an error
		-- TODO Add unbound variable with fresh type variable to substitution instead of throwing error
		(throwE $ UnboundVariable x)
		-- If the variable is found put a null substitution in and instantiate the type
		((mempty,) <.> TypedExp (VariableF x) <.> flip instantiate fresh)
		-- Lookup happens here
		(env !? x)

	-- If we see a lambda make a new type "freshVar -> bodyType"
	-- where bodyType is the inferred type of the body of the lambda
	-- TODO Add the argument to the current list of environment variables. Or maybe change
	-- how Environment works to allow demanding for future binding of free variables?
	(LambdaF argument (body, _)) -> do
		argType <- fresh
		(s, body') <- inferType (addNewVar argument argType env) body
		return (s, TypedExp (LambdaF argument body') (Arrow argType (annotation body')))

	-- If we see a function application infer the types, then unify the function's
	-- argument type and the body type, and infer the function's return type
	(ApplicationF (_, function) (_, body)) -> do
		(s1, function') <- function
		(s2, body') <- body
		case annotation function' of
			t@(HindleyMilner.Type.Variable _) -> do
				returnType <- fresh
				-- this case is needed to allow for infering the arguments to higher order functions
				-- TODO: however currently it supercedes the error that should be thrown when a
				-- function is passed to many arguments
				traceM "Application with type variable function"
				traceShowM t
				traceShowM returnType
				s3 <- unify t $ Arrow (annotation body') returnType
				return (s3 <> s2 <> s1, TypedExp (ApplicationF function' body') returnType)
			x@(Arrow t1 t2) -> do
				s3 <- unify t1 $ annotation body'
				traceM "Application with type arrow function"
				traceShowM x
				traceShowM $ annotation body'
				return (s3 <> s2 <> s1, TypedExp (ApplicationF function' body') t2)
			(Constructor _) -> throwE TooManyArguments

	-- ExplicitType is deprecated and should soon be eliminated, don't bother handling
	(ExplicitTypeF _ _) -> undefined

infixr 9 <.>
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) = (.) . fmap

addType :: Environment -> Expression -> Either TypeError TypedExp
addType env = apply' <.> unwrapInfer . inferType env

addType' :: Environment -> Expression -> Except TypeError TypedExp
addType' = (except .) . addType

addTypeUnsafe :: Environment -> Expression -> TypedExp
addTypeUnsafe = (fromRight (error "Type Error") .) . addType

test :: Expression -> IO ()
test = either print (BS.putStrLn . encode) . addType mempty

-- testExp :: Expression
-- testExp = Application (Lambda "x" $ BuiltIn "4") $ BuiltIn "4"