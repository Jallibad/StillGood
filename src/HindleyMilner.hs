module HindleyMilner
	( addType
	, test
	) where

import AST.Expression (Expression, ExpressionF (..))
import Control.Monad.Except
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Functor.Foldable (cata)
import HindleyMilner.Environment (Environment, (!?))
import HindleyMilner.Infer
import HindleyMilner.Scheme (instantiate)
import HindleyMilner.Substitution (Subst, apply')
import HindleyMilner.Type (Type (..), typeInt)
import HindleyMilner.TypeError (TypeError (..))

inferType :: Environment -> Expression -> Infer (Subst, ExpressionWithType)
inferType env = cata $ \case
	-- If we see a builtin add a hardcoded type to it
	-- TODO Support type inference on other builtins (needed for operator parsing)
	(BuiltInF x) -> pure (mempty, ExpressionWithType (BuiltInF x) typeInt)

	-- If we see a variable look it up from our environment
	-- (VariableF x) -> second (ExpressionWithType $ VariableF x) <$> lookupEnv env x
	(VariableF x) -> maybe
		-- If the variable isn't found throw an error
		(throwError $ UnboundVariable x)
		-- If the variable is found put a null substitution in and 
		((mempty,) <.> ExpressionWithType (VariableF x) <.> flip instantiate fresh)
		-- Lookup happens here
		(env !? x)

	-- If we see a lambda make a new type "freshVar -> bodyType"
	-- where bodyType is the inferred type of the body of the lambda
	-- TODO Add the argument to the current list of environment variables. Or maybe change
	-- how Environment works to allow demanding for future binding of free variables?
	(LambdaF argument body) -> do
		(s, body') <- body
		outerType <- flip Arrow (annotation body') <$> fresh
		pure (s, ExpressionWithType (LambdaF argument body') outerType)

	-- If we see a function application infer the types, then unify the function's
	-- argument type and the body type, and infer the function's return type
	(ApplicationF function body) -> do
		(s1, function') <- function
		(s2, body') <- body
		case annotation function' of
			(Arrow t1 t2) -> do
				s3 <- unify t1 $ annotation body'
				pure (s3 <> s2 <> s1, ExpressionWithType (ApplicationF function' body') t2)
			_ -> throwError TooManyArguments

	-- ExplicitType is deprecated and should soon be eliminated, don't bother handling
	(ExplicitTypeF _ _) -> undefined

infixr 9 <.>
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<.>) = (.) . fmap

addType :: Environment -> Expression -> Either TypeError ExpressionWithType
addType env = apply' <.> unwrapInfer . inferType env

test :: Expression -> IO ()
test = either print (BS.putStrLn . encode) . addType mempty

-- testExp :: Expression
-- testExp = Application (Lambda "x" $ BuiltIn "4") $ BuiltIn "4"