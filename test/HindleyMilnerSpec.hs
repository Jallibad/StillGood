module HindleyMilnerSpec where

import AST.Expression
import HindleyMilner
import HindleyMilner.Type (typeInt)
import qualified HindleyMilner.Type as T
import HindleyMilner.TypedExp
import Test.Hspec

spec :: Spec
spec = describe "HindleyMilner" $
		it "adds type information to expressions" $ do
			(annotation <$> addType mempty (BuiltIn "1")) `shouldBe` Right (T.Constructor "Int")
			(annotation <$> addType mempty (Lambda "x" ((BuiltIn "+" `Application` Variable "x") `Application` BuiltIn "1"))) `shouldBe` Right (T.Arrow typeInt typeInt)