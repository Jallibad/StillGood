module Main (main) where

import AST.Expression
import HindleyMilner
import qualified HindleyMilner.Type as T
import HindleyMilner.TypedExp
import Test.Hspec

main :: IO ()
main = hspec $ do
	describe "HindleyMilner" $ do
		it "adds type information to expressions" $ do
			(annotation <$> (addType mempty (BuiltIn "1"))) `shouldBe` Right (T.Constructor "Int")