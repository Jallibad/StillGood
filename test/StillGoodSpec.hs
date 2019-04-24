module Main (main) where

import AST.Parsable (parser)
import qualified AST.Parsable as AST (Parser)
import AST.Expression (Expression(..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "StillGood parser" $ do
    it "returns correct result" $
        runParser (parser :: AST.Parser Expression) "" "(print 1)" `shouldParse` Application {function = BuiltIn "print", body = BuiltIn "1"}

