module CalcySpec (spec) where

import Calc (calc)
import Test.Hspec (Spec, it, shouldBe)
import Text.Megaparsec.Error (errorBundlePretty)

spec :: Spec
spec = do
  it "can add numbers" $ do
    calc "1 + 2" `shouldBe` Right "3"

  it "can multiply and add numbers" $ do
    calc "2 * 2 + 9" `shouldBe` Right "13"

  it "can calculate an expression with parentheses" $ do
    calc "2 * (2 + 9)" `shouldBe` Right "22"

  it "can subtract numbers" $ do
    calc "9 - 5" `shouldBe` Right "4"

  it "rejects invalid input" $ do
    let res = calc "12 abc"
    isLeft res `shouldBe` True
