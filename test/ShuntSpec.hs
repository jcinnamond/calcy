module ShuntSpec (spec) where

import Shunt (shunt)
import Test.Hspec
import Tokens (CalcToken (..))

spec :: Spec
spec = do
  it "converts the input to RPN" $ do
    shunt [TNum 1, TPlus, TNum 2] `shouldBe` [TNum 1, TNum 2, TPlus]

  it "handles multiple operators" $ do
    shunt [TNum 1, TPlus, TNum 2, TMult, TNum 3] `shouldBe` [TNum 1, TNum 2, TNum 3, TMult, TPlus]

  it "handles precedence" $ do
    shunt [TNum 1, TMult, TNum 2, TPlus, TNum 3] `shouldBe` [TNum 1, TNum 2, TMult, TNum 3, TPlus]
