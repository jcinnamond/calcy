module VMSpec (spec) where

import Test.Hspec (Spec, it, shouldBe)
import qualified VM

spec :: Spec
spec = do
  it "multiplies numbers" $ do
    let program = fromList [VM.push, 2, VM.push, 3, VM.mult, VM.halt]
    VM.run program `shouldBe` "6"

  it "calls functions" $ do
    let program = fromList [VM.push, 2, VM.push, 3, VM.mult, VM.call, 9, VM.add, VM.halt, VM.push, 1, VM.push, 2, VM.add, VM.ret]
    VM.run program `shouldBe` "9"
