module VMSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import qualified VM

spec :: Spec
spec = do
  describe "error handling" $ do
    it "errors on unrecognised instructions" $ do
      let program = fromList [VM.nonsense]
      VM.run program `shouldBe` "UnrecognisedInstruction 255"

  describe "ret" $ do
    it "errors if the stack is empty" $ do
      let program = fromList [VM.ret]
      VM.run program `shouldBe` "EmptyStack"

    it "errors if the stack does not contain a return address" $ do
      let program = fromList [VM.push, 2, VM.ret]
      VM.run program `shouldBe` "EmptyStack"

    it "just works" $ do
      let program = fromList [VM.call, 3, VM.halt, VM.push, 5, VM.ret]
      VM.run program `shouldBe` "5"

  describe "binary operations" $ do
    it "adds numbers" $ do
      let program = fromList [VM.push, 2, VM.push, 3, VM.add, VM.halt]
      VM.run program `shouldBe` "5"

    it "subtracts numbers" $ do
      let program = fromList [VM.push, 2, VM.push, 3, VM.sub, VM.halt]
      VM.run program `shouldBe` "-1"

    it "multiplies numbers" $ do
      let program = fromList [VM.push, 2, VM.push, 3, VM.mult, VM.halt]
      VM.run program `shouldBe` "6"
