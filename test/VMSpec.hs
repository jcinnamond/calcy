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

    it "just works" $ do
      let program = fromList [VM.call, 0, 4, VM.halt, VM.push, 5, VM.ret]
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

  describe "load" $ do
    it "copies values to the head of the stack" $ do
      let program = fromList [VM.push, 2, VM.push, 3, VM.push, 4, VM.push, 5, VM.load, 0, VM.load, 2, VM.add, VM.halt]
      VM.run program `shouldBe` "6"

  describe "functions" $ do
    it "passes values in and out" $ do
      -- fun add(x,y) = x+y
      -- 9+3
      -- add(2,5)
      let program = fromList [VM.push, 9, VM.push, 3, VM.add, VM.push, 5, VM.push, 2, VM.call, 2, 13, VM.halt, VM.param, 0, VM.param, 1, VM.add, VM.ret]
      VM.run program `shouldBe` "7"

    it "can call other functions" $ do
      -- fun a(x,y) = x+b(y)
      -- fun b(x) = x+2
      -- a(2,5)
      let program = fromList [VM.push, 2, VM.push, 5, VM.call, 2, 8, VM.halt, VM.param, 0, VM.param, 1, VM.call, 1, 17, VM.add, VM.ret, VM.param, 0, VM.push, 2, VM.add, VM.ret]
      VM.run program `shouldBe` "9"
