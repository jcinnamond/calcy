module StackSpec (spec) where

import Stack
import Test.Hspec (Spec, describe, it, shouldBe)
import VMErrors (VMError (EmptyStack, InvalidStackPosition, NotEnoughElementsOnStack))

mkStack :: [Int] -> Stack
mkStack = foldl' (flip push) emptyStack

spec :: Spec
spec = do
  describe "push and pop" $ do
    it "pushes elements onto the front" $ do
      let stack = mkStack [1, 2]
      pop stack `shouldBe` Right (2, Stack [1])

    it "returns an error when popping and empty stack" $ do
      pop emptyStack `shouldBe` Left EmptyStack

  describe "binop" $ do
    it "applies the operator to the top two elements and pushes the result" $ do
      let stack = mkStack [1, 2]
      binop (+) stack `shouldBe` Right (Stack [3])

    it "returns an error when there is one element on the stack" $ do
      let stack = push 1 emptyStack
      binop (+) stack `shouldBe` Left NotEnoughElementsOnStack

    it "returns an error when there are no elements on the stack" $ do
      binop (+) emptyStack `shouldBe` Left NotEnoughElementsOnStack

  describe "ret" $ do
    it "takes the top of the stack, drops the unwanted elements, pushes the return value" $ do
      let stack = mkStack [1, 2, 3, 4, 5, 6]
      ret 2 stack `shouldBe` Right (2, Stack [1])

    it "returns an error when the stack is too small" $ do
      let stack = mkStack [1, 2, 3]
      ret 7 stack `shouldBe` Left InvalidStackPosition

    it "returns an error when the stack is empty" $ do
      ret 0 emptyStack `shouldBe` Left EmptyStack
