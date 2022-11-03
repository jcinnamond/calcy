{-# LANGUAGE TupleSections #-}

module Stack
  ( Stack (..),
    emptyStack,
    push,
    pop,
    binop,
    ret,
    position,
    load,
  )
where

import Control.Monad.Except (MonadError (throwError))
import VMErrors (VMError (EmptyStack, InvalidStackPosition, NotEnoughElementsOnStack))

newtype Stack = Stack [Int] deriving (Show, Eq)

emptyStack :: Stack
emptyStack = Stack []

position :: Stack -> Int
position (Stack s) = length s

push :: Int -> Stack -> Stack
push x (Stack s) = Stack (x : s)

pop :: Stack -> Either VMError (Int, Stack)
pop (Stack []) = throwError EmptyStack
pop (Stack (x : xs)) = pure (x, Stack xs)

binop :: (Int -> Int -> Int) -> Stack -> Either VMError Stack
binop op (Stack (x : y : zs)) = Right $ Stack (result : zs)
  where
    result = y `op` x
binop _ _ = Left NotEnoughElementsOnStack

ret :: Int -> Stack -> Either VMError (Int, Stack)
ret p s = jump p s >>= pop

jump :: Int -> Stack -> Either VMError Stack
jump _ (Stack []) = Left EmptyStack
jump p (Stack s)
  | length s >= p = Right $ Stack truncatedStack
  | otherwise = Left InvalidStackPosition
  where
    truncatedStack = drop diff s
    diff = length s - p

load :: Int -> Stack -> Either VMError Int
load addr (Stack s) = case s !!? pos of
  Just v -> pure v
  Nothing -> throwError InvalidStackPosition
  where
    pos = length s - 1 - addr