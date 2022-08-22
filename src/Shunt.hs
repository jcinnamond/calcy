module Shunt (shunt) where

import Data.List (foldl)
import Data.Sequence
import Tokens (CalcToken (..))

shunt :: [CalcToken] -> [CalcToken]
shunt ts = toList $ shunt' ts [] Empty

type Stack = [CalcToken]

type Instructions = Seq CalcToken

shunt' :: [CalcToken] -> Stack -> Instructions -> Instructions
shunt' [] s i = popAll s i
shunt' (TNum x : ts) s i = shunt' ts s (i |> TNum x)
shunt' (op : ts) s i =
  let (s', i') = shuntOp op s i
   in shunt' ts s' i'

popAll :: Stack -> Instructions -> Instructions
popAll xs is = foldl (|>) is xs

shuntOp :: CalcToken -> Stack -> Instructions -> (Stack, Instructions)
shuntOp op [] i = ([op], i)
shuntOp op ops@(s : ss) i
  | op `lowerPrecedenceThan` s = shuntOp op ss (i |> s)
  | otherwise = (op : ops, i)

lowerPrecedenceThan :: CalcToken -> CalcToken -> Bool
lowerPrecedenceThan TPlus TMult = True
lowerPrecedenceThan _ _ = False