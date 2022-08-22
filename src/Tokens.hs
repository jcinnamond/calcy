module Tokens (CalcToken (..)) where

data CalcToken
  = TNum Int
  | TPlus
  | TSub
  | TMult
  deriving (Show, Eq)
