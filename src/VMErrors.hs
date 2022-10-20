module VMErrors (VMError (..)) where

data VMError
  = UnrecognisedInstruction Word8
  | EmptyStack
  | NotEnoughElementsOnStack
  | InvalidStackPosition
  | UnexpectedInstruction
  deriving (Show, Eq)
