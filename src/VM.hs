{-# LANGUAGE NamedFieldPuns #-}

module VM
  ( run,
    runTrace,
    halt,
    push,
    call,
    ret,
    add,
    sub,
    mult,
    nonsense,
    initialCPU,
    CPU (..),
  )
where

import Control.Monad.Except (MonadError (catchError, throwError))
import Data.Vector.Unboxed (Vector, (!))
import GHC.Base (until)
import Relude.Unsafe (head, tail)
import qualified Stack as S
import Tokens (CalcToken)
import VMErrors (VMError (UnexpectedInstruction, UnrecognisedInstruction))
import Prelude hiding (head, tail)

data CPU = CPU
  { cpuIP :: Int,
    cpuSP :: Int,
    cpuStack :: S.Stack,
    cpuError :: Maybe VMError,
    cpuHalted :: Bool
  }
  deriving (Show)

type Program = (Vector Word8)

type Instruction = Word8

halt, push, call, ret, add, sub, mult, nonsense :: Instruction
halt = 0x00
push = 0x01
call = 0x02
ret = 0x03
add = 0x04
sub = 0x05
mult = 0x06
nonsense = 0xff

-- trace = 0xff

initialCPU :: CPU
initialCPU =
  CPU
    { cpuIP = 0,
      cpuSP = 0,
      cpuStack = S.emptyStack,
      cpuError = Nothing,
      cpuHalted = False
    }

runTrace :: Program -> IO String
runTrace p = go initialCPU
  where
    go :: CPU -> IO String
    go cpu@CPU {cpuHalted = True} = do
      prettyPrint cpu
      pure $ result cpu
    go cpu = do
      prettyPrint cpu
      let cpu' = step p cpu
      go cpu'

    prettyPrint :: CPU -> IO ()
    prettyPrint CPU {cpuStack, cpuIP, cpuSP} =
      print cpuStack >> print cpuSP >> print cpuIP >> putStrLn ""

    result CPU {cpuError = Just err} = show err
    result CPU {cpuStack = s} = show $ S.pop s

run :: Program -> String
run p = result finalCPU
  where
    finalCPU = until (cpuHalted) (step p) initialCPU
    result CPU {cpuError = Just err} = show err
    result CPU {cpuStack = s} = stackTop
      where
        stackTop = case S.pop s of
          Left err -> show err
          Right (v, _) -> show v

step :: Program -> CPU -> CPU
step _ cpu@CPU {cpuError = Just _} = cpu
step p cpu@CPU {cpuIP} =
  case go of
    Left err -> cpu {cpuHalted = True, cpuError = Just err}
    Right c -> c
  where
    instruction = p ! cpuIP
    go
      | instruction == halt = stepHalt cpu
      | instruction == push = stepPush p cpu
      | instruction == call = stepCall p cpu
      | instruction == ret = stepRet cpu
      | instruction == add = stepOp (+) cpu
      | instruction == sub = stepOp (-) cpu
      | instruction == mult = stepOp (*) cpu
      | otherwise = throwError $ UnrecognisedInstruction instruction

stepHalt :: CPU -> Either VMError CPU
stepHalt cpu = Right cpu {cpuHalted = True}

stepPush :: Program -> CPU -> Either VMError CPU
stepPush p cpu@CPU {cpuIP, cpuStack} =
  pure
    cpu
      { cpuIP = cpuIP + 2,
        cpuStack = S.push v cpuStack
      }
  where
    v = fromIntegral $ p ! (cpuIP + 1)

stepCall :: Program -> CPU -> Either VMError CPU
stepCall p cpu@CPU {cpuIP, cpuStack} =
  Right
    cpu
      { cpuIP = callAddr,
        cpuSP = newSP,
        cpuStack = newStack
      }
  where
    callAddr = fromIntegral $ p ! (cpuIP + 1)
    newSP = S.position newStack
    retAddr = cpuIP + 2
    newStack = S.push retAddr cpuStack

stepRet :: CPU -> Either VMError CPU
stepRet cpu@CPU {cpuStack, cpuSP} = do
  (res, s') <- S.pop cpuStack
  (retAddr, s) <- S.ret cpuSP s'
  pure
    cpu
      { cpuStack = S.push res s,
        cpuIP = retAddr
      }

stepOp :: (Int -> Int -> Int) -> CPU -> Either VMError CPU
stepOp op cpu@CPU {cpuIP, cpuStack} = do
  s <- S.binop op cpuStack
  pure
    cpu
      { cpuIP = cpuIP + 1,
        cpuStack = s
      }
