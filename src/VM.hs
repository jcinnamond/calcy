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
    load,
    param,
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
import VMErrors (VMError (EmptyStack, UnexpectedInstruction, UnrecognisedInstruction))
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

halt, push, call, ret, add, sub, mult, load, param, nonsense :: Instruction
halt = 0x00
push = 0x01
call = 0x02
ret = 0x03
add = 0x04
sub = 0x05
mult = 0x06
load = 0x07
param = 0x08
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
      print cpuStack >> print ("SP: " :: Text) >> print cpuSP >> print ("IP: " :: Text) >> print cpuIP >> putStrLn ""

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
  case go p cpu of
    Left err -> cpu {cpuHalted = True, cpuError = Just err}
    Right c -> c
  where
    instruction = p ! cpuIP
    go
      | instruction == halt = nullaryOp stepHalt
      | instruction == push = unaryOp stepPush
      | instruction == call = binaryOp stepCall
      | instruction == ret = nullaryOp stepRet
      | instruction == add = nullaryOp $ stepOp (+)
      | instruction == sub = nullaryOp $ stepOp (-)
      | instruction == mult = nullaryOp $ stepOp (*)
      | instruction == load = unaryOp stepLoad
      | instruction == param = unaryOp stepParam
      | otherwise = \_ _ -> throwError $ UnrecognisedInstruction instruction

nullaryOp :: (CPU -> Either VMError CPU) -> Program -> CPU -> Either VMError CPU
nullaryOp fn _ c@CPU {cpuIP} = fn $ c {cpuIP = cpuIP + 1}

unaryOp :: (Word8 -> CPU -> Either VMError CPU) -> Program -> CPU -> Either VMError CPU
unaryOp fn p cpu@CPU {cpuIP} = fn v newCpu
  where
    v = p ! (cpuIP + 1)
    newCpu = cpu {cpuIP = cpuIP + 2}

binaryOp :: (Word8 -> Word8 -> CPU -> Either VMError CPU) -> Program -> CPU -> Either VMError CPU
binaryOp fn p cpu@CPU {cpuIP} = fn v w newCpu
  where
    v = p ! (cpuIP + 1)
    w = p ! (cpuIP + 2)
    newCpu = cpu {cpuIP = cpuIP + 3}

stepHalt :: CPU -> Either VMError CPU
stepHalt cpu = Right cpu {cpuHalted = True}

stepPush :: Word8 -> CPU -> Either VMError CPU
stepPush v cpu@CPU {cpuStack} =
  pure cpu {cpuStack = S.push (fromIntegral v) cpuStack}

stepCall :: Word8 -> Word8 -> CPU -> Either VMError CPU
stepCall numParams callAddr cpu@CPU {cpuIP, cpuStack, cpuSP} = do
  (params, cpuStack') <- S.popN (fromIntegral numParams) cpuStack
  let newSP = S.position cpuStack'
  let retAddr = cpuIP
  let newStack = params <> S.push retAddr (S.push cpuSP cpuStack')
  Right
    cpu
      { cpuIP = fromIntegral callAddr,
        cpuSP = newSP,
        cpuStack = newStack
      }

stepRet :: CPU -> Either VMError CPU
stepRet cpu@CPU {cpuStack, cpuSP} = do
  (res, _) <- S.pop cpuStack
  newSP <- S.load cpuSP cpuStack
  newIP <- S.load (cpuSP + 1) cpuStack
  s <- S.jump cpuSP cpuStack
  pure
    cpu
      { cpuStack = S.push res s,
        cpuSP = newSP,
        cpuIP = newIP
      }

stepOp :: (Int -> Int -> Int) -> CPU -> Either VMError CPU
stepOp op cpu@CPU {cpuStack} = do
  s <- S.binop op cpuStack
  pure cpu {cpuStack = s}

stepLoad :: Word8 -> CPU -> Either VMError CPU
stepLoad addr cpu@CPU {cpuStack} = do
  v <- S.load (fromIntegral addr) cpuStack
  pure cpu {cpuStack = S.push v cpuStack}

stepParam :: Word8 -> CPU -> Either VMError CPU
stepParam addr cpu@CPU {cpuStack, cpuSP} = do
  v <- S.load a cpuStack
  pure cpu {cpuStack = S.push v cpuStack}
  where
    a = cpuSP + 2 + fromIntegral addr