{-# LANGUAGE NamedFieldPuns #-}

module VM (run, runTrace, halt, push, call, ret, add, sub, mult) where

import Data.Vector.Unboxed (Vector, (!))
import GHC.Base (until)
import Relude.Unsafe (head, tail)
import Tokens (CalcToken)
import Prelude hiding (head, tail)

data CPU = CPU
  { cpuIP :: Int,
    cpuSP :: Int,
    cpuStack :: [Int],
    cpuStackLength :: Int,
    cpuError :: Maybe String,
    cpuHalted :: Bool
  }

type Program = (Vector Word8)

type Instruction = Word8

halt, push, call, ret, add, sub, mult :: Instruction
halt = 0x00
push = 0x01
call = 0x02
ret = 0x03
add = 0x04
sub = 0x05
mult = 0x06

-- trace = 0xff

stackPush :: CPU -> Int -> CPU
stackPush cpu@CPU {cpuStack, cpuStackLength} x =
  cpu
    { cpuStack = x : cpuStack,
      cpuStackLength = succ cpuStackLength
    }

stackPop :: CPU -> (Int, CPU)
stackPop cpu@CPU {cpuStack, cpuStackLength} = (v, cpu')
  where
    v = head cpuStack
    cpu' =
      cpu
        { cpuStack = tail cpuStack,
          cpuStackLength = pred cpuStackLength
        }

initialCPU :: CPU
initialCPU =
  CPU
    { cpuIP = 0,
      cpuSP = 0,
      cpuStack = [],
      cpuStackLength = 0,
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

    result CPU {cpuError = Just err} = err
    result CPU {cpuStack = []} = "unexpected empty stack"
    result CPU {cpuStack = (x : _)} = show x

run :: Program -> String
run p = result finalCPU
  where
    finalCPU = until (cpuHalted) (step p) initialCPU
    result CPU {cpuError = Just err} = err
    result CPU {cpuStack = []} = "unexpected empty stack"
    result CPU {cpuStack = (x : _)} = show x

step :: Program -> CPU -> CPU
step _ cpu@CPU {cpuError = Just _} = cpu
step p cpu@CPU {cpuIP} = go
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
      | otherwise = cpu {cpuError = Just "unexpected instruction"}

stepHalt :: CPU -> CPU
stepHalt cpu = cpu {cpuHalted = True}

stepPush :: Program -> CPU -> CPU
stepPush p cpu@CPU {cpuIP} =
  (stackPush cpu (fromIntegral v)) {cpuIP = cpuIP + 2}
  where
    v = p ! (cpuIP + 1)

stepCall :: Program -> CPU -> CPU
stepCall p cpu@CPU {cpuIP, cpuStackLength} = cpu' {cpuIP = callAddr, cpuSP = newSP}
  where
    cpu' = stackPush cpu (cpuIP + 2)
    callAddr = fromIntegral $ p ! (cpuIP + 1)
    newSP = cpuStackLength

stepRet :: CPU -> CPU
stepRet cpu@CPU {cpuSP, cpuStack, cpuStackLength} =
  cpu
    { cpuIP = retAddr,
      cpuStack = newStack,
      cpuHalted = True
    }
  where
    retValue = head cpuStack
    outerFrame = drop frameSize cpuStack
    frameSize = cpuStackLength - cpuSP
    retAddr = head cpuStack
    newStack = retValue : tail outerFrame

stepOp :: (Int -> Int -> Int) -> CPU -> CPU
stepOp op cpu@CPU {cpuIP} =
  let (x, cpu') = stackPop cpu
   in let (y, cpu'') = stackPop cpu'
       in let cpu''' = stackPush cpu'' (op y x)
           in cpu''' {cpuIP = cpuIP + 1}
