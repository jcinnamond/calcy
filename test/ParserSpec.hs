module ParserSpec (spec) where

import Parser (calcParser)
import qualified Parser as P
import Test.Hspec
import Test.Hspec.Megaparsec
import Tokens (CalcToken (..))

spec :: Spec
spec = do
  it "parses a simple equation" $ do
    calcParser "1 + 2" `shouldParse` P.Plus (P.Int 1) (P.Int 2)

  it "parses a more complex equation" $ do
    calcParser "1 + 2 * 3" `shouldParse` P.Plus (P.Int 1) (P.Mult (P.Int 2) (P.Int 3))

  it "parses parentheses" $ do
    calcParser "(1 + 2) * 3" `shouldParse` P.Mult (P.Plus (P.Int 1) (P.Int 2)) (P.Int 3)

  it "parses subtraction" $ do
    calcParser "2 - 1" `shouldParse` P.Sub (P.Int 2) (P.Int 1)

  it "rejects invalid input" $ do
    calcParser `shouldFailOn` "123 abc"
