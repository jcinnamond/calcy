module Calc (calc) where

import qualified Data.ByteString as T
import Parser (calcParser)
import qualified Parser as P
import Shunt (shunt)
import Text.Megaparsec (ParseErrorBundle)
import Tokens (CalcToken (..))
import VM (run)
import qualified VM

lower :: [CalcToken] -> [Word8]
lower ts = concatMap lower' ts ++ [VM.halt]
  where
    lower' :: CalcToken -> [Word8]
    lower' (TNum x) = [VM.push, fromIntegral x]
    lower' TPlus = [VM.add]
    lower' TSub = [VM.sub]
    lower' TMult = [VM.mult]

calc :: Text -> Either (ParseErrorBundle Text Void) String
calc ts = run . fromList . lower . P.lower <$> calcParser ts
