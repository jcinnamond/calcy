module Parser (calcParser, Expr (..), lower) where

import Control.Monad.Combinators.Expr (Operator (InfixL), makeExprParser)
import Text.Megaparsec (MonadParsec (eof), ParseErrorBundle, Parsec, between, choice, parse)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Tokens (CalcToken (..))
import Prelude hiding (many, some)

type Parser = Parsec Void Text

data Expr
  = Int Int
  | Plus Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  deriving (Show, Eq)

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "#")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      pInteger
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [binary "*" Mult],
    [ binary "+" Plus,
      binary "-" Sub
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

calcParser :: Text -> Either (ParseErrorBundle Text Void) Expr
calcParser = parse (pExpr <* eof) ""

lower :: Expr -> [CalcToken]
lower (Int x) = [TNum x]
lower (Plus l r) = lower l ++ lower r ++ [TPlus]
lower (Sub l r) = lower l ++ lower r ++ [TSub]
lower (Mult l r) = lower l ++ lower r ++ [TMult]