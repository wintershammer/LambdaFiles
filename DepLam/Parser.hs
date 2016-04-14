module Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex

import Lexer
import Types

-------------------------------------------------------------------------------
-- Expression
-------------------------------------------------------------------------------

variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  x <- identifier
  reservedOp ":"
  t <- expr
  reservedOp "."
  e <- expr
  return (Lam x t e)


term :: Parser Expr
term =  parens expr
    <|> variable
    <|> lambda

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 App es)


-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

parseExpr :: String -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input