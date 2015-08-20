module Syntax where

import qualified Data.Map as Map


type Name = String

data Expr = 
		  Lit Integer
		  | Var Name
		  | Plus Expr Expr
		  | Minus Expr Expr
		  | Lam Name Expr
		  |	App Expr Expr
		  deriving (Show)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

data Value = IntVal Integer
		   | FunVal Env Name Expr
		   deriving(Show)

type Env = Map.Map Name Value