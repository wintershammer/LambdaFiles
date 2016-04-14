import DepLam
import Types
import Parser

base = Var "Base"
nid  = Lam "x" base (Var "x") -- lx.x
plid =  Lam "a" (Kind Star) (Lam "x" (Var "a") (Var "x")) -- la:*.lx:a.x
appla = App nid (Var "a")
applb  = App (App plid (Var "Base")) (Var "a") -- (la:*.lx:a.x) Base a 
-- ie you gotta feed it a type first and it returns the identity function for that type

myEnv = Env [("Base", Kind Star),("a", Var "Base"),("Bool", Kind Star),("Int", Kind Star)]

chka = tCheck myEnv appla
chkb = tCheck myEnv applb


parseAndCheck :: String -> TC Type
parseAndCheck input = 
  case (parseExpr input) of (Right express) -> tCheck myEnv express

parseAndRun input = 
  case (parseExpr input) of (Right express) -> whnf express
