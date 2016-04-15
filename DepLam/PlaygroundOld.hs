import DepLam
import Types
import Parser

base = Var "Base"
nid  = Lam "x" base (Var "x") -- lx.x
plid =  Lam "a" (Kind Star) (Lam "x" (Var "a") (Var "x")) -- la:*.lx:a.x
appla = App nid (Var "a")
applb  = App (App plid (Var "Base")) (Var "a") -- (la:*.lx:a.x) Base a 
-- ie you gotta feed it a type first and it returns the identity function for that type

myEnv = Env [("Base", Kind Star),("a", Var "Base"),("Bool", (Pi "a" (Kind Star) (Pi "x" (Var "a") (Pi "y" (Var "a") (Var "a"))))),("b", Var "Int"),("Int", Kind Star),("Eq", Kind Star)]

chka = tCheck myEnv appla
chkb = tCheck myEnv applb


parseAndCheck :: String -> TC Type
parseAndCheck input = 
  case (parseExpr input) of (Right express) -> tCheck myEnv express

parseAndRun input = 
  case (parseExpr input) of (Right express) -> whnf express


kek = (App (App (Lam "a" (Kind Star) (Lam "x" (Var "a") (Var "x"))) (Pi "a" (Kind Star) (Pi "x" (Var "a") (Var "a")))) (Lam "a" (Kind Star) (Lam "x" (Var "a") (Var "x"))))


eq = (Pi "a" (Kind Star) (Pi "x" (Var "a") (Pi "y" (Var "a") (Kind Star))))


eqfun = (Lam "a" (Kind Star) (Lam "x" (Var "a") (Lam "y" (Var "a") (Var "Eq"))))
constFunOne = (Lam "a" (Var "Base") (Var "a"))
constFunTwo = (App (App plid constType) constFunOne)
constType = (Pi "a" (Var "Base") (Var "Base"))

proof = (App (App (App eqfun constType) constFunOne) constFunTwo)

-- proof that \\x.x and id(\\x.x) are the same!

