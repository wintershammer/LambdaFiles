import DepLam
import Types
import Parser


myEnv :: Env
myEnv = Env [("Bool", Kind Star), ("Boola", bool)]

parseAndCheck :: String -> TC Type
parseAndCheck input = 
  case (parseExpr input) of (Right express) -> tCheck myEnv express

parseAndRun input = 
  case (parseExpr input) of (Right express) -> whnf express


-- boolean type = (a :: *) -> a -> a -> a
bool = (Pi "a" (Kind Star) (Pi "x" (Var "a") (Pi "y" (Var "a") (Var "a")))) --bool type

--True = \ (boolT::*) (false::boolT) (true::boolT) -> true;

true  = (Lam "boolT" (Kind Star) (Lam "false" (Var "boolT") (Lam "true" (Var "boolT") (Var "true"))))

--False = \ (boolT::*) (false::boolT) (true::boolT) -> false;

false = (Lam "boolT" (Kind Star) (Lam "false" (Var "boolT") (Lam "true" (Var "boolT") (Var "false"))))


--if :: forall (a::*) . Bool -> a -> a -> a;
--if a b t f = b a f t;
--if true then a else b example: 
--whnf (App (App (App (App ift (Var "base")) true) (Var "a")) (Var "b"))
--                              ^^ gotta feed it a type first

ift =  (Lam "a" (Kind Star) (Lam "b" bool (Lam "t" (Var "a") (Lam "f" (Var "a") (App (App (App (Var "b") (Var "a")) (Var "f")) (Var "t"))))))


example =  tCheck myEnv (App (Lam "x" (Var "Bool") (Var "x")) true) -- typecheck :  (\x : Bool . x) true