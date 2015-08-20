
module Eval where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

import Data.Maybe
import qualified Data.Map as Map

import Parser
import Syntax


type Eval a = ReaderT Env (ErrorT String Identity) a

runEval :: Env -> Eval a -> Either String a
runEval env ev = runIdentity( runErrorT (runReaderT ev env))


eval :: Expr -> Eval Value



eval (Lit i)       =   return $ IntVal i


eval (Var n)       =  do 
					  env <- ask
					  case Map.lookup n env of
					  	Nothing -> throwError ("unbound variable")
					  	Just val -> return val					  	

eval (Lam n e)  = do
				  env <- ask
				  return $ FunVal env n e

eval (App e1 e2) = do
				   val1 <- eval e1
				   val2 <- eval e2
				   case val1 of
				   	FunVal env' n body ->
				   		local (const (Map.insert n val2 env'))
				   			(eval body)
				   	_   -> throwError "type Error in app"



eval (Plus e1 e2) = do 
					e1t <- eval e1
					e2t <- eval e2
					case (e1t,e2t) of
						(IntVal i1,IntVal i2) ->
							return $ IntVal (i1 + i2)
						_ -> throwError "typeError in addition"


eval (Minus e1 e2) = do 
					e1t <- eval e1
					e2t <- eval e2
					case (e1t,e2t) of
						(IntVal i1,IntVal i2) ->
							return $ IntVal (i1 - i2)
						_ -> throwError "typeError in sub"


exampleExp = Lit 12 ` Plus ` ( App ( Lam "x" ( Var "x" )) ( Lit 4 ` Plus ` Lit 2))


try = runEval Map.empty (eval exampleExp)