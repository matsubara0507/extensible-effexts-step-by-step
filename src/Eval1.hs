{-# LANGUAGE DataKinds #-}

module Eval1 where

import           Data.Extensible.Effect (Eff, leaveEff)
import qualified Data.Map               as Map
import           Data.Maybe             (fromJust)
import           Exp

type Eval1 a = Eff '[] a

runEval1 :: Eval1 a -> a
runEval1 ev = leaveEff ev

-- |
-- >>> runEval1 $ eval1 Map.empty exampleExp
-- IntVal 18
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)       = return $ IntVal i
eval1 env (Var n)       =
    maybe (fail ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2)  = do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal (i1 + i2)
eval1 env (Abs n e)     = return $ FunVal env n e
eval1 env (App e1 e2)   = do
    val1 <- eval1 env e1
    val2 <- eval1 env e2
    case val1 of
      FunVal env' n body -> eval1 (Map.insert n val2 env') body
