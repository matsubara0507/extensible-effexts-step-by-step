{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE ExplicitNamespaces #-}
-- {-# LANGUAGE TypeOperators      #-}

module Eval2 where

import           Control.Monad.Except           (MonadError (..))
import           Data.Extensible.Effect         (Eff, leaveEff)
import           Data.Extensible.Effect.Default (EitherDef, runEitherDef)
-- import           Data.Extensible.Internal       (type (>:))
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
-- import           Data.Proxy
import           Exp

-- type Eval2 a = Eff '[ "Either" >: Either String ] a
-- type Eval2 a = Eff '[ "Either" >: EitherEff String ] a
type Eval2 a = Eff '[ EitherDef String ] a

-- throwError :: String -> Eval2 a
-- throwError err = liftEff (Proxy :: Proxy "Either") $ Left err
-- throwError err = throwEff (Proxy :: Proxy "Either") err

runEval2 :: Eval2 a -> Either String a
-- runEval2 ev = retractEff ev
-- runEval2 ev = leaveEff $ runEitherEff ev
runEval2 ev = leaveEff $ runEitherDef ev

-- |
-- >>> runEval2 $ eval2 Map.empty exampleExp
-- Right (IntVal 18)
-- >>> runEval2 $ eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))
-- Left "type error in addition"
-- >>> runEval2 $ eval2 Map.empty (Var "x")
-- Left "unbound variable: x"
eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)       = return $ IntVal i
eval2 env (Var n)       =
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval2 env (Plus e1 e2)  = do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval2 env (Abs n e)     = return $ FunVal env n e
eval2 env (App e1 e2)   = do
    val1 <- eval2 env e1
    val2 <- eval2 env e2
    case val1 of
        FunVal env' n body -> eval2 (Map.insert n val2 env') body
        _                  -> throwError "type error in application"
