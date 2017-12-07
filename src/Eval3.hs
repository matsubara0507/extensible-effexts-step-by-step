{-# LANGUAGE DataKinds #-}

module Eval3 where

import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.Reader           (MonadReader (..))
import           Data.Extensible.Effect         (Eff, leaveEff)
import           Data.Extensible.Effect.Default (EitherDef, ReaderDef,
                                                 runEitherDef, runReaderDef)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import           Exp

type Eval3 a =
  Eff '[ ReaderDef Env, EitherDef String ] a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = leaveEff . runEitherDef . flip runReaderDef env $ ev

-- |
-- >>> runEval3 Map.empty $ eval3 exampleExp
-- Right (IntVal 18)
eval3 :: Exp -> Eval3 Value
eval3 (Lit i)       = return $ IntVal i
eval3 (Var n)       = do
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval3 (Plus e1 e2)  = do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval3 (Abs n e)     = do
    env <- ask
    return $ FunVal env n e
eval3 (App e1 e2)   = do
    val1 <- eval3 e1
    val2 <- eval3 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval3 body)
        _                  -> throwError "type error in application"
