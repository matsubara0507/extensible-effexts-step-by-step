{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Eval4 where

import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.Reader           (MonadReader (..))
import           Control.Monad.State            (MonadState (..))
import           Data.Extensible.Effect         (Eff, leaveEff)
import           Data.Extensible.Effect.Default (EitherDef, ReaderDef, StateDef,
                                                 runEitherDef, runReaderDef,
                                                 runStateDef)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import           Exp

type Eval4 a =
  Eff '[ ReaderDef Env, EitherDef String, StateDef Integer ] a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev =
  leaveEff . flip runStateDef st . runEitherDef . flip runReaderDef env $ ev

-- |
-- >>> runEval4 Map.empty 0 $ eval4 exampleExp
-- (Right (IntVal 18),8)
eval4 :: Exp -> Eval4 Value
eval4 (Lit i)       = do
    tick
    return $ IntVal i
eval4 (Var n)       = do
    tick
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval4 (Plus e1 e2)  = do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval4 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval4 (App e1 e2)   = do
    tick
    val1 <- eval4 e1
    val2 <- eval4 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval4 body)
        _                  -> throwError "type error in application"

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)
