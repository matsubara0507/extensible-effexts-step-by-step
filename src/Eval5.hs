{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Eval5 where

import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.Reader           (MonadReader (..))
import           Control.Monad.State            (MonadState (..))
import           Control.Monad.Writer           (MonadWriter (..))
import           Data.Extensible.Effect         (Eff, leaveEff)
import           Data.Extensible.Effect.Default (EitherDef, ReaderDef, StateDef,
                                                 WriterDef, runEitherDef,
                                                 runReaderDef, runStateDef,
                                                 runWriterDef)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import           Exp

type Eval5 a =
    Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer ] a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev =
    leaveEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env $ ev

-- |
-- >>> runEval5 Map.empty 0 $ eval5 exampleExp
-- ((Right (IntVal 18),["x"]),8)
eval5 :: Exp -> Eval5 Value
eval5 (Lit i)       = do
    tick
    return $ IntVal i
eval5 (Var n)       = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval5 (Plus e1 e2)  = do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval5 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval5 (App e1 e2)   = do
    tick
    val1 <- eval5 e1
    val2 <- eval5 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval5 body)
        _                  -> throwError "type error in application"

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)
