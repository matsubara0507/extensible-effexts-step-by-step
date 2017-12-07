{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Eval6 where

import           Control.Monad.Except           (MonadError (..))
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Reader           (MonadReader (..))
import           Control.Monad.State            (MonadState (..))
import           Control.Monad.Writer           (MonadWriter (..))
import           Data.Extensible.Effect         (Eff, retractEff)
import           Data.Extensible.Effect.Default (EitherDef, ReaderDef, StateDef,
                                                 WriterDef, runEitherDef,
                                                 runReaderDef, runStateDef,
                                                 runWriterDef)
import           Data.Extensible.Internal       (type (>:))
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust)
import           Exp

type Eval6 a =
    Eff '[ ReaderDef Env, EitherDef String, WriterDef [String], StateDef Integer, "IO" >: IO ] a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev =
    retractEff . flip runStateDef st . runWriterDef . runEitherDef . flip runReaderDef env $ ev

-- |
-- >>> runEval6 Map.empty 0 $ eval6 exampleExp
-- 12
-- 4
-- 2
-- ((Right (IntVal 18),["x"]),8)
eval6 :: Exp -> Eval6 Value
eval6 (Lit i)       = do
    tick
    liftIO $ print i
    return $ IntVal i
eval6 (Var n)       = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Nothing  -> throwError ("unbound variable: " `mappend` n)
        Just val -> return val
eval6 (Plus e1 e2)  = do
    tick
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
        (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
        _                      -> throwError "type error in addition"
eval6 (Abs n e)     = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App e1 e2)   = do
    tick
    val1 <- eval6 e1
    val2 <- eval6 e2
    case val1 of
        FunVal env' n body ->
            local (const (Map.insert n val2 env')) (eval6 body)
        _                  -> throwError "type error in application"

tick :: (Num s, MonadState s m) => m ()
tick = do
    st <- get
    put (st + 1)
