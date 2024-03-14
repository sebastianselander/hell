{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Shell where

import Control.Monad.State
import Control.Monad.Writer
import Parser
import Util
import Prelude hiding (log)

data ExitCode = EXIT_FAILURE | EXIT_SUCCESS
    deriving (Show)

data Shell = Shell
    { exitCode :: ExitCode
    , currentDirectory :: FilePath
    , previousDirectory :: FilePath
    }
    deriving (Show)

newtype Env a = Env {runEnv :: StateT Shell (WriterT [String] IO) a}
    deriving (Functor, Applicative, Monad, MonadState Shell, MonadWriter [String], MonadIO)

getExitCode :: Env ExitCode
getExitCode = gets exitCode

negateExitCode :: Env ()
negateExitCode = modify (\shell -> shell{exitCode = flipCode shell.exitCode})
  where
    flipCode EXIT_SUCCESS = EXIT_FAILURE
    flipCode _ = EXIT_SUCCESS

onSuccess :: ExitCode -> Env () -> Env ()
onSuccess EXIT_SUCCESS ma = ma
onSuccess EXIT_FAILURE _ = return ()

onFailure :: ExitCode -> Env () -> Env ()
onFailure EXIT_FAILURE ma = ma
onFailure EXIT_SUCCESS _ = return ()

log :: String -> Env ()
log s = tell [s]

interpret :: Term -> Env ()
interpret = \case
    Empty -> return ()
    TSeq l r -> interpret l >> log "sequence" >> interpret r
    TOr l r -> do
        interpret l
        log "or"
        code <- getExitCode
        onFailure code (interpret r)
    TAnd l r -> do
        interpret l
        log "and"
        code <- getExitCode
        onSuccess code (interpret r)
    TPipe l r -> TODO
    TBang l -> interpret l >> log "bang" >> negateExitCode
    TSub l -> do
        st <- get
        interpret l
        log "sub"
        put st
    TExternal external -> undefined
    TBuiltin builtin -> TODO

builtin :: Builtin -> Env ()
builtin = \case
    TCd args -> undefined
    TExit args | isEmpty args -> undefined
    THelp args -> undefined

class IsEmpty a where
    isEmpty :: a -> Bool

instance IsEmpty Term where
    isEmpty Empty = True
    isEmpty _ = False

instance IsEmpty Args where
    isEmpty (AList []) = True
    isEmpty _ = False
