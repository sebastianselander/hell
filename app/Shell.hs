{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Shell where

import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Text (Text, init, isSuffixOf, unpack)
import Data.Text.IO (getLine)
import GHC.IO.Handle.Text (hPutStrLn)
import Parser (term)
import System.Directory (doesDirectoryExist)
import System.Exit
import System.IO (hFlush, stderr, stdout)
import System.Posix
import Types
import Util
import Prelude hiding (getLine, init, log)

{-
TODO:
    - Reduce the amount of unpacks necessary by creating own versions.
    - Lenses
    - Other prelude?
    - Effect system?
-}

runShell :: Env a -> IO a
runShell =
    fmap fst
        . runWriterT
        . flip evalStateT (error "Shell not initialised")
        . runEnv
shell :: Env ()
shell = do
    initShell
    loop
  where
    loop = do
        prompt
        line <- shellGetInput
        case term line of
            Left e -> liftIO $ putStrLn e
            Right t -> interpret t
        loop

shellGetInput :: Env Text
shellGetInput = do
    liftIO $ continue =<< getLine
  where
    continue :: Text -> IO Text
    continue s = do
        if
            | "\\" `isSuffixOf` s -> do
                putStr "> " >> hFlush stdout
                s' <- getLine
                continue (init s <> s')
            | "&&" `isSuffixOf` s || "|" `isSuffixOf` s -> do
                putStr "> " >> hFlush stdout
                s' <- getLine
                continue (s <> s')
            | otherwise -> return s

prompt :: Env ()
prompt = do
    getExitCode >>= \case
        ExitSuccess -> setGreen
        ExitFailure _ -> setRed
    liftIO $ putStr "\n$ "
    setReset
    liftIO $ hFlush stdout

resetTo :: Shell -> Env ()
resetTo sh = liftIO $ changeWorkingDirectory sh.currentDirectory

getVar :: String -> Env String
getVar str = gets (Map.findWithDefault "" str . variables)

initShell :: Env ()
initShell = do
    dir <- liftIO getWorkingDirectory
    variables <- Map.fromList <$> liftIO getEnvironment
    put
        ( Shell
            { exitCode = ExitSuccess
            , currentDirectory = dir
            , previousDirectory = dir
            , variables = variables
            }
        )

getExitCode :: Env ExitCode
getExitCode = gets exitCode

setExitCode :: ExitCode -> Env ()
setExitCode code = modify (\sh -> sh{exitCode = code})

negateExitCode :: Env ()
negateExitCode = modify (\sh -> sh{exitCode = flipCode sh.exitCode})
  where
    flipCode ExitSuccess = ExitFailure 1
    flipCode _ = ExitSuccess

onSuccess :: ExitCode -> Env () -> Env ()
onSuccess ExitSuccess ma = ma
onSuccess (ExitFailure _) _ = return ()

onFailure :: ExitCode -> Env () -> Env ()
onFailure (ExitFailure _) ma = ma
onFailure ExitSuccess _ = return ()

log :: String -> Env ()
log s = tell [s]

interpret :: Term -> Env ()
interpret = \case
    Empty -> return ()
    TSeq l r -> log "sequence" >> interpret l >> interpret r
    TOr l r -> do
        interpret l
        log "or"
        code <- getExitCode
        onFailure code (interpret r)
    TAnd l r -> do
        log "and"
        interpret l
        code <- getExitCode
        onSuccess code (interpret r)
    TPipe l r -> TODO
        
    TBang l -> log "bang" >> interpret l >> negateExitCode
    TSub l -> do
        st <- get
        log "sub"
        interpret l
        resetTo st
    TExternal command -> log "external" >> external command
    TBuiltin command -> log "builtin" >> builtin command

external :: External -> Env ()
external = \case
    TCommand command as -> do
        as' <- args as
        vars <- gets variables
        let run = executeFile (unpack command) True as' (Just (Map.toList vars))
        pid <- liftIO $ forkProcess run
        status <- wait pid
        case status of
            Exited code -> setExitCode code
            Terminated sig _codeDumped ->
                err $ "child: terminated with " <> show sig
            Stopped sig -> err $ "child: stopped with " <> show sig

wait :: ProcessID -> Env ProcessStatus
wait pid = do
    status <- liftIO $ getProcessStatus False True pid
    maybe (wait pid) return status

args :: [Arg] -> Env [String]
args = mapM eval
  where
    eval :: Arg -> Env String
    eval (AIdent str) = return (unpack str)
    eval (ASub term) = TODO

builtin :: Builtin -> Env ()
builtin = \case
    TCd [] -> getVar "HOME" >>= success . changeWorkingDirectory
    TCd [ASub _] -> TODO
    TCd [AIdent arg] -> do
        liftIO (doesDirectoryExist (unpack arg)) >>= \case
            False -> err "cd: No such file or directory"
            True -> success $ changeWorkingDirectory (unpack arg)
    TCd ((_ : _)) -> err "cd: too many arguments"
    TPwd args
        | isEmpty args -> success (getWorkingDirectory >>= putStrLn)
        | otherwise -> err "pwd: too many arguments"
    TExit args
        | isEmpty args -> exitShell
        | otherwise -> err "exit: too many arguments"

exitShell :: Env ()
exitShell = liftIO exitSuccess

success :: IO a -> Env a
success ma = do
    setExitCode ExitSuccess
    liftIO ma

class Error a where
    err :: String -> Env a

class IsEmpty a where
    isEmpty :: a -> Bool

instance (Monoid a) => Error a where
    err str = do
        setExitCode (ExitFailure 1)
        liftIO (hPutStrLn stderr str >> return mempty)

instance IsEmpty Term where
    isEmpty Empty = True
    isEmpty _ = False

instance IsEmpty [a] where
    isEmpty [] = True
    isEmpty _ = False
