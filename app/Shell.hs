{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Shell where

import Control.Exception (IOException, catch)
import Control.Monad.State
import Control.Monad.Writer
import Data.Map qualified as Map
import Data.Text (Text, init, isSuffixOf, unpack)
import Data.Text.IO (getLine, hPutStrLn)
import Parser (term)
import System.Directory (doesDirectoryExist)
import System.Exit
import System.IO (hFlush, stderr, stdout, stdin)
import System.Posix
import System.Process
import Types
import Util
import Prelude hiding (getLine, init, log)
import Data.Maybe (fromMaybe)

{-
TODO:
    - Reduce the amount of unpacks necessary by creating own versions.
    - Lenses
    - Other prelude?
    - Effect system?
-}

defaultHandles :: Handles
defaultHandles = Handles stdin stdout stderr 

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
            Right t -> liftIO (print t) >> void (interpret t)
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

onSuccess :: ExitCode -> Env a -> Env ()
onSuccess ExitSuccess ma = void ma
onSuccess (ExitFailure _) _ = return ()

onFailure :: ExitCode -> Env a -> Env ()
onFailure (ExitFailure _) ma = void ma
onFailure ExitSuccess _ = return ()

log :: String -> Env ()
log s = tell [s]

interpret :: Term -> Env Handles
interpret = \case
    Empty -> return defaultHandles
    TSeq l r -> log "sequence" >> interpret l >> interpret r
    TOr l r -> do
        log "or"
        handles <- interpret l
        code <- getExitCode
        onFailure code (interpret r)
        return handles
    TAnd l r -> do
        log "and"
        handles <- interpret l
        code <- getExitCode
        onSuccess code (interpret r)
        return handles
    TPipe l r -> do
        TODO
    TBang l -> do
        log "bang" 
        handles <- interpret l 
        negateExitCode 
        return handles
    TSub l -> do
        st <- get
        log "sub"
        handles <- interpret l
        resetTo st
        return handles
    TExternal command -> log "external" >> external command
    TBuiltin command -> log "builtin" >> builtin command >> return defaultHandles

external :: External -> Env Handles
external = \case
    External command as -> do
        as' <- evalArgs as
        let cmd = proc (unpack command) as'
        mby <- catchIO $ createProcess cmd
        case mby of
            -- TODO: Make the type application unnecessary
            Nothing -> err @() (command <> ": command not found") >> return defaultHandles
            Just (in_handle, out_handle, err_handle, processHandle) -> do
                code <- liftIO $ waitForProcess processHandle
                setExitCode code
                return (Handles (fromMaybe stdin in_handle) (fromMaybe stdout out_handle) (fromMaybe stderr err_handle))

evalArgs :: [Arg] -> Env [String]
evalArgs = mapM eval
  where
    eval :: Arg -> Env String
    eval (AIdent str) = return (unpack str)
    eval (ASub _) = TODO

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

catchIO :: IO a -> Env (Maybe a)
catchIO ma = liftIO $ catch @IOException (Just <$> ma) (const (return Nothing))

success :: IO a -> Env a
success ma = do
    setExitCode ExitSuccess
    liftIO ma

class Error a where
    err :: Text -> Env a

class IsEmpty a where
    isEmpty :: a -> Bool

instance (Monoid a) => Error a where
    err str = do
        setExitCode (ExitFailure 1)
        liftIO (hPutStrLn stderr str >> return mempty)

instance IsEmpty [a] where
    isEmpty [] = True
    isEmpty _ = False
