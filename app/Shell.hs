{-# LANGUAGE DuplicateRecordFields #-}
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
import Data.Maybe (fromMaybe)
import Data.Text (Text, init, isSuffixOf, unpack)
import Data.Text.IO (getLine, hPutStrLn)
import Optics hiding (Empty)
import Optics.State.Operators ((%=), (.=))
import Parser (term)
import System.Directory (doesDirectoryExist)
import System.Exit
import System.IO (hFlush, stderr, stdin, stdout)
import System.Posix hiding (createPipe)
import System.Process
import Types
import Util
import Prelude hiding (getLine, init, log)

{-
TODO:
    - Reduce the amount of unpacks necessary by creating own versions.
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

resetShellTo :: Shell -> Env ()
resetShellTo = liftIO . changeWorkingDirectory . view currentDirectory

getVar :: String -> Env String
getVar str = gets (Map.findWithDefault "" str . _variables)

initShell :: Env ()
initShell = do
    dir <- liftIO getWorkingDirectory
    vars <- Map.fromList <$> liftIO getEnvironment
    put
        ( Shell
            { _exitCode = ExitSuccess
            , _currentDirectory = dir
            , _previousDirectory = dir
            , _variables = vars
            , _handles = defaultHandles
            , _exit = False
            }
        )

getExitCode :: Env ExitCode
getExitCode = gets _exitCode

setExitCode :: ExitCode -> Env ()
setExitCode code = exitCode %= const code

negateExitCode :: Env ()
negateExitCode = exitCode %= flipCode
  where
    flipCode ExitSuccess = ExitFailure 1
    flipCode _ = ExitSuccess

onSuccess :: a -> ExitCode -> Env a -> Env a
onSuccess _ ExitSuccess ma = ma
onSuccess a (ExitFailure _) _ = return a

onFailure :: a -> ExitCode -> Env a -> Env a
onFailure _ (ExitFailure _) ma = ma
onFailure a ExitSuccess _ = return a

log :: String -> Env ()
log s = tell [s]

interpret :: Term -> Env Handles
interpret = \case
    Empty -> return defaultHandles
    TSeq l r -> log "sequence" >> interpret l >> interpret r
    TOr l r -> do
        log "or"
        hs <- interpret l
        code <- getExitCode
        onFailure hs code (interpret r)
    TAnd l r -> do
        log "and"
        hs <- interpret l
        code <- getExitCode
        onSuccess hs code (interpret r)
    TPipe l r -> do
        log "pipe"
        (readEnd, writeEnd) <- liftIO createPipe
        handles %= (hstd_out .~ writeEnd)
        hs <- interpret l
        liftIO $ hClose writeEnd
        handles .= (hs & hstd_in .~ readEnd)
        void (interpret r)
        liftIO $ hClose readEnd
        handles .= defaultHandles
        return defaultHandles
    TBang bang -> do
        log "bang"
        hs <- interpret bang
        negateExitCode
        return hs
    TSub subshell -> do
        log "sub"
        sh <- get
        hs <- interpret subshell
        resetShellTo sh
        return hs
    TExternal command -> do
        log "external"
        external command
    TBuiltin command -> do
        log "builtin"
        builtin command
        return defaultHandles

external :: External -> Env Handles
external = \case
    External command as -> do
        hs <- use handles
        as' <- evalArgs as
        let cmd = proc (unpack command) as'
        mby <-
            catchIO $
                createProcess
                    ( cmd
                        { std_in = UseHandle hs._hstd_in
                        , std_out = UseHandle hs._hstd_out
                        , std_err = UseHandle hs._hstd_err
                        }
                    )
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

-- TODO: Not really compatible with pipes atm
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
