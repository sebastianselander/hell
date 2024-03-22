{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Shell where

import Control.Exception (IOException, catch)
import Control.Monad.Reader (ask, asks, local, runReaderT)
import Control.Monad.State
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Map qualified as Map
import Data.Text (Text, init, isSuffixOf, pack, unpack)
import Data.Text.IO (getLine, hPutStrLn)
import Optics hiding (Empty)
import Optics.State.Operators ((%=))
import Parser (term)
import System.Directory (doesDirectoryExist)
import System.Directory.Internal (copyHandleData)
import System.Exit (ExitCode (..), exitSuccess)
import System.IO
    ( Handle,
      hClose,
      hFlush,
      hGetContents,
      hPutStr,
      stderr,
      stdin,
      stdout,
    )
import System.Posix qualified as Unix hiding (fdRead)
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

-- TODO: initializie stuff in main and pass as arguments
runShell :: Env a -> IO a
runShell =
    flip runReaderT defaultHandles
        . flip evalStateT (error "Shell not initialised")
        . runEnv

shell :: Env ()
shell = do
    initShell
    loop
  where
    loop = do
        b <- use exit
        if b
            then return ()
            else do
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

resetShellTo :: Shell -> Env ()
resetShellTo = liftIO . Unix.changeWorkingDirectory . view currentDirectory

getVar :: String -> Env String
getVar str = gets (Map.findWithDefault "" str . _variables)

initShell :: Env ()
initShell = do
    dir <- liftIO Unix.getWorkingDirectory
    vars <- Map.fromList <$> liftIO Unix.getEnvironment
    put
        ( Shell
            { _exitCode = ExitSuccess
            , _currentDirectory = dir
            , _previousDirectory = dir
            , _variables = vars
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

interpret :: Term -> Env ()
interpret = \case
    Empty -> return ()
    TSeq l r -> interpret l >> interpret r
    TExternal command -> external command
    TBuiltin command -> builtin command
    TOr l r -> do
        interpret l
        code <- getExitCode
        onFailure () code (interpret r)
    TAnd l r -> do
        interpret l
        code <- getExitCode
        onSuccess () code (interpret r)
    TBang bang -> do
        interpret bang
        negateExitCode
    TSub subshell -> do
        sh <- get
        interpret subshell
        put sh
        resetShellTo sh
    TPipe l r -> do
        (readEnd, writeEnd) <- liftIO createPipe
        hs <- ask
        local (hstd_out .~ writeEnd) $ do
            interpret l
            liftIO $ hClose writeEnd
        local (const (hs & hstd_in .~ readEnd)) $ do
            interpret r
            liftIO $ hClose readEnd
    TRedirection command redirs -> do
        hds <- liftIO $ redirections redirs
        let (readz, writez) = splitByMode hds
        inputHandle <-
            if not (null readz)
                then do
                    (readEnd, writeEnd) <- liftIO createPipe
                    liftIO $ do
                        mapM_
                            (\h -> copyHandleData h writeEnd >> hClose h)
                            readz
                        hClose writeEnd
                    return (Just readEnd)
                else return Nothing
        let updateInputHandle = maybe id (hstd_in .~) inputHandle
            closeInputHandle = maybe (return ()) (liftIO . hClose) inputHandle
        if null writez
            then do
                local updateInputHandle $ do
                    hs <- interpret command
                    closeInputHandle
                    return hs
            else do
                (readEnd, writeEnd) <- liftIO createPipe
                local ((hstd_out .~ writeEnd) . updateInputHandle) $ do
                    hs <- interpret command
                    liftIO $ hClose writeEnd
                    closeInputHandle
                    output <- liftIO $ hGetContents readEnd
                    liftIO $ mapM_ (\h -> hPutStr h output >> hClose h) writez
                    liftIO $ hClose readEnd
                    return hs

redirections :: NonEmpty Redirection -> IO [(Handle, Mode)]
redirections = mapM redirection . toList
  where
    redirection :: Redirection -> IO (Handle, Mode)
    redirection = \case
        Redirection Write (path :| _) -> do
            fd <-
                liftIO $
                    Unix.fdToHandle
                        =<< Unix.openFd
                            path
                            Unix.WriteOnly
                            ( Unix.defaultFileFlags
                                { Unix.creat = Just Unix.stdFileMode
                                , Unix.trunc = True
                                }
                            )
            return (fd, Write)
        Redirection Append (path :| _) -> do
            fd <-
                liftIO $
                    Unix.fdToHandle
                        =<< Unix.openFd
                            path
                            Unix.WriteOnly
                            ( Unix.defaultFileFlags
                                { Unix.creat = Just Unix.stdFileMode
                                , Unix.append = True
                                }
                            )
            return (fd, Append)
        Redirection Read (a :| xs) -> do
            let path = last (a : xs)
            fd <-
                liftIO $
                    Unix.fdToHandle
                        =<< Unix.openFd path Unix.ReadOnly Unix.defaultFileFlags
            return (fd, Read)
        Redirection ReadWrite (a :| xs) -> do
            let path = last (a : xs)
            fd <-
                liftIO $
                    Unix.fdToHandle
                        =<< Unix.openFd path Unix.ReadWrite Unix.defaultFileFlags
            return (fd, ReadWrite)

external :: External -> Env ()
external = \case
    External command as -> do
        hs <- ask
        as' <- evalArgs as
        let cmd = proc (unpack command) as'
        mby <-
            catchIO $
                createProcess
                    ( cmd
                        { std_in = UseHandle (hs ^. hstd_in)
                        , std_out = UseHandle (hs ^. hstd_out)
                        , std_err = UseHandle (hs ^. hstd_err)
                        }
                    )
        case mby of
            -- TODO: Make the type application unnecessary
            Nothing ->
                err @() (command <> ": command not found")
            Just (_, _, _, processHandle) -> do
                code <- liftIO $ waitForProcess processHandle
                setExitCode code
                return ()

evalArgs :: [Arg] -> Env [String]
evalArgs = mapM eval
  where
    eval :: Arg -> Env String
    eval (AIdent str) = return (unpack str)
    eval (ASub _) = TODO

-- TODO: Not really compatible with pipes atm
builtin :: Builtin -> Env ()
builtin = \case
    TCd [] -> getVar "HOME" >>= success . Unix.changeWorkingDirectory
    TCd [ASub _] -> TODO
    TCd [AIdent arg] -> do
        liftIO (doesDirectoryExist (unpack arg)) >>= \case
            False -> err "cd: No such file or directory"
            True -> success $ Unix.changeWorkingDirectory (unpack arg)
    TCd ((_ : _)) -> err "cd: too many arguments"
    TPwd args
        | isEmpty args -> do
            out <- asks (view hstd_out)
            success (Unix.getWorkingDirectory >>= hPutStrLn out . pack)
        | otherwise -> err "pwd: too many arguments"
    TExit args
        | isEmpty args -> exitShell
        | otherwise -> err "exit: too many arguments"
    TLog args
        | isEmpty args -> modifying exit (const True)
        | otherwise -> err "log: too many arguments"

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
