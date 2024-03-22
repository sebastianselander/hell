{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Shell where

import Control.Exception (IOException, catch, handle)
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
      IOMode (..),
      hClose,
      hFlush,
      hGetContents,
      hPutStr,
      openFile,
      stderr,
      stdin,
      stdout,
    )
import System.IO.Error
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
        prompt
        line <- shellGetInput
        case term line of
            Left e -> liftIO $ putStrLn e
            Right t -> liftIO (print t) >> interpret t
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
        local (set hstd_out writeEnd) $ do
            interpret l
            liftIO $ hClose writeEnd
        local (set hstd_in readEnd) $ do
            interpret r
            liftIO $ hClose readEnd

    {-
        command n> word
        create pipe from n to word

        command n< word
        read from word to n

    -}
    TRedirection command redirs -> do
        liftIO (redirections redirs) >>= \case
            Left errs -> report errs
            Right hds -> do
                let (readHandles, writeHandles) = splitByMode hds
                inputHandle <- case readHandles of
                    [] -> return Nothing
                    _ -> liftIO $ do
                        (readEnd, writeEnd) <- createPipe
                        let copy' h = copyHandleData h writeEnd >> hClose h
                        mapM_ copy' readHandles
                        hClose writeEnd
                        return (Just readEnd)
                let updateInputHandle = maybe id (set hstd_in) inputHandle
                    closeInputHandle =
                        maybe
                            (return ())
                            (liftIO . hClose)
                            inputHandle
                case writeHandles of
                    [] -> do
                        local updateInputHandle $ do
                            hs <- interpret command
                            closeInputHandle
                            return hs
                    _ -> do
                        (readEnd, writeEnd) <- liftIO createPipe
                        local (set hstd_out writeEnd . updateInputHandle) $ do
                            hs <- interpret command
                            closeInputHandle
                            liftIO $ do
                                hClose writeEnd
                                output <- hGetContents readEnd
                                mapM_
                                    (\h -> hPutStr h output >> hClose h)
                                    writeHandles
                                hClose readEnd
                            return hs

report :: [Text] -> Env ()
report = mapM_ (err @())

redirections :: NonEmpty Redirection -> IO (Either [Text] [(Handle, Mode)])
redirections = fmap (esequence . reverse) . mapM redirection . toList
  where
    redirection :: Redirection -> IO (Either Text (Handle, Mode))
    redirection = \case
        Redirection fd Write (path :| _) -> do
            hndl <-
                handle @IOError
                    (return . Left . pack . show . flip ioeSetLocation "")
                    (Right <$> openFile path WriteMode)
            return ((,Write) <$> hndl)
        Redirection fd Append (path :| _) -> do
            hndl <-
                handle @IOError
                    (return . Left . pack . show . flip ioeSetLocation "")
                    (Right <$> openFile path AppendMode)
            return ((,Append) <$> hndl)
        Redirection fd Read (a :| xs) -> do
            let path = last (a : xs)
            hndl <-
                handle @IOError
                    (return . Left . pack . show . flip ioeSetLocation "")
                    (Right <$> openFile path ReadMode)
            return ((,Read) <$> hndl)
        Redirection fd ReadWrite (a :| xs) -> do
            let path = last (a : xs)
            hndl <-
                handle @IOError
                    (return . Left . pack . show . flip ioeSetLocation "")
                    (Right <$> openFile path ReadWriteMode)
            return ((,Read) <$> hndl)

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
                        { std_in = UseHandle (view hstd_in hs)
                        , std_out = UseHandle (view hstd_out hs)
                        , std_err = UseHandle (view hstd_err hs)
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
