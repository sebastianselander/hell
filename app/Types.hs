{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Text (Text)
import Optics
import System.Exit (ExitCode (..))
import System.IO (Handle)
import Text.Parsec (Parsec)
import Text.Parsec.Expr (OperatorTable)
import Data.List.NonEmpty (NonEmpty)
import Control.Monad.Reader (ReaderT, MonadReader)

data Handles = Handles
    { _hstd_in :: !Handle
    , _hstd_out :: !Handle
    , _hstd_err :: !Handle
    }
    deriving (Show, Eq)

makeLenses ''Handles

data Shell = Shell
    { _exitCode :: !ExitCode
    , _currentDirectory :: !FilePath
    , _previousDirectory :: !FilePath
    , _variables :: !(Map String String)
    , _exit :: !Bool
    }
    deriving (Show)

makeLenses 'Shell

newtype Env a = Env {runEnv :: StateT Shell (ReaderT Handles IO) a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState Shell
        , MonadReader Handles
        , MonadIO
        )

type Ident = Text

data Arg = AIdent !Ident | ASub !Term
    deriving (Show, Eq)

data External = External !Ident ![Arg]
    deriving (Show, Eq)

data Builtin
    = TCd ![Arg]
    | TExit ![Arg]
    | TPwd ![Arg]
    | TLog ![Arg]
    deriving (Show, Eq)

-- TODO: Add optional file descriptors as well
data Mode = Write | Read | ReadWrite | Append
    deriving (Show, Eq)

data Redirection = Redirection !Mode !(NonEmpty FilePath)
    deriving (Show, Eq)

data Term
    = Empty
    | TRedirection !Term !(NonEmpty Redirection)
    | TSeq !Term !Term
    | TOr !Term !Term
    | TAnd !Term !Term
    | TPipe !Term !Term
    | TBang !Term
    | TSub !Term
    | TExternal !External
    | TBuiltin !Builtin
    deriving (Show, Eq)

type Parser a = Parsec Text () a

type Table a = OperatorTable Text () Identity a
