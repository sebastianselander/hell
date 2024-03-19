{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Writer (MonadWriter, WriterT)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Text (Text)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import Text.Parsec (Parsec)
import Text.Parsec.Expr (OperatorTable)

data Shell = Shell
    { exitCode :: !ExitCode
    , currentDirectory :: !FilePath
    , previousDirectory :: !FilePath
    , variables :: !(Map String String)
    , handles :: !Handles
    }
    deriving (Show)

newtype Env a = Env {runEnv :: StateT Shell (WriterT [String] IO) a}
    deriving (Functor, Applicative, Monad, MonadState Shell, MonadWriter [String], MonadIO)

type Ident = Text

data Mode = Read | Write | ReadWrite | Append
    deriving (Show, Eq)

data Term
    = Empty
    | TRedirection !Mode !Term !Text
    | TSeq !Term !Term
    | TOr !Term !Term
    | TAnd !Term !Term
    | TPipe !Term !Term
    | TBang !Term
    | TSub !Term
    | TExternal !External
    | TBuiltin !Builtin
    deriving (Show, Eq)

data Handles = Handles
    { handles_stdin :: !Handle
    , handles_stdout :: !Handle
    , handles_stderr :: !Handle
    }
    deriving (Show, Eq)

data External = External !Ident ![Arg]
    deriving (Show, Eq)

data Builtin
    = TCd ![Arg]
    | TExit ![Arg]
    | TPwd ![Arg]
    deriving (Show, Eq)

data Arg = AIdent !Ident | ASub !Term
    deriving (Show, Eq)

type Parser a = Parsec Text () a
type Table a = OperatorTable Text () Identity a
