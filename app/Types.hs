{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Writer (MonadWriter, WriterT)
import Data.Functor.Identity (Identity)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import Text.Parsec (Parsec)
import Text.Parsec.Expr (OperatorTable)
import Optics


data Handles = Handles
    { _hstd_in :: !Handle
    , _hstd_out :: !Handle
    , _hstd_err :: !Handle
    }
    deriving (Show, Eq, Generic)

makeLenses ''Handles

data Shell = Shell
    { _exitCode :: !ExitCode
    , _currentDirectory :: !FilePath
    , _previousDirectory :: !FilePath
    , _variables :: !(Map String String)
    , _handles :: !Handles
    , _exit :: !Bool
    }
    deriving (Show, Generic)
makeLenses 'Shell

newtype Env a = Env {runEnv :: StateT Shell (WriterT [String] IO) a}
    deriving (Functor, Applicative, Monad, MonadState Shell, MonadWriter [String], MonadIO, Generic)

type Ident = Text


data Arg = AIdent !Ident | ASub !Term
    deriving (Show, Eq, Generic)

data External = External !Ident ![Arg]
    deriving (Show, Eq, Generic)

data Builtin
    = TCd ![Arg]
    | TExit ![Arg]
    | TPwd ![Arg]
    | TLog ![Arg]
    deriving (Show, Eq, Generic)
data Mode = Write | Read | ReadWrite | Append
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
    deriving (Show, Eq, Generic)

type Parser a = Parsec Text () a
type Table a = OperatorTable Text () Identity a
