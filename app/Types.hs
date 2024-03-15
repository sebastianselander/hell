{-# LANGUAGE UndecidableInstances #-}
module Types where

import Data.Map (Map)
import Control.Monad.State (StateT, MonadState)
import Control.Monad.Writer (WriterT, MonadWriter)
import Control.Monad.IO.Class (MonadIO)
import Text.Parsec.Expr (OperatorTable)
import Text.Parsec (Parsec)
import Data.Functor.Identity (Identity)
import System.Exit (ExitCode(..))
import Data.Text (Text)

data Shell = Shell
    { exitCode :: ExitCode
    , currentDirectory :: FilePath
    , previousDirectory :: FilePath
    , variables :: Map String String
    }
    deriving (Show)

newtype Env a = Env {runEnv :: StateT Shell (WriterT [String] IO) a}
    deriving (Functor, Applicative, Monad, MonadState Shell, MonadWriter [String], MonadIO)

type Ident = Text

data Term
    = Empty
    | TSeq !Term !Term
    | TOr !Term !Term
    | TAnd !Term !Term
    | TPipe !Term !Term
    | TBang !Term
    | TSub !Term
    | TExternal !External
    | TBuiltin !Builtin
    deriving (Show, Eq, Ord)

data External = TCommand !Ident ![Arg]
    deriving (Show, Eq, Ord)

data Builtin
    = TCd ![Arg]
    | TExit ![Arg]
    | TPwd ![Arg]
    deriving (Show, Eq, Ord)

data Arg = AIdent !Ident | ASub !Term
    deriving (Show, Eq, Ord)

type Parser a = Parsec Text () a
type Table a = OperatorTable Text () Identity a
