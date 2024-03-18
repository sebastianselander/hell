{-# LANGUAGE PatternSynonyms #-}

module Util where

import Types
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import System.Console.ANSI

pattern TODO :: a
pattern TODO <- _
  where TODO = error "TODO: Not yet implemented"

setColor :: Color -> Env ()
setColor col = liftIO $ setSGR [SetColor Foreground Dull col]

setGreen, setRed :: Env ()
(setGreen, setRed) = (setColor Green, setColor Red)

setReset :: Env ()
setReset = liftIO $ setSGR [Reset, SetDefaultColor Foreground]
