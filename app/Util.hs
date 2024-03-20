{-# LANGUAGE PatternSynonyms #-}

module Util where

import Control.Monad.IO.Class (liftIO)
import System.Console.ANSI
import Types

{-# WARNING TODO "TODO IN CODE" #-}
pattern TODO :: a
pattern TODO <- _
    where
        TODO = error "TODO: Not yet implemented"

setColor :: Color -> Env ()
setColor col = liftIO $ setSGR [SetColor Foreground Dull col]

setGreen, setRed :: Env ()
setGreen = setColor Green
setRed = setColor Red

setReset :: Env ()
setReset = liftIO $ setSGR [Reset, SetDefaultColor Foreground]
