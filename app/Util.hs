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

splitByMode :: [(a, Mode)] -> ([a], [a])
splitByMode = go [] [] 
  where
    go reads writes [] = (reverse reads, reverse writes)
    go reads writes ((a, mode) : rest) = case mode of
        Read -> go (a : reads) writes rest
        ReadWrite -> go (a : reads) writes rest
        Write -> go reads (a : writes) rest
        Append -> go reads (a : writes) rest
