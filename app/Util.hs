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

threeToTwo :: (a,b,c) -> ((a,b), c)
threeToTwo (a,b,c) = ((a,b), c)

esequence :: [Either err a] -> Either [err] [a]
esequence xs = case go [] [] xs of
    ([], rights) -> Right rights
    (errs, _) -> Left errs
  where
    go lefts rights [] = (lefts, rights)
    go lefts rights (x:xs) = case x of
        Right a -> go lefts (a : rights) xs
        Left err -> go (err : lefts) [] xs
