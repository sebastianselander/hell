{-# LANGUAGE PatternSynonyms #-}

module Util where

pattern TODO :: a
pattern TODO <- _
  where TODO = error "TODO: Not yet implemented"
