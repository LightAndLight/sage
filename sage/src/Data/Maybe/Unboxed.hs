{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language PatternSynonyms #-}
module Data.Maybe.Unboxed where

-- | The unboxed equivalent of 'Maybe'.
--
-- Contructors: 'Nothing#' and 'Just#'
type Maybe# a = (# (# #) | a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (# #) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# COMPLETE Nothing#, Just# #-}
