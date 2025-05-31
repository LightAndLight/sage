{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Streaming.Chars.ByteString.Utf8 (StreamUtf8 (..)) where

{-

Some code in this module has been copied from the `text` library's `Data.Text.Internal.Unsafe.Char` module.

(c) 2008, 2009 Tom Harper, (c) 2009, 2010 Bryan O'Sullivan, (c) 2009 Duncan Coutts

-}

import Data.Text ()
import Data.ByteString.Internal (ByteString(..))
import GHC.Exts (Int#, Word8#, chr#, int8ToInt#, uncheckedIShiftL#, word8ToInt8#, (+#), (-#), Ptr (..), Int (..), (<#), Addr#, runRW#, Char#, readWord8OffAddr#, State#)
import GHC.Word (Word8 (W8#))
import Streaming.Chars (Chars (..))
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import Data.String (IsString (..))
import Data.Text.Encoding (encodeUtf8)

word8ToInt# :: Word8# -> Int#
word8ToInt# w = int8ToInt# (word8ToInt8# w)

chr2 :: Word8# -> Word8# -> Char#
chr2 x1# x2# =
  chr# (z1# +# z2#)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8# -> Word8# -> Word8# -> Char#
chr3 x1# x2# x3# =
  chr# (z1# +# z2# +# z3#)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !y3# = word8ToInt# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4 x1# x2# x3# x4# =
  chr# (z1# +# z2# +# z3# +# z4#)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !y3# = word8ToInt# x3#
    !y4# = word8ToInt# x4#
    !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
    !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
    !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

{-# inline unconsW8 #-}
unconsW8 ::
  (# Addr#, Int# #) ->
  Int# ->
  State# s ->
  (# State# s, (# (# #) | Word8# #) #)
unconsW8 (# ptr, len #) pos s
  | 1# <- pos <# len =
      let !(# s', w #) = readWord8OffAddr# ptr pos s in
      (# s', (# | w #) #)
  | otherwise = (# s, (# (# #) | #) #)

{-# inlineable decodeChar #-}
decodeChar :: Word8# -> (# Addr#, Int# #) -> Int# -> State# s -> (# State# s, (# Int#, Char# #) #)
decodeChar n1 bs pos s
  | W8# n1 < 0xC0 =
      let !c = chr# (word8ToInt# n1) in
      (# s, (# 0#, c #) #)
  | W8# n1 < 0xE0 =
      let
        !(# s', c #)
          | (# s0, (# | n2 #) #) <- unconsW8 bs pos s
          = (# s0, chr2 n1 n2 #)
          | otherwise = error utf8Error
      in
      (# s', (# 1#, c #) #)
  | W8# n1 < 0xF0 =
      let
        !(# s'', c  #)
          | (# s', (# | n2 #) #) <- unconsW8 bs pos s
          , (# s0, (# | n3 #) #) <- unconsW8 bs (pos +# 1#) s'
          = (# s0, chr3 n1 n2 n3  #)
          | otherwise = error utf8Error
      in
      (# s'', (# 2#, c #) #)
  | otherwise =
      let
        !(# s''', c #)
          | (# s', (# | n2 #) #) <- unconsW8 bs pos s
          , (# s'', (# | n3 #) #) <- unconsW8 bs (pos +# 1#) s'
          , (# s0, (# | n4 #) #) <- unconsW8 bs (pos +# 2#) s''
          = (# s0, chr4 n1 n2 n3 n4 #)
          | otherwise = error utf8Error
      in
      (# s''', (# 3#, c #) #)
  where
    utf8Error = "utf8 encoding error"

newtype StreamUtf8 = StreamUtf8 ByteString

instance IsString StreamUtf8 where
  fromString = StreamUtf8 . encodeUtf8 . fromString

instance Chars StreamUtf8 where
  {-# inline unsafeWithPinned #-}
  unsafeWithPinned (StreamUtf8 (BS ptr (I# len))) f =
    unsafePerformIO (withForeignPtr ptr $ \(Ptr addr) -> pure (f (# addr, len #)))

  {-# inlineable uncons #-}
  uncons bs pos =
    runRW# (\s -> 
      let !(# s', res #) = unconsW8 bs pos s in
      case res of
        (# (# #) | #) ->
          (# 0#, chr# 0# #)
        (# | w #) ->
          let !(# _s'', (# off, c #) #) = decodeChar w bs (pos +# 1#) s' in
          (# off +# 1#, c #)
      )

