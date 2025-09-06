{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Sage.Utf8 (uncons, Result(..)) where

{-

Some code in this module has been copied from the `text` library's `Data.Text.Internal.Unsafe.Char` module.

(c) 2008, 2009 Tom Harper, (c) 2009, 2010 Bryan O'Sullivan, (c) 2009 Duncan Coutts

-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)
import GHC.Exts (Int#, Word8#, chr#, int8ToInt#, uncheckedIShiftL#, word8ToInt8#, (+#), (-#), Char#)
import GHC.Word (Word8 (W8#))

word8ToInt# :: Word8# -> Int#
word8ToInt# w = int8ToInt# (word8ToInt8# w)

unsafeChr8 :: Word8 -> Char#
unsafeChr8 (W8# w#) = chr# (word8ToInt# w#)
{-# INLINE unsafeChr8 #-}

chr1 :: Word8 -> (# Char#, Int# #)
chr1 n1 = (# unsafeChr8 n1, 1# #)
{-# INLINE chr1 #-}

chr2 :: Word8 -> Word8 -> (# Char#, Int# #)
chr2 (W8# x1#) (W8# x2#) =
  (# chr# (z1# +# z2#), 2# #)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> (# Char#, Int# #)
chr3 (W8# x1#) (W8# x2#) (W8# x3#) =
  (# chr# (z1# +# z2# +# z3#), 3# #)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !y3# = word8ToInt# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> (# Char#, Int# #)
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
  (# chr# (z1# +# z2# +# z3# +# z4#), 4# #)
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

decodeChar :: Word8 -> ByteString -> (# Char#, Int# #)
decodeChar n1 bs =
  if n1 < 0xC0
    then chr1 n1
    else case ByteString.uncons bs of
      Nothing -> error utf8Error
      Just (n2, bs') ->
        if n1 < 0xE0
          then chr2 n1 n2
          else case ByteString.uncons bs' of
            Nothing -> error utf8Error
            Just (n3, bs'') ->
              if n1 < 0xF0
                then chr3 n1 n2 n3
                else case ByteString.uncons bs'' of
                  Nothing -> error utf8Error
                  Just (n4, _bs''') ->
                    chr4 n1 n2 n3 n4
  where
    utf8Error = "utf8 encoding error"

data Result = Done | More !Char# !Int#

uncons :: ByteString -> Result
uncons bs =
  case ByteString.uncons bs of
    Nothing -> Done
    Just (w, b') | (# c, offset #) <- decodeChar w b' -> More c offset
