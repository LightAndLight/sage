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

module Streaming.Chars.ByteString.Utf8 (StreamUtf8 (..)) where

{-

Some code in this module has been copied from the `text` library's `Data.Text.Internal.Unsafe.Char` module.

(c) 2008, 2009 Tom Harper, (c) 2009, 2010 Bryan O'Sullivan, (c) 2009 Duncan Coutts

-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)
import GHC.Exts (Char (C#), Int#, Word8#, chr#, uncheckedIShiftL#, int8ToInt#, word8ToInt8#, (+#), (-#))
import GHC.Word (Word8 (W8#))
import Streaming.Chars (Chars (..))

word8ToInt# :: Word8# -> Int#
word8ToInt# w = int8ToInt# (word8ToInt8# w)

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word8ToInt# w#))
{-# INLINE unsafeChr8 #-}

chr1 :: Word8 -> ByteString -> (# Char, ByteString #)
chr1 n1 bs = (# unsafeChr8 n1, bs #)
{-# INLINE chr1 #-}

chr2 :: Word8 -> Word8 -> ByteString -> (# Char, ByteString #)
chr2 (W8# x1#) (W8# x2#) bs =
  (# C# (chr# (z1# +# z2#)), bs #)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
    !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> Word8 -> Word8 -> ByteString -> (# Char, ByteString #)
chr3 (W8# x1#) (W8# x2#) (W8# x3#) bs =
  (# C# (chr# (z1# +# z2# +# z3#)), bs #)
  where
    !y1# = word8ToInt# x1#
    !y2# = word8ToInt# x2#
    !y3# = word8ToInt# x3#
    !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
    !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
    !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> ByteString -> (# Char, ByteString #)
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) bs =
  (# C# (chr# (z1# +# z2# +# z3# +# z4#)), bs #)
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

decodeChar :: Word8 -> ByteString -> (# Char, ByteString #)
decodeChar n1 bs =
  if n1 < 0xC0
    then chr1 n1 bs
    else case ByteString.uncons bs of
      Nothing -> error utf8Error
      Just (n2, bs') ->
        if n1 < 0xE0
          then chr2 n1 n2 bs'
          else case ByteString.uncons bs' of
            Nothing -> error utf8Error
            Just (n3, bs'') ->
              if n1 < 0xF0
                then chr3 n1 n2 n3 bs''
                else case ByteString.uncons bs'' of
                  Nothing -> error utf8Error
                  Just (n4, bs''') ->
                    chr4 n1 n2 n3 n4 bs'''
  where
    utf8Error = "utf8 encoding error"

newtype StreamUtf8 = StreamUtf8 ByteString

instance Chars StreamUtf8 where
  data Result StreamUtf8 = Done | More !Char {-# UNPACK #-} !ByteString
  fromResult Done = Nothing
  fromResult (More c b) = Just (c, StreamUtf8 b)

  uncons (StreamUtf8 b) =
    case ByteString.uncons b of
      Nothing -> Done
      Just (w, b') | (# c, b'' #) <- decodeChar w b' -> More c b''
