{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Streaming.Chars.ByteString.Utf8 (StreamUtf8 (..)) where

{-

Some code in this module has been copied from the `text` library's `Data.Text.Internal.Unsafe.Char` module.

(c) 2008, 2009 Tom Harper, (c) 2009, 2010 Bryan O'Sullivan, (c) 2009 Duncan Coutts

-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Word (Word8)
import GHC.Exts (Char (C#), chr#, uncheckedIShiftL#, word2Int#, word8ToWord#, (+#), (-#))
import GHC.Word (Word8 (W8#))
import Streaming.Chars (Chars (..))

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# (word8ToWord# w#)))
{-# INLINE unsafeChr8 #-}

chr1 :: Word8 -> ByteString -> (Char, ByteString)
chr1 n1 bs = (unsafeChr8 n1, bs)

utf8Error :: a
utf8Error = error "utf8 encoding error"

chr2 :: Word8 -> ByteString -> (Char, ByteString)
chr2 (W8# x1#) bs =
  case ByteString.uncons bs of
    Nothing -> utf8Error
    Just (W8# x2#, bs') -> (C# (chr# (z1# +# z2#)), bs')
      where
        !y1# = word2Int# (word8ToWord# x1#)
        !y2# = word2Int# (word8ToWord# x2#)
        !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
        !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> ByteString -> (Char, ByteString)
chr3 (W8# x1#) bs =
  case ByteString.uncons bs of
    Nothing -> utf8Error
    Just (W8# x2#, bs') ->
      case ByteString.uncons bs' of
        Nothing -> utf8Error
        Just (W8# x3#, bs'') -> (C# (chr# (z1# +# z2# +# z3#)), bs'')
          where
            !y1# = word2Int# (word8ToWord# x1#)
            !y2# = word2Int# (word8ToWord# x2#)
            !y3# = word2Int# (word8ToWord# x3#)
            !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
            !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
            !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> ByteString -> (Char, ByteString)
chr4 (W8# x1#) bs =
  case ByteString.uncons bs of
    Nothing -> utf8Error
    Just (W8# x2#, bs') ->
      case ByteString.uncons bs' of
        Nothing -> utf8Error
        Just (W8# x3#, bs'') ->
          case ByteString.uncons bs'' of
            Nothing -> utf8Error
            Just (W8# x4#, bs''') -> (C# (chr# (z1# +# z2# +# z3# +# z4#)), bs''')
              where
                !y1# = word2Int# (word8ToWord# x1#)
                !y2# = word2Int# (word8ToWord# x2#)
                !y3# = word2Int# (word8ToWord# x3#)
                !y4# = word2Int# (word8ToWord# x4#)
                !z1# = uncheckedIShiftL# (y1# -# 0xF0#) 18#
                !z2# = uncheckedIShiftL# (y2# -# 0x80#) 12#
                !z3# = uncheckedIShiftL# (y3# -# 0x80#) 6#
                !z4# = y4# -# 0x80#
{-# INLINE chr4 #-}

decodeChar :: Word8 -> ByteString -> (Char, ByteString)
decodeChar n1 bs
  | n1 < 0xC0 = chr1 n1 bs
  | n1 < 0xE0 = chr2 n1 bs
  | n1 < 0xF0 = chr3 n1 bs
  | otherwise = chr4 n1 bs
{-# INLINE decodeChar #-}

newtype StreamUtf8 = StreamUtf8 ByteString

instance Chars StreamUtf8 where
  data Result StreamUtf8 = Done | More !Char {-# UNPACK #-} !ByteString
  fromResult Done = Nothing
  fromResult (More c b) = Just (c, StreamUtf8 b)

  uncons (StreamUtf8 b) =
    case ByteString.uncons b of
      Nothing -> Done
      Just (w, b') | (c, b'') <- decodeChar w b' -> More c b''
