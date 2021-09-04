{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Streaming.ByteString.Strict.Utf8 (StreamByteStringUtf8(..)) where

import Data.ByteString (ByteString)
import Streaming.Class (Stream(..))
import Data.Functor.Of (Of ((:>)))
import Data.Word (Word8)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.ByteString as ByteString
import GHC.Word (Word8(W8#))
import GHC.Exts (Char(C#), chr#, word2Int#)

unsafeChr8 :: Word8 -> Char
unsafeChr8 (W8# w#) = C# (chr# (word2Int# w#))
{-# INLINE unsafeChr8 #-}

chr1 :: Word8 -> ByteString -> (Char, ByteString)
chr1 n1 bs = (unsafeChr8 n1, bs)


chr2 :: Word8 -> ByteString -> (Char, ByteString)
chr2 (W8# x1#) (W8# x2#) = C# (chr# (z1# +# z2#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !z1# = uncheckedIShiftL# (y1# -# 0xC0#) 6#
      !z2# = y2# -# 0x80#
{-# INLINE chr2 #-}

chr3 :: Word8 -> ByteString -> (Char, ByteString)
chr3 (W8# x1#) (W8# x2#) (W8# x3#) = C# (chr# (z1# +# z2# +# z3#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !z1# = uncheckedIShiftL# (y1# -# 0xE0#) 12#
      !z2# = uncheckedIShiftL# (y2# -# 0x80#) 6#
      !z3# = y3# -# 0x80#
{-# INLINE chr3 #-}

chr4 :: Word8 -> ByteString -> (Char, ByteString)
chr4 (W8# x1#) (W8# x2#) (W8# x3#) (W8# x4#) =
    C# (chr# (z1# +# z2# +# z3# +# z4#))
    where
      !y1# = word2Int# x1#
      !y2# = word2Int# x2#
      !y3# = word2Int# x3#
      !y4# = word2Int# x4#
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

newtype StreamByteStringUtf8 = StreamByteStringUtf8 ByteString

instance Stream (Of Char) Identity () StreamByteStringUtf8 where  
  data Result StreamByteStringUtf8 = Done | More !Char {-# unpack #-} !ByteString
  fromResult Done = Left ()
  fromResult (More c b) = Right (c :> StreamByteStringUtf8 b)
  
  uncons (StreamByteStringUtf8 b) =
    Identity $
    case ByteString.uncons b of
      Nothing -> Done
      Just (w, b') | (c, b'') <- decodeChar w b' -> More c b''
