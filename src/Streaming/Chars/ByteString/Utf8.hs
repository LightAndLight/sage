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

import Data.Text ()
import Data.ByteString.Internal (ByteString(..))
import GHC.Exts (Int#, Word8#, chr#, int8ToInt#, uncheckedIShiftL#, word8ToInt8#, (+#), (-#), Ptr (..), Int (..), (<#), Addr#, runRW#, Char#)
import GHC.Word (Word8 (W8#))
import Streaming.Chars (Chars (..))
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Storable (readWord8OffPtr)
import Data.String (IsString (..))
import Data.Text.Encoding (encodeUtf8)
import GHC.IO (IO(..), noDuplicate)

word8ToInt# :: Word8# -> Int#
word8ToInt# w = int8ToInt# (word8ToInt8# w)

unsafeChr8 :: Word8# -> Char#
unsafeChr8 w# = chr# (word8ToInt# w#)
{-# INLINE unsafeChr8 #-}

chr1 :: Word8# -> Char#
chr1 n1 = unsafeChr8 n1
{-# INLINE chr1 #-}

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
  (# (# #) | Word8# #)
unconsW8 (# ptr, len #) pos
  | 1# <- pos <# len =
      let !(W8# w) = unsafePerformIO' (readWord8OffPtr (Ptr ptr) (I# pos)) in
      (# | w #)
  | otherwise = (# (# #) | #)

{-# inline unsafeDupablePerformIO' #-}
unsafeDupablePerformIO'  :: IO a -> a
unsafeDupablePerformIO' (IO m) = case runRW# m of (# _, a #) -> a

{-# inline unsafePerformIO' #-}
unsafePerformIO' :: IO a -> a
unsafePerformIO' m = unsafeDupablePerformIO' (noDuplicate >> m)

{-# inlineable decodeChar #-}
decodeChar :: Word8# -> (# Addr#, Int# #) -> Int# -> (# Int#, Char# #)
decodeChar n1 bs pos =
  if W8# n1 < 0xC0
    then
      let !c = chr1 n1 in
      (# 0#, c #)
    else case unconsW8 bs pos of
      (# (# #) | #) ->
        error utf8Error
      (# | n2 #) ->
        if W8# n1 < 0xE0
        then
          let !c = chr2 n1 n2 in
          (# 1#, c #)
        else case unconsW8 bs (pos +# 1#) of
          (# (# #) | #) -> error utf8Error
          (# | n3 #) ->
            if W8# n1 < 0xF0
              then
                let !c = chr3 n1 n2 n3 in
                (# 2#, c #)
              else case unconsW8 bs (pos +# 2#) of
                (# (# #) | #)-> error utf8Error
                (# | n4 #) ->
                  let !c = chr4 n1 n2 n3 n4 in
                  (# 3#, c #)
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
    case unconsW8 bs pos of
      (# (# #) | #) ->
        (# 0#, chr# 0# #)
      (# | w #) ->
        let !(# off, c #) = decodeChar w bs (pos +# 1#) in
        (# off +# 1#, c #)

