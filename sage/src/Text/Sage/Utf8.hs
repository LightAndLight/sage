{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Text.Sage.Utf8 (ByteString#, unsafeWithByteString#, uncons) where

import GHC.Exts (Int#, Word8#, chr#, Char#, Addr#, (>=#), readWord8OffAddr#, ltWord8#, int2Word#, wordToWord8#, word8ToWord#, word2Int#, or#, and#, uncheckedShiftL#, Int (..), runRW#, Ptr (..))
import GHC.Word (Word8 (W8#))
import Numeric (showHex)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (toForeignPtr0)
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)

type ByteString# = (# Addr#, Int# #)

{- | The @Addr#@ inside the @ByteString#@ must not outlive the call to @unsafeWithByteString#@.

In addition to being explicitly stored in the @a@, it's also possible for the @Addr#@ to
be captured by a thunk (residing in the @a@) that will evaluate to something that doesn't
reference the @Addr#@. That thunk will be evaluated after @unsafeWithByteString#@ returns, risking
invalid memory access.

Any thunks involving the @Addr#@ should be forced with the @a@ using 'seq'.
-}
unsafeWithByteString# :: ByteString -> (ByteString# -> a) -> a
unsafeWithByteString# bs f =
  let !(fptr, I# len) = toForeignPtr0 bs in
  unsafePerformIO . withForeignPtr fptr $ \(Ptr addr) -> pure $! f (# addr, len #)

int2Word8# :: Int# -> Word8#
int2Word8# x = wordToWord8# (int2Word# x)

uncons :: ByteString# -> (# (# #) | (# Char#, Int# #) #)
uncons (# addr, len #) =
  runRW# (\s0 ->
  case len >=# 1# of
    1# ->
      let
        !(# s1, b1 #) = readWord8OffAddr# addr 0# s0
      in
        case () of
          ()
            | 1# <- b1 `ltWord8#` int2Word8# 0x80# {- 0b1000_0000 -} ->
                let
                  !c = word8ToWord# b1
                in
                  (# | (# chr# (word2Int# c), 1# #) #)
            | 1# <- b1 `ltWord8#` int2Word8# 0xE0# {- 0b1110_0000 -} ->
                case len >=# 2# of
                  1# ->
                    let
                      !(# _s2, b2 #) = readWord8OffAddr# addr 1# s1
                      !c =
                        uncheckedShiftL#
                          (and# (int2Word# 0x1F# {- 0b0001_1111 -}) (word8ToWord# b1))
                          6#
                          `or#`
                          and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b2)
                    in
                      (# | (# chr# (word2Int# c), 2# #) #)
                  _ ->
                    error "unexpected end of 2-byte UTF-8 sequence"
            | 1# <- b1 `ltWord8#` int2Word8# 0xF0# {- 0b1111_0000 -} ->
                case len >=# 3# of
                  1# ->
                    let
                      !(# s2, b2 #) = readWord8OffAddr# addr 1# s1
                      !(# _s3, b3 #) = readWord8OffAddr# addr 2# s2
                      !c =
                        uncheckedShiftL#
                          (and# (int2Word# 0x0F# {- 0b0000_1111 -}) (word8ToWord# b1))
                          12#
                        `or#`
                        uncheckedShiftL#
                          (and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b2))
                          6#
                        `or#`
                        and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b3)
                    in
                      (# | (# chr# (word2Int# c), 3# #) #)
                  _ ->
                    error "unexpected end of 3-byte UTF-8 sequence"
            | 1# <- b1 `ltWord8#` int2Word8# 0xF8# {- 0b1111_1000 -} ->
                case len >=# 4# of
                  1# ->
                    let
                      !(# s2, b2 #) = readWord8OffAddr# addr 1# s1
                      !(# s3, b3 #) = readWord8OffAddr# addr 2# s2
                      !(# _s4, b4 #) = readWord8OffAddr# addr 3# s3
                      !c =
                        uncheckedShiftL#
                          (and# (int2Word# 0x7# {- 0b0000_0111 -}) (word8ToWord# b1))
                          18#
                        `or#`
                        uncheckedShiftL#
                          (and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b2))
                          12#
                        `or#`
                        uncheckedShiftL#
                          (and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b3))
                          6#
                        `or#`
                        and# (int2Word# 0x3F# {- 0b0011_1111 -}) (word8ToWord# b4)
                    in
                      (# | (# chr# (word2Int# c), 4# #) #)
                  _ ->
                    error "unexpected end of 4-byte UTF-8 sequence"
            | otherwise -> error $ "invalid UTF-8 leading byte " ++ showHex (W8# b1) ""
    _ ->
      (# (# #) | #)
  )
