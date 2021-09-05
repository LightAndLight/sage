{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Streaming.ByteString.Strict (StreamByteString (..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Of (Of ((:>)))
import Data.Word (Word8)
import Streaming.Class (Stream (..))

newtype StreamByteString = StreamByteString ByteString

instance Stream (Of Word8) Identity () StreamByteString where
  data Result StreamByteString = Done | More !Word8 {-# UNPACK #-} !ByteString
  fromResult Done = Left ()
  fromResult (More c b) = Right (c :> StreamByteString b)

  uncons (StreamByteString b) =
    Identity $
      case ByteString.uncons b of
        Nothing -> Done
        Just (w, b') -> More w b'
