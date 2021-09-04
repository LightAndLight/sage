{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Streaming.ByteString.Strict (StreamByteString(..)) where

import Data.ByteString (ByteString)
import Streaming.Class (Stream(..))
import Data.Functor.Of (Of ((:>)))
import Data.Word (Word8)
import Data.Functor.Identity (Identity (Identity))
import qualified Data.ByteString as ByteString

newtype StreamByteString = StreamByteString ByteString

instance Stream (Of Word8) Identity () StreamByteString where  
  data Result StreamByteString = Done | More !Word8 {-# unpack #-} !ByteString
  fromResult Done = Left ()
  fromResult (More c b) = Right (c :> StreamByteString b)
  
  uncons (StreamByteString b) =
    Identity $
    case ByteString.uncons b of
      Nothing -> Done
      Just (w, b') -> More w b'
