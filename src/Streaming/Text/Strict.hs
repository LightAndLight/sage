{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Streaming.Text.Strict (StreamText(..)) where

import Data.Text (Text)
import Streaming.Class (Stream(..))
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Of (Of ((:>)))
import qualified Data.Text as Text
import Data.String (IsString)
newtype StreamText = StreamText Text
  deriving (Eq, Show, IsString)

instance Stream (Of Char) Identity () StreamText where
  data Result StreamText = Done | More !Char {-# unpack #-} !Text
  
  fromResult Done = Left ()
  fromResult (More c t) = Right (c :> StreamText t)
  
  uncons (StreamText t) =
    Identity $
    case Text.uncons t of
      Nothing -> Done
      Just (c, t') -> More c t'