{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Streaming.Text.Strict (StreamText (..)) where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Of (Of ((:>)))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Streaming.Class (Stream (..))

newtype StreamText = StreamText Text
  deriving (Eq, Show, IsString, NFData)

instance Stream (Of Char) Identity () StreamText where
  data Result StreamText = Done | More !Char {-# UNPACK #-} !Text

  fromResult Done = Left ()
  fromResult (More c t) = Right (c :> StreamText t)

  uncons (StreamText t) =
    Identity $
      case Text.uncons t of
        Nothing -> Done
        Just (c, t') -> More c t'