{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Streaming.Chars.Text (StreamText (..)) where

import Control.DeepSeq (NFData)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Streaming.Chars (Chars (..), Result(..))

newtype StreamText = StreamText Text
  deriving (Eq, Show, IsString, NFData)

instance Chars StreamText where
  {-# INLINE uncons #-}
  uncons (StreamText t) =
    case Text.uncons t of
      Nothing -> Done
      Just (c, t') -> More c (StreamText t')
