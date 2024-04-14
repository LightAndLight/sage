{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes #-}

module Streaming.Chars.Text (StreamText (..)) where

import Control.DeepSeq (NFData)
import Data.String (IsString)
import Data.Text (Text)

newtype StreamText = StreamText Text
  deriving (Eq, Show, IsString, NFData)

{-
instance Chars StreamText where
  data Result StreamText = Done | More !Char {-# UNPACK #-} !Text

  {-# inline fromResult #-}
  fromResult Done = Nothing
  fromResult (More c t) = Just (c, StreamText t)

  {-# inline uncons #-}
  uncons (StreamText t) =
    case Text.uncons t of
      Nothing -> Done
      Just (c, t') -> More c t'
-}
