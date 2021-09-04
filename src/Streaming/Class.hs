{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Streaming.Class (Stream(..), toStream) where

import qualified Streaming
import Data.Functor.Of (Of ((:>)))
import qualified Data.Text as Strict
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Text as Text

class Stream f m a s | s -> f m a where
  uncons :: s -> m (Either a (f s))

instance Monad m => Stream f m a (Streaming.Stream f m a) where
  uncons = Streaming.inspect

instance Stream (Of Char) Identity () Strict.Text where
  uncons t =
    Identity $
    case Text.uncons t of
      Nothing -> Left ()
      Just (c, t') -> Right (c :> t')

toStream :: (Functor f, Monad m) => Stream f m a s => s -> Streaming.Stream f m a
toStream = Streaming.unfold uncons