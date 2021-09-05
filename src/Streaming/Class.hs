{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Streaming.Class (module Data.Functor.Of, Stream(..), toStream) where

import qualified Streaming
import Data.Functor.Of

class Stream f m a s | s -> f m a where
  data Result s :: *
  fromResult :: Result s -> Either a (f s)
  uncons :: s -> m (Result s)

instance Monad m => Stream f m a (Streaming.Stream f m a) where
  newtype Result (Streaming.Stream f m a) = Result { getResult :: Either a (f (Streaming.Stream f m a)) }
  fromResult = getResult
  uncons s = Result <$> Streaming.inspect s

toStream :: (Functor f, Monad m) => Stream f m a s => s -> Streaming.Stream f m a
toStream = Streaming.unfold (fmap fromResult . uncons)