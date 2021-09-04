{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC
    -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
#-}
module Streaming.Class (Stream(..), toStream) where

import qualified Streaming

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