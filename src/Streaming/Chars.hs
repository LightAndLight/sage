{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Streaming.Chars (module Data.Functor.Of, Chars (..), toStream) where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Functor.Of
import qualified Streaming

class Chars s where
  data Result s :: *
  fromResult :: Result s -> Maybe (Char, s)
  uncons :: s -> Result s

instance (f ~ Of Char, m ~ Identity, a ~ ()) => Chars (Streaming.Stream f m a) where
  newtype Result (Streaming.Stream f m a) = Result {getResult :: Either a (f (Streaming.Stream f m a))}
  fromResult = either (\() -> Nothing) (\(c :> rest) -> Just (c, rest)) . getResult
  uncons s = Result . runIdentity $ Streaming.inspect s

toStream :: (Chars s, Monad m) => s -> Streaming.Stream (Of Char) m ()
toStream = Streaming.unfold (pure . maybe (Left ()) (\(c, rest) -> Right $ c :> rest) . fromResult . uncons)