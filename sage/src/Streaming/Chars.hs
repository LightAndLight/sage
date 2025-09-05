{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Streaming.Chars (module Data.Functor.Of, Chars (..), Result(..), toStream) where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Functor.Of
import qualified Streaming

data Result s
  = Done
  | More {-# UNPACK #-} !Char !s

class Chars s where
  uncons :: s -> Result s

instance (f ~ Of Char, m ~ Identity, a ~ ()) => Chars (Streaming.Stream f m a) where
  uncons s =
    case runIdentity $ Streaming.inspect s of
      Left () -> Done
      Right (c :> s') -> More c s'

toStream :: (Chars s, Monad m) => s -> Streaming.Stream (Of Char) m ()
toStream =
  Streaming.unfold $ \s ->
  case uncons s of
    Done -> pure $ Left ()
    More c s' -> pure $ Right (c :> s')
