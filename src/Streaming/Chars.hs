{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Streaming.Chars (Chars(..)) {- (module Data.Functor.Of, Chars (..), toStream) -} where

import GHC.Exts (Addr#, Int#, Char#)

class Chars s where
  unsafeWithPinned :: s -> ((# Addr#, Int# #) -> a) -> a
  uncons :: (# Addr#, Int# #) -> Int# -> (# Int#, Char# #)

{-
instance (f ~ Of Char, m ~ Identity, a ~ ()) => Chars (Streaming.Stream f m a) where
  newtype Result (Streaming.Stream f m a) = Result {getResult :: Either a (f (Streaming.Stream f m a))}
  fromResult = either (\() -> Nothing) (\(c :> rest) -> Just (c, rest)) . getResult
  uncons s = Result . runIdentity $ Streaming.inspect s

toStream :: (Chars s, Monad m) => s -> Streaming.Stream (Of Char) m ()
toStream = Streaming.unfold (pure . maybe (Left ()) (\(c, rest) -> Right $ c :> rest) . fromResult . uncons)
-}
