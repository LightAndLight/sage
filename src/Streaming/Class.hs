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
import Data.Functor.Of (Of ((:>)))
import qualified Data.Text as Strict
import Control.Monad.Identity (Identity (Identity))
import qualified Data.Text as Text

class Stream f m a s | s -> f m a where
  data Result s :: *
  fromResult :: Result s -> Either a (f s)
  uncons :: s -> m (Result s)

instance Monad m => Stream f m a (Streaming.Stream f m a) where
  newtype Result (Streaming.Stream f m a) = ResultStream { getResult :: Either a (f (Streaming.Stream f m a)) }
  fromResult = getResult
  uncons s = ResultStream <$> Streaming.inspect s

instance Stream (Of Char) Identity () Strict.Text where
  data Result Strict.Text = DoneStrictText | MoreStrictText !Char {-# unpack #-} !Strict.Text
  
  fromResult DoneStrictText = Left ()
  fromResult (MoreStrictText c t) = Right (c :> t)
  
  uncons t =
    Identity $
    case Text.uncons t of
      Nothing -> DoneStrictText
      Just (c, t') -> MoreStrictText c t'

toStream :: (Functor f, Monad m) => Stream f m a s => s -> Streaming.Stream f m a
toStream = Streaming.unfold (fmap fromResult . uncons)