{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Text.Parser.Sage (ParsersSage (..)) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (MonadPlus)
import Streaming.Chars (Chars)
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.LookAhead (LookAheadParsing (..))
import Text.Parser.Token (TokenParsing (..))
import Text.Sage (Parser (..), skipMany, string)
import qualified Text.Sage

newtype ParsersSage s a = ParsersSage {getParsersSage :: Parser s a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Chars s => Parsing (ParsersSage s) where
  try (ParsersSage p) = ParsersSage (Text.Sage.try p)
  (<?>) (ParsersSage p) = ParsersSage . (Text.Sage.<?>) p
  skipMany (ParsersSage p) = ParsersSage (Text.Sage.skipMany p)
  skipSome (ParsersSage p) = ParsersSage (Text.Sage.skipSome p)
  notFollowedBy (ParsersSage p) = ParsersSage (Text.Sage.notFollowedBy p)
  unexpected _ = empty
  eof = ParsersSage Text.Sage.eof

instance Chars s => CharParsing (ParsersSage s) where
  satisfy = ParsersSage . Text.Sage.satisfy
  char = ParsersSage . Text.Sage.char
  text = ParsersSage . Text.Sage.string

instance Chars s => TokenParsing (ParsersSage s) where
  token p = p <* (someSpace <|> pure ())

instance Chars s => LookAheadParsing (ParsersSage s) where
  lookAhead (ParsersSage p) = ParsersSage (Text.Sage.lookAhead p)
