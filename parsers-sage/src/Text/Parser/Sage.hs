{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Parser.Sage () where

import Control.Applicative (Alternative (..), (<|>))
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.LookAhead (LookAheadParsing (..))
import Text.Parser.Token (TokenParsing (..))
import Text.Sage (Parser (..), skipMany, string)
import qualified Text.Sage

instance Parsing Parser where
  try = Text.Sage.try
  (<?>) = (Text.Sage.<?>)
  skipMany = Text.Sage.skipMany
  skipSome = Text.Sage.skipSome
  notFollowedBy = Text.Sage.notFollowedBy
  unexpected _ = empty
  eof = Text.Sage.eof

instance CharParsing Parser where
  satisfy = Text.Sage.satisfy
  char = Text.Sage.char
  text = Text.Sage.string

instance TokenParsing Parser where
  token p = p <* (someSpace <|> pure ())

instance LookAheadParsing Parser where
  lookAhead = Text.Sage.lookAhead
