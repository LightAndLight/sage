{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Text.Sage (
  -- * Parsing
  Parser,
  parseText,
  parseUtf8,
  parse,

  -- * Errors
  Label (..),
  ParseError (..),

  -- * Spans
  Span (..),
  spanContains,
  spanStart,
  spanLength, -- , spanned

  -- * Combinators
  label,
  string,
  count,
  skipMany,
  getOffset,
) where

import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus (..))
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts (Int (..), Int#, orI#, (+#))
import GHC.Generics (Generic)
import Streaming.Chars (Chars, fromResult)
import qualified Streaming.Chars as Chars
import Streaming.Chars.ByteString.Utf8 (StreamUtf8 (StreamUtf8))
import Streaming.Chars.Text (StreamText (StreamText))
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.LookAhead (LookAheadParsing (..))
import Text.Parser.Token (TokenParsing (..))

data Label
  = Eof
  | Char Char
  | String String
  | Text Text
  deriving (Eq, Ord, Show, Generic)

instance NFData Label

data ParseError = Unexpected
  { position :: Int
  , expected :: Set Label
  }
  deriving (Eq, Show, Generic)

instance NFData ParseError

type Consumed# = Int#

type Pos# = Int#

type Maybe# a = (# (# #)| a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (##) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# COMPLETE Nothing#, Just# #-}

newtype Parser s a = Parser
  { unParser ::
      (# s, Pos#, Set Label #) ->
      (# Consumed#, s, Pos#, Set Label, Maybe# a #)
  }

parseText :: Parser StreamText a -> Text -> Either ParseError a
parseText p = parse p . StreamText

parseUtf8 :: Parser StreamUtf8 a -> ByteString -> Either ParseError a
parseUtf8 p = parse p . StreamUtf8

parse :: Parser s a -> s -> Either ParseError a
parse (Parser p) input =
  case p (# input, 0#, mempty #) of
    (# _, _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

instance Functor (Parser s) where
  fmap f (Parser p) =
    Parser $ \input ->
      case p input of
        (# consumed, input', pos', ex', ra #) ->
          (#
            consumed
            , input'
            , pos'
            , ex'
            , case ra of
                Nothing# -> Nothing#
                Just# a -> Just# (f a)
          #)

instance Applicative (Parser s) where
  pure a = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, Just# a #)
  Parser pf <*> Parser pa =
    Parser $ \input ->
      case pf input of
        (# fConsumed, input', pos', ex', rf #) ->
          case rf of
            Nothing# ->
              (# fConsumed, input', pos', ex', Nothing# #)
            Just# f ->
              case pa (# input', pos', ex' #) of
                (# aConsumed, input'', pos'', ex'', ra #) ->
                  (#
                    orI# fConsumed aConsumed
                    , input''
                    , pos''
                    , ex''
                    , case ra of
                        Nothing# ->
                          Nothing#
                        Just# a ->
                          Just# (f a)
                  #)

instance Alternative (Parser s) where
  empty = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, Nothing# #)

  Parser pa <|> Parser pb =
    Parser $ \(# input, pos, ex #) ->
      case pa (# input, pos, ex #) of
        (# aConsumed, input', pos', ex', ra #) ->
          case ra of
            Nothing# ->
              case aConsumed of
                1# -> (# aConsumed, input', pos', ex', ra #)
                _ ->
                  pb (# input', pos', ex' #)
            Just# _ ->
              (# aConsumed, input', pos', ex', ra #)

  {-# INLINE many #-}
  many (Parser p) =
    Parser (go 0# id)
    where
      go consumed acc state =
        case p state of
          (# consumed', input', pos', ex', ra #) ->
            let consumed'' = orI# consumed consumed'
             in case ra of
                  Nothing# ->
                    (#
                      consumed''
                      , input'
                      , pos'
                      , ex'
                      , case consumed' of
                          1# -> Nothing#
                          _ -> let !acc' = acc [] in Just# acc'
                    #)
                  Just# a -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

  {-# INLINE some #-}
  some (Parser p) =
    Parser $ \(# !input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, !input', pos', ex', ra #) ->
          case ra of
            Nothing# -> (# consumed, input', pos', ex', Nothing# #)
            Just# a -> go consumed (a :) (# input', pos', ex' #)
    where
      go consumed acc (# !input, pos, ex #) =
        case p (# input, pos, ex #) of
          (# consumed', input', pos', ex', ra #) ->
            let consumed'' = orI# consumed consumed'
             in case ra of
                  Nothing# ->
                    (#
                      consumed''
                      , input'
                      , pos'
                      , ex'
                      , case consumed' of
                          1# -> Nothing#
                          _ -> let !acc' = acc [] in Just# acc'
                    #)
                  Just# a -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

instance Monad (Parser s) where
  Parser p >>= f =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, input', pos', ex', ra #) ->
          case ra of
            Nothing# ->
              (# consumed, input', pos', ex', Nothing# #)
            Just# a ->
              case unParser (f a) (# input', pos', ex' #) of
                (# consumed', input'', pos'', ex'', rb #) ->
                  (# orI# consumed consumed', input'', pos'', ex'', rb #)

instance MonadPlus (Parser s)

{-# INLINEABLE string #-}
string :: forall s. Chars s => Text -> Parser s Text
string t =
  Parser $ \state@(# input, pos, _ #) -> stringGo state t t input pos
  where
    stringGo ::
      (# s, Pos#, Set Label #) ->
      Text ->
      Text ->
      s ->
      Pos# ->
      (# Consumed#, s, Pos#, Set Label, Maybe# Text #)
    stringGo state t' expect !input' pos' =
      -- passing around t' prevents some re-boxing
      case Text.uncons expect of
        Nothing ->
          (#
            1#
            , input'
            , pos'
            , mempty
            , Just# t'
          #)
        Just (!expectedC, !expect') ->
          case fromResult $ Chars.uncons input' of
            Just (actualC, input'')
              | expectedC == actualC ->
                stringGo state t' expect' input'' (pos' +# 1#)
            _ ->
              let !(# input, pos, ex #) = state
               in (# 0#, input, pos, Set.insert (Text t') ex, Nothing# #)

count :: Parser s a -> Parser s Int
count (Parser p) =
  Parser (go 0# 0#)
  where
    go n consumed state =
      case p state of
        (# consumed', input', pos', ex', res #) ->
          let consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  (#
                    consumed''
                    , input'
                    , pos'
                    , ex'
                    , case consumed' of
                        1# -> Nothing#
                        _ -> Just# (I# n)
                  #)
                Just# _ -> go (1# +# n) consumed'' (# input', pos', ex' #)

skipMany :: Parser s a -> Parser s ()
skipMany (Parser p) =
  Parser (go 0#)
  where
    go consumed state =
      case p state of
        (# consumed', input', pos', ex', res #) ->
          let consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  (#
                    consumed''
                    , input'
                    , pos'
                    , ex'
                    , case consumed' of
                        1# -> Nothing#
                        _ -> Just# ()
                  #)
                Just# _ -> go consumed'' (# input', pos', ex' #)

label :: Label -> Parser s a -> Parser s a
label l (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert l ex, res #)

instance Chars s => Parsing (Parser s) where
  {-# INLINEABLE try #-}
  try (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, input', pos', ex', res #) ->
          case res of
            Nothing# ->
              (# 0#, input, pos, ex, res #)
            Just# _ ->
              (# consumed, input', pos', ex', res #)

  {-# INLINEABLE (<?>) #-}
  (<?>) p n = label (String n) p

  {-# INLINE skipMany #-}
  skipMany = Text.Sage.skipMany

  {-# INLINEABLE skipSome #-}
  skipSome (Parser p) =
    Parser $ \state ->
      case p state of
        (# consumed, input', pos', ex', res #) ->
          case res of
            Nothing# -> (# consumed, input', pos', ex', Nothing# #)
            Just# _ -> go consumed (# input', pos', ex' #)
    where
      go consumed state =
        case p state of
          (# consumed', input', pos', ex', res #) ->
            let consumed'' = orI# consumed consumed'
             in case res of
                  Nothing# ->
                    (#
                      consumed''
                      , input'
                      , pos'
                      , ex'
                      , case consumed' of
                          1# -> Nothing#
                          _ -> Just# ()
                    #)
                  Just# _ -> go consumed'' (# input', pos', ex' #)

  {-# INLINEABLE notFollowedBy #-}
  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# _, _, _, _, res #) ->
          case res of
            Nothing# -> (# 0#, input, pos, ex, Just# () #)
            Just# _ -> (# 0#, input, pos, ex, Nothing# #)

  {-# INLINEABLE unexpected #-}
  unexpected _ = empty

  {-# INLINEABLE eof #-}
  eof =
    Parser $ \(# input, pos, ex #) ->
      case fromResult $ Chars.uncons input of
        Nothing -> (# 0#, input, pos, ex, Just# () #)
        Just{} -> (# 0#, input, pos, Set.insert Eof ex, Nothing# #)

instance Chars s => CharParsing (Parser s) where
  {-# INLINE satisfy #-}
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
      case fromResult $ Chars.uncons input of
        Just (c, input')
          | f c ->
            (# 1#, input', pos +# 1#, mempty, Just# c #)
        _ ->
          (# 0#, input, pos, ex, Nothing# #)

  {-# INLINEABLE char #-}
  char c =
    Parser $ \(# input, pos, ex #) ->
      case fromResult $ Chars.uncons input of
        Just (c', input')
          | c == c' ->
            (# 1#, input', pos +# 1#, mempty, Just# c #)
        _ ->
          (# 0#, input, pos, Set.insert (Char c) ex, Nothing# #)

  {-# INLINEABLE text #-}
  text = Text.Sage.string

instance Chars s => TokenParsing (Parser s) where
  {-# INLINEABLE token #-}
  token p = p <* (someSpace <|> pure ())

instance Chars s => LookAheadParsing (Parser s) where
  {-# INLINEABLE lookAhead #-}
  lookAhead (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# _, _, _, _, res #) ->
          (# 0#, input, pos, ex, res #)

getOffset :: Parser s Int
getOffset = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, Just# (I# pos) #)

data Span = Span {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

spanContains :: Span -> Span -> Bool
spanContains (Span p l) (Span p' l') =
  case compare p p' of
    LT -> p + l >= p' + l'
    EQ -> l >= l'
    GT -> False

-- | `Span` is a meet semilattice with respect to the `spanContains` ordering
instance Semigroup Span where
  Span p l <> Span p' l' =
    case compare p p' of
      LT -> Span p (max (p + l) (p' + l') - p)
      EQ -> Span p (max l l')
      GT -> Span p' (max (p + l) (p' + l') - p')

spanStart :: Span -> Int
spanStart (Span s _) = s

spanLength :: Span -> Int
spanLength (Span _ l) = l
