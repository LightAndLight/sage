{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Text.Sage (
  -- * Parsing
  Parser(..),
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
  spanLength,

  -- * Combinators
  label,
  string,
  count,
  skipMany,
  skipSome,
  getOffset,
  try,
  lookAhead,
  (<?>),
  satisfy,
  notFollowedBy,
  eof,
  char,
  sepBy,
  
  -- * Internal
  Pos#,
  Consumed#,
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

-- | Parse a 'Text'.
parseText :: Parser StreamText a -> Text -> Either ParseError a
parseText p = parse p . StreamText

-- | Parse a UTF-8 encoded 'ByteString'.
parseUtf8 :: Parser StreamUtf8 a -> ByteString -> Either ParseError a
parseUtf8 p = parse p . StreamUtf8

-- | Parse an arbitrary string.
parse :: Parser s a -> s -> Either ParseError a
parse (Parser p) input =
  case p (# input, 0#, mempty #) of
    (# _, _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

-- | A parsing error.
data ParseError = Unexpected
  -- | Byte offset at which the error occurred.
  { position :: Int
  -- | Names for things that the parser would have accepted at the point where it failed.
  , expected :: Set Label
  }
  deriving (Eq, Show, Generic)

instance NFData ParseError

-- | Names for things the parser is expecting.
data Label
  = Eof
  | Char Char
  | String String
  | Text Text
  deriving (Eq, Ord, Show, Generic)

instance NFData Label

-- | A parser that consumes a string of type @s@ and produces a value of type @a@.
newtype Parser s a = Parser
  { unParser ::
      (# s, Pos#, Set Label #) ->
      (# Consumed#, s, Pos#, Set Label, Maybe# a #)
  }

type Consumed# = Int#

type Pos# = Int#


-- | The unboxed equivalent of 'Maybe'.
--
-- Contructors: 'Nothing#' and 'Just#'
type Maybe# a = (# (# #) | a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (# #) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# COMPLETE Nothing#, Just# #-}

instance Functor (Parser s) where
  fmap f (Parser p) =
    Parser $ \input ->
      case p input of
        (# consumed, input', pos', ex', ra #) ->
          let
            !rb =
              case ra of
                Nothing# -> Nothing#
                Just# a -> Just# (f a)
          in
            (# consumed, input', pos', ex', rb #)

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
                  let
                    !bConsumed = orI# fConsumed aConsumed
                    !rb =
                      case ra of
                        Nothing# ->
                          Nothing#
                        Just# a ->
                          Just# (f a)
                  in
                    (# bConsumed, input'', pos'', ex'', rb #)

instance Alternative (Parser s) where
  empty = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, Nothing# #)

  Parser pa <|> Parser pb =
    Parser $ \(# input, pos, ex #) ->
      case pa (# input, pos, ex #) of
        (# aConsumed, input', pos', ex', ra #) ->
          case ra of
            Nothing# ->
              case aConsumed of
                1# ->
                  (# aConsumed, input', pos', ex', ra #)
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
            let !consumed'' = orI# consumed consumed' in
            case ra of
              Nothing# ->
                let
                  !ras =
                    case consumed' of
                      1# -> Nothing#
                      _ -> let !acc' = acc [] in Just# acc'
                in
                (# consumed'', input', pos', ex', ras #)
              Just# a ->
                go consumed'' (acc . (a :)) (# input', pos', ex' #)

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
            let !consumed'' = orI# consumed consumed'
             in case ra of
                  Nothing# ->
                    let
                      !ras =
                        case consumed' of
                          1# -> Nothing#
                          _ -> let !acc' = acc [] in Just# acc'
                    in
                      (# consumed'', input', pos', ex', ras #)
                  Just# a ->
                    go consumed'' (acc . (a :)) (# input', pos', ex' #)

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
                  let !consumed'' = orI# consumed consumed' in
                  (# consumed'', input'', pos'', ex'', rb #)

instance MonadPlus (Parser s)

{-# INLINEABLE string #-}
string :: forall s. (Chars s) => Text -> Parser s Text
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
          let !consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  let
                    !n' =
                      case consumed' of
                        1# -> Nothing#
                        _ -> Just# (I# n)
                  in
                    (# consumed'', input', pos', ex', n' #)
                Just# _ ->
                  go (1# +# n) consumed'' (# input', pos', ex' #)

skipMany :: Parser s a -> Parser s ()
skipMany (Parser p) =
  Parser (go 0#)
  where
    go consumed state =
      case p state of
        (# consumed', input', pos', ex', res #) ->
          let !consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  let
                    !result =
                      case consumed' of
                        1# -> Nothing#
                        _ -> Just# ()
                  in
                    (# consumed'', input', pos', ex', result #)
                Just# _ -> go consumed'' (# input', pos', ex' #)

label :: Label -> Parser s a -> Parser s a
label l (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert l ex, res #)

getOffset :: Parser s Int
getOffset = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, Just# (I# pos) #)

-- | Match a specific character
char :: (Chars s) => Char -> Parser s Char
char c =
  Parser $ \(# input, pos, ex #) ->
    case fromResult $ Chars.uncons input of
      Just (c', input')
        | c == c' ->
            (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, Nothing# #)

sepBy :: Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep =
  (:) <$> p <* sep <*> sepBy p sep <|>
  pure []

-- | Attempt a parser, but never consume input on failure
try :: Parser s a -> Parser s a
try (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', ex', res #) ->
        case res of
          Nothing# ->
            (# 0#, input, pos, ex, res #)
          Just# _ ->
            (# consumed, input', pos', ex', res #)

-- | Look ahead in the input without consuming it
lookAhead :: Parser s a -> Parser s a
lookAhead (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
        (# 0#, input, pos, ex, res #)

-- | Add an expected label to a parser
(<?>) :: Parser s a -> String -> Parser s a
p <?> n = label (String n) p

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

skipSome :: Parser s a -> Parser s ()
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
          let !consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  let
                    !result =
                      case consumed' of
                        1# -> Nothing#
                        _ -> Just# ()
                  in
                    (# consumed'', input', pos', ex', result #)
                Just# _ ->
                  go consumed'' (# input', pos', ex' #)

notFollowedBy :: Parser s a -> Parser s ()
notFollowedBy (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
        case res of
          Nothing# -> (# 0#, input, pos, ex, Just# () #)
          Just# _ -> (# 0#, input, pos, ex, Nothing# #)

eof :: Chars s => Parser s ()
eof =
  Parser $ \(# input, pos, ex #) ->
    case fromResult $ Chars.uncons input of
      Nothing -> (# 0#, input, pos, ex, Just# () #)
      Just{} -> (# 0#, input, pos, Set.insert Eof ex, Nothing# #)

satisfy :: Chars s => (Char -> Bool) -> Parser s Char
satisfy f =
  Parser $ \(# input, pos, ex #) ->
    case fromResult $ Chars.uncons input of
      Just (c, input')
        | f c ->
            (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, ex, Nothing# #)

