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
  Parser (..),
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

import Control.Applicative (Alternative (..), optional)
import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus (..))
import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Exts (Int (..), Int#, orI#, (+#), Addr#, Char#, Char (..), eqChar#, (-#))
import GHC.Generics (Generic)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents (..))
import Data.ByteString.Internal (toForeignPtr0, fromForeignPtr)
import qualified Text.Sage.Utf8 as Utf8

type ByteString# = (# Addr#, Int# #)

toByteString# :: ByteString -> ByteString#
toByteString# bs =
  let !(ForeignPtr addr _, I# len) = toForeignPtr0 bs in
  (# addr, len #)

uncons :: ByteString# -> Pos# -> (# (# #) | (# Char#, Int# #) #)
uncons (# addr, len #) pos =
  case Utf8.uncons (fromForeignPtr (ForeignPtr addr FinalPtr) (I# pos) (I# (len -# pos))) of
    Utf8.Done -> (# (# #) | #)
    Utf8.More c i -> (# | (# c, i #) #)

-- | Parse a UTF-8 encoded 'ByteString'.
parse :: Parser a -> ByteString -> Either ParseError a
parse (Parser p) bs =
  let !input = toByteString# bs in
  case p (# input, 0#, mempty #) of
    (# _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

-- | A parsing error.
data ParseError = Unexpected
  { position :: Int
  -- ^ Byte offset at which the error occurred.
  , expected :: Set Label
  -- ^ Names for things that the parser would have accepted at the point where it failed.
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
newtype Parser a = Parser
  { unParser ::
      (# ByteString#, Pos#, Set Label #) ->
      (# Consumed#, Pos#, Set Label, Maybe# a #)
  }

type Consumed# = Int#

type Pos# = Int#

{- | The unboxed equivalent of 'Maybe'.

Contructors: 'Nothing#' and 'Just#'
-}
type Maybe# a = (# (# #) | a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (# #) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# COMPLETE Nothing#, Just# #-}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input ->
      case p input of
        (# consumed, pos', ex', ra #) ->
          let
            !rb =
              case ra of
                Nothing# -> Nothing#
                Just# a -> Just# (f a)
          in
            (# consumed, pos', ex', rb #)

instance Applicative Parser where
  pure a = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Just# a #)
  Parser pf <*> Parser pa =
    Parser $ \state@(# input, _, _ #) ->
      case pf state of
        (# fConsumed, pos', ex', rf #) ->
          case rf of
            Nothing# ->
              (# fConsumed, pos', ex', Nothing# #)
            Just# f ->
              case pa (# input, pos', ex' #) of
                (# aConsumed, pos'', ex'', ra #) ->
                  let
                    !bConsumed = orI# fConsumed aConsumed
                    !rb =
                      case ra of
                        Nothing# ->
                          Nothing#
                        Just# a ->
                          Just# (f a)
                  in
                    (# bConsumed, pos'', ex'', rb #)

instance Alternative Parser where
  empty = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Nothing# #)

  Parser pa <|> Parser pb =
    Parser $ \state@(# input, _, _ #) ->
      case pa state of
        (# aConsumed, pos', ex', ra #) ->
          case ra of
            Nothing# ->
              case aConsumed of
                1# ->
                  (# aConsumed, pos', ex', ra #)
                _ ->
                  pb (# input, pos', ex' #)
            Just# _ ->
              (# aConsumed, pos', ex', ra #)

  {-# INLINE many #-}
  many (Parser p) =
    Parser (go 0# id)
    where
      go consumed acc (# input, pos, ex #) =
        case p (# input, pos, ex #) of
          (# consumed', pos', ex', ra #) ->
            let
              !consumed'' = orI# consumed consumed'
            in
              case ra of
                Nothing# ->
                  let
                    !ras =
                      case consumed' of
                        1# -> Nothing#
                        _ -> let !acc' = acc [] in Just# acc'
                  in
                    (# consumed'', pos', ex', ras #)
                Just# a ->
                  go consumed'' (acc . (a :)) (# input, pos', ex' #)

  {-# INLINE some #-}
  some (Parser p) =
    Parser $ \state@(# input, _, _ #) ->
      case p state of
        (# consumed, pos', ex', ra #) ->
          case ra of
            Nothing# -> (# consumed, pos', ex', Nothing# #)
            Just# a -> go consumed (a :) (# input, pos', ex' #)
    where
      go consumed acc state@(# input, _, _ #) =
        case p state of
          (# consumed', pos', ex', ra #) ->
            let
              !consumed'' = orI# consumed consumed'
            in
              case ra of
                Nothing# ->
                  let
                    !ras =
                      case consumed' of
                        1# -> Nothing#
                        _ -> let !acc' = acc [] in Just# acc'
                  in
                    (# consumed'', pos', ex', ras #)
                Just# a ->
                  go consumed'' (acc . (a :)) (# input, pos', ex' #)

instance Monad Parser where
  Parser p >>= f =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, pos', ex', ra #) ->
          case ra of
            Nothing# ->
              (# consumed, pos', ex', Nothing# #)
            Just# a ->
              case unParser (f a) (# input, pos', ex' #) of
                (# consumed', pos'', ex'', rb #) ->
                  let
                    !consumed'' = orI# consumed consumed'
                  in
                    (# consumed'', pos'', ex'', rb #)

instance MonadPlus Parser

{-# INLINEABLE string #-}
string :: Text -> Parser Text
string t =
  Parser $ \state@(# _input, pos, _ #) -> stringGo state t t pos
  where
    stringGo ::
      (# ByteString#, Pos#, Set Label #) ->
      Text ->
      Text ->
      Pos# ->
      (# Consumed#, Pos#, Set Label, Maybe# Text #)
    stringGo state@(# input, _, _ #) t' expect pos' =
      -- passing around t' prevents some re-boxing
      case Text.uncons expect of
        Nothing ->
          (# 1#, pos', mempty, Just# t' #)
        Just (C# expectedC, !expect') ->
          case uncons input pos' of
            (# | (# actualC, offset #) #)
              | 1# <- expectedC `eqChar#` actualC ->
                  stringGo state t' expect' (pos' +# offset)
            _ ->
              let
                !(# _, pos, ex #) = state
              in
                (# 0#, pos, Set.insert (Text t') ex, Nothing# #)

count :: Parser a -> Parser Int
count (Parser p) =
  Parser (go 0# 0#)
  where
    go n consumed state@(# input, _, _ #) =
      case p state of
        (# consumed', pos', ex', res #) ->
          let
            !consumed'' = orI# consumed consumed'
          in
            case res of
              Nothing# ->
                let
                  !n' =
                    case consumed' of
                      1# -> Nothing#
                      _ -> Just# (I# n)
                in
                  (# consumed'', pos', ex', n' #)
              Just# _ ->
                go (1# +# n) consumed'' (# input, pos', ex' #)

skipMany :: Parser a -> Parser ()
skipMany (Parser p) =
  Parser (go 0#)
  where
    go consumed state@(# input, _, _ #) =
      case p state of
        (# consumed', pos', ex', res #) ->
          let
            !consumed'' = orI# consumed consumed'
          in
            case res of
              Nothing# ->
                let
                  !result =
                    case consumed' of
                      1# -> Nothing#
                      _ -> Just# ()
                in
                  (# consumed'', pos', ex', result #)
              Just# _ -> go consumed'' (# input, pos', ex' #)

label :: Label -> Parser a -> Parser a
label l (Parser p) =
  Parser $ \state@(# _input, _, ex #) ->
    case p state of
      (# consumed, pos', _, res #) ->
        (# consumed, pos', Set.insert l ex, res #)

getOffset :: Parser Int
getOffset = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Just# (I# pos) #)

-- | Match a specific character
{-# INLINEABLE char #-}
char :: Char -> Parser Char
char c@(C# c#) =
  Parser $ \(# input, pos, ex #) ->
    case uncons input pos of
      (# | (# c', offset #) #)
        | 1# <- c# `eqChar#` c' ->
            (# 1#, pos +# offset, mempty, Just# c #)
      _ ->
        (# 0#, pos, Set.insert (Char c) ex, Nothing# #)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep =
  ((\x -> maybe [x] (x :)) <$> p <*> optional (sep *> sepBy p sep))
    <|> pure []

-- | Attempt a parser, but never consume input on failure
try :: Parser a -> Parser a
try (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, pos', ex', res #) ->
        case res of
          Nothing# ->
            (# 0#, pos, ex, res #)
          Just# _ ->
            (# consumed, pos', ex', res #)

-- | Look ahead in the input without consuming it
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, res #) ->
        (# 0#, pos, ex, res #)

-- | Add an expected label to a parser
(<?>) :: Parser a -> String -> Parser a
p <?> n = label (String n) p

infixl 2 <?>

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

skipSome :: Parser a -> Parser ()
skipSome (Parser p) =
  Parser $ \state@(# input, _, _ #) ->
    case p state of
      (# consumed, pos', ex', res #) ->
        case res of
          Nothing# -> (# consumed, pos', ex', Nothing# #)
          Just# _ -> go consumed (# input, pos', ex' #)
  where
    go consumed state@(# input, _, _ #) =
      case p state of
        (# consumed', pos', ex', res #) ->
          let
            !consumed'' = orI# consumed consumed'
          in
            case res of
              Nothing# ->
                let
                  !result =
                    case consumed' of
                      1# -> Nothing#
                      _ -> Just# ()
                in
                  (# consumed'', pos', ex', result #)
              Just# _ ->
                go consumed'' (# input, pos', ex' #)

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, res #) ->
        case res of
          Nothing# -> (# 0#, pos, ex, Just# () #)
          Just# _ -> (# 0#, pos, ex, Nothing# #)

eof :: Parser ()
eof =
  Parser $ \(# input, pos, ex #) ->
    case uncons input pos of
      (# (# #) | #) -> (# 0#, pos, ex, Just# () #)
      (# | _ #) -> (# 0#, pos, Set.insert Eof ex, Nothing# #)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser $ \(# input, pos, ex #) ->
    case uncons input pos of
      (# | (# c#, offset #) #)
        | let c = C# c#, f c ->
            (# 1#, pos +# offset, mempty, Just# c #)
      _ ->
        (# 0#, pos, ex, Nothing# #)
