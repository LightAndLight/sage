{-# language BangPatterns #-}
{-# language PatternSynonyms #-}
{-# language DeriveGeneric #-}
{-# language MagicHash, UnboxedSums, UnboxedTuples #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -fno-warn-unused-top-binds #-}
module Text.Sage
  ( -- * Parsing
    Parser
  , parse
    -- * Errors
  , Label(..)
  , ParseError(..)
    -- * Spans
  , Span(..), spanContains, spanStart, spanLength -- , spanned
    -- * Combinators
  , label
  , string
  , count
  , skipMany
  , getOffset
  )
where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Control.Monad (MonadPlus(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Exts ((+#), Int(..), Int#, orI#, inline)
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Parser.Token (TokenParsing)

data Label
  = Eof
  | Char Char
  | String String
  | Text Text
  deriving (Eq, Ord, Show, Generic)
instance NFData Label

data ParseError
  = Unexpected
  { position :: Int
  , expected :: Set Label
  } deriving (Eq, Show, Generic)
instance NFData ParseError

type Consumed# = Int#
type Pos# = Int#

type Maybe# a = (# (# #) | a #)

pattern Nothing# :: Maybe# a
pattern Nothing# = (# (# #) | #)

pattern Just# :: a -> Maybe# a
pattern Just# a = (# | a #)

{-# complete Nothing#, Just# #-}

newtype Parser a
  = Parser
  { unParser ::
      (# Text, Pos#, Set Label #) ->
      (# Consumed#, Text, Pos#, Set Label, Maybe# a #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  case p (# input, 0#, mempty #) of
    (# _, _, pos, ex, res #) ->
      case res of
        Nothing# -> Left $ Unexpected (I# pos) ex
        Just# a -> Right a

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input ->
    case p input of
      (# consumed, input', pos', ex', ra #) ->
        (# consumed
        , input'
        , pos'
        , ex'
        , case ra of
            Nothing# -> Nothing#
            Just# a -> Just# (f a)
        #)

instance Applicative Parser where
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
                (# orI# fConsumed aConsumed
                , input''
                , pos''
                , ex''
                , case ra of
                    Nothing# ->
                      Nothing#
                    Just# a ->
                      Just# (f a)
                #)

instance Alternative Parser where
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

  {-# inline many #-}
  many (Parser p) =
    Parser (go 0# id)
    where
      go consumed acc state =
        case p state of
          (# consumed', input', pos', ex', ra #) ->
            let consumed'' = orI# consumed consumed' in
            case ra of
              Nothing# ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> Nothing#
                    _ -> let !acc' = acc [] in Just# acc'
                #)
              Just# a -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

  {-# inline some #-}
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
            let consumed'' = orI# consumed consumed' in
            case ra of
              Nothing# ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> Nothing#
                    _ -> let !acc' = acc [] in Just# acc'
                #)
              Just# a -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

instance Monad Parser where
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

instance MonadPlus Parser

string :: Text -> Parser Text
string t =
  Parser $ \state@(# input, pos, _ #) -> go state t input pos
    where
      go state expect !input' pos' =
        case Text.uncons expect of
          Nothing ->
            (# 1#
            , input'
            , pos'
            , mempty
            , Just# t
            #)
          Just (!expectedC, !expect') ->
            case Text.uncons input' of
              Just (actualC, input'') | expectedC == actualC ->
                go state expect' input'' (pos' +# 1#)
              _ ->
                let
                  !(# input, pos, ex #) = state
                in
                (# 0#, input, pos, Set.insert (Text t) ex, Nothing# #)

count :: Parser a -> Parser Int
count (Parser p) =
  Parser (go 0# 0#)
  where
    go n consumed state =
      case p state of
        (# consumed', input', pos', ex', res #) ->
          let consumed'' = orI# consumed consumed' in
          case res of
            Nothing# ->
              (# consumed''
              , input'
              , pos'
              , ex'
              , case consumed' of
                  1# -> Nothing#
                  _ -> Just# (I# n)
              #)
            Just# _ -> go (1# +# n) consumed'' (# input', pos', ex' #)

skipMany :: Parser a -> Parser ()
skipMany (Parser p) =
  Parser (go 0#)
  where
    go consumed state =
      case p state of
        (# consumed', input', pos', ex', res #) ->
          let consumed'' = orI# consumed consumed' in
          case res of
            Nothing# ->
              (# consumed''
              , input'
              , pos'
              , ex'
              , case consumed' of
                  1# -> Nothing#
                  _ -> Just# ()
              #)
            Just# _ -> go consumed'' (# input', pos', ex' #)

label :: Label -> Parser a -> Parser a
label l (Parser p) =
  Parser $ \(# input, pos, ex #) ->
  case p (# input, pos, ex #) of
    (# consumed, input', pos', _, res #) ->
      (# consumed, input', pos', Set.insert l ex, res #)

instance Parsing Parser where
  try (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', ex', res #) ->
        case res of
          Nothing# ->
            (# 0#, input, pos, ex, res #)
          Just# _ ->
            (# consumed, input', pos', ex', res #)

  (<?>) p n = label (String n) p

  {-# inline skipMany #-}
  skipMany = Text.Sage.skipMany

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
            let consumed'' = orI# consumed consumed' in
            case res of
              Nothing# ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> Nothing#
                    _ -> Just# ()
                #)
              Just# _ -> go consumed'' (# input', pos', ex' #)

  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
        case res of
          Nothing# -> (# 0#, input, pos, ex, Just# () #)
          Just# _ -> (# 0#, input, pos, ex, Nothing# #)

  unexpected _ = empty

  eof =
    Parser $ \(# input, pos, ex #) ->
    if Text.null input
    then (# 0#, input, pos, ex, Just# () #)
    else (# 0#, input, pos, Set.insert Eof ex, Nothing# #)

instance CharParsing Parser where
  {-# inline satisfy #-}
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
    case inline Text.uncons input of
      Just (c, input') | f c ->
        (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, ex, Nothing# #)

  char c =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (!c', input') | c == c' ->
        (# 1#, input', pos +# 1#, mempty, Just# c #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, Nothing# #)

  text = Text.Sage.string

instance TokenParsing Parser

instance LookAheadParsing Parser where
  lookAhead (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
        (# 0#, input, pos, ex, res #)

getOffset :: Parser Int
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
