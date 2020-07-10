{-# language BangPatterns #-}
{-# language DeriveGeneric #-}
{-# language MagicHash, UnboxedSums, UnboxedTuples #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# options_ghc -fno-warn-unused-top-binds #-}
module Text.Sage
  ( Parser
  , Label(..)
  , ParseError(..)
  , parse
  , string
  -- , Span(..), spanContains, spanStart, spanLength
  -- , spanned
  )
where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.Exts (Int#, orI#)
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing

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
type Pos = Int

newtype Parser a
  = Parser
  { unParser ::
      (# Text, Pos, Set Label #) ->
      (# Consumed#, Text, Pos, Set Label, (# (# #) | a #) #)
  }

parse :: Parser a -> Text -> Either ParseError a
parse (Parser p) input =
  case p (# input, 0, mempty #) of
    (# _, _, pos, ex, res #) ->
      case res of
        (# (# #) | #) -> Left $ Unexpected pos ex
        (# | a #) -> Right a

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
            (# (# #) | #) -> (# (# #) | #)
            (# | a #) -> (# | f a #)
        #)

instance Applicative Parser where
  pure a = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, (# | a #) #)
  Parser pf <*> Parser pa =
    Parser $ \input ->
    case pf input of
      (# fConsumed, input', pos', ex', rf #) ->
        case rf of
          (# (# #) | #) ->
            (# fConsumed, input', pos', ex', (# (# #) | #) #)
          (# | f #) ->
            case pa (# input', pos', ex' #) of
              (# aConsumed, input'', pos'', ex'', ra #) ->
                (# orI# fConsumed aConsumed
                , input''
                , pos''
                , ex''
                , case ra of
                    (# (# #) | #) ->
                      (# (# #) | #)
                    (# | a #) ->
                      (# | f a #)
                #)

instance Alternative Parser where
  empty = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, (# (# #) | #) #)

  Parser pa <|> Parser pb =
    Parser $ \(# input, pos, ex #) ->
    case pa (# input, pos, ex #) of
      (# aConsumed, input', pos', ex', ra #) ->
        case ra of
          (# (# #) | #) ->
            case aConsumed of
              1# -> (# aConsumed, input', pos', ex', ra #)
              _ -> pb (# input, pos, ex' #)
          (# | _ #) ->
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
              (# (# #) | #) ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> (# (# #) | #)
                    _ -> (# | acc [] #)
                #)
              (# | a #) -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

  some (Parser p) =
    Parser $ \state ->
    case p state of
      (# consumed, input', pos', ex', ra #) ->
        case ra of
          (# (# #) | #) -> (# consumed, input', pos', ex', (# (# #) | #) #)
          (# | a #) -> go consumed (a :) (# input', pos', ex' #)
    where
      go consumed acc state =
        case p state of
          (# consumed', input', pos', ex', ra #) ->
            let consumed'' = orI# consumed consumed' in
            case ra of
              (# (# #) | #) ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> (# (# #) | #)
                    _ -> (# | acc [] #)
                #)
              (# | a #) -> go consumed'' (acc . (a :)) (# input', pos', ex' #)

string :: Text -> Parser Text
string t =
  Parser $ \(# input, pos, ex #) ->
  let
    ex' = Set.insert (Text t) ex
    go expect !input' !pos' =
      case Text.uncons expect of
        Nothing ->
          (# 1#
          , input'
          , pos'
          , mempty
          , (# | t #)
          #)
        Just (!expectedC, !expect') ->
          case Text.uncons input' of
            Just (actualC, input'') | expectedC == actualC ->
              go expect' input'' (pos' + 1)
            _ ->
              (# 0#, input, pos, ex', (# (# #) | #) #)
   in
     go t input pos

instance Parsing Parser where
  try (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, input', pos', ex', res #) ->
        (# 0#, input', pos', ex', res #)

  (<?>) (Parser p) n =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', _, res #) ->
        (# consumed, input', pos', Set.insert (String n) ex, res #)

  skipMany (Parser p) =
    Parser (go 0#)
    where
      go consumed state =
        case p state of
          (# consumed', input', pos', ex', res #) ->
            let consumed'' = orI# consumed consumed' in
            case res of
              (# (# #) | #) ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> (# (# #) | #)
                    _ -> (# | () #)
                #)
              (# | _ #) -> go consumed'' (# input', pos', ex' #)

  skipSome (Parser p) =
    Parser $ \state ->
    case p state of
      (# consumed, input', pos', ex', res #) ->
        case res of
          (# (# #) | #) -> (# consumed, input', pos', ex', (# (# #) | #) #)
          (# | _ #) -> go consumed (# input', pos', ex' #)
    where
      go consumed state =
        case p state of
          (# consumed', input', pos', ex', res #) ->
            let consumed'' = orI# consumed consumed' in
            case res of
              (# (# #) | #) ->
                (# consumed''
                , input'
                , pos'
                , ex'
                , case consumed' of
                    1# -> (# (# #) | #)
                    _ -> (# | () #)
                #)
              (# | _ #) -> go consumed'' (# input', pos', ex' #)

  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# _, _, _, _, res #) ->
        case res of
          (# (# #) | #) -> (# 0#, input, pos, ex, (# | () #) #)
          (# | _ #) -> (# 0#, input, pos, ex, (# (# #) | #) #)

  unexpected _ = empty

  eof =
    Parser $ \(# input, pos, ex #) ->
    if Text.null input
    then (# 0#, input, pos, ex, (# | () #) #)
    else (# 0#, input, pos, Set.insert Eof ex, (# (# #) | #) #)

instance CharParsing Parser where
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (c, input') | f c ->
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, ex, (# (# #) | #) #)

  char c =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (c', input') | c == c' ->
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, (# (# #) | #) #)
