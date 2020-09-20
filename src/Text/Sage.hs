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
  , skipMany
  , getOffset
  , Span(..), spanContains, spanStart, spanLength
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
import GHC.Exts (Int#, orI#, inline)
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
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

  {-# inline some #-}
  some (Parser p) =
    Parser $ \(# !input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, !input', pos', ex', ra #) ->
        case ra of
          (# (# #) | #) -> (# consumed, input', pos', ex', (# (# #) | #) #)
          (# | a #) -> go consumed (a :) (# input', pos', ex' #)
    where
      go consumed acc (# !input, pos, ex #) =
        case p (# input, pos, ex #) of
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

instance Monad Parser where
  Parser p >>= f =
    Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, input', pos', ex', ra #) ->
        case ra of
          (# (# #) | #) ->
            (# consumed, input', pos', ex', (# (# #) | #) #)
          (# | a #) ->
            case unParser (f a) (# input', pos', ex' #) of
              (# consumed', input'', pos'', ex'', rb #) ->
                (# orI# consumed consumed', input'', pos'', ex'', rb #)

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

skipMany :: Parser a -> Parser ()
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

  {-# inline skipMany #-}
  skipMany = Text.Sage.skipMany

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
  {-# inline satisfy #-}
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
    case inline Text.uncons input of
      Just (c, input') | f c ->
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, ex, (# (# #) | #) #)

  char !c =
    Parser $ \(# input, pos, ex #) ->
    case Text.uncons input of
      Just (!c', input') | c == c' ->
        let !pos' = pos + 1 in
        (# 1#, input', pos', mempty, (# | c #) #)
      _ ->
        (# 0#, input, pos, Set.insert (Char c) ex, (# (# #) | #) #)

  text = Text.Sage.string

instance TokenParsing Parser

getOffset :: Parser Int
getOffset = Parser $ \(# input, pos, ex #) -> (# 0#, input, pos, ex, (# | pos #) #)

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
