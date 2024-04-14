{-# OPTIONS_GHC -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes #-}
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
{-# LANGUAGE TypeApplications #-}

module Text.Sage (
  -- * Parsing
  Parser,
  -- parseText,
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
  takeWhile,
  takeWhile1,
  skipWhile,
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
import GHC.Exts (Int (..), Int#, orI#, (+#), Addr#, (>#), Char (..), eqChar#, runRW#, Ptr (..), plusAddr#, (-#))
import GHC.Generics (Generic)
import Streaming.Chars (Chars)
import qualified Streaming.Chars as Chars
import Streaming.Chars.ByteString.Utf8 (StreamUtf8 (StreamUtf8))
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as CharParsing
import Text.Parser.Combinators (Parsing)
import qualified Text.Parser.Combinators as Parsing
import Text.Parser.LookAhead (LookAheadParsing (..))
import Text.Parser.Token (TokenParsing (..))
import Data.Text.Short (ShortText)
import Data.Text.Short.Unsafe (fromShortByteStringUnsafe)
import Data.ByteString.Short (ShortByteString(..))
import Data.Primitive.ByteArray (newByteArray, MutableByteArray (..), ByteArray (..), unsafeFreezeByteArray, copyPtrToMutableByteArray)
import GHC.IO (IO(..), noDuplicate)
import Data.Word (Word8)
import Prelude hiding (takeWhile)

{-
-- | Parse a 'Text'.
parseText :: Parser StreamText a -> Text -> Either ParseError a
parseText p = parse p . StreamText
-}

-- | Parse a UTF-8 encoded 'ByteString'.
parseUtf8 :: Parser StreamUtf8 a -> ByteString -> Either ParseError a
parseUtf8 p = parse p . StreamUtf8

-- | Parse an arbitrary string.
parse :: Chars s => Parser s a -> s -> Either ParseError a
parse (Parser p) input =
  Chars.unsafeWithPinned input $
  \bs ->
  case p (# bs, 0#, mempty #) of
    (# _, pos, ex, res #) ->
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
  | Char {-# unpack #-} !Char
  | String String
  | Text Text
  deriving (Eq, Ord, Show, Generic)

instance NFData Label

-- | A parser that consumes a string of type @s@ and produces a value of type @a@.
newtype Parser s a = Parser
  { unParser ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      (# Consumed#, Pos#, Set Label, Maybe# a #)
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
        (# consumed, pos', ex', ra #) ->
          let
            !rb =
              case ra of
                Nothing# -> Nothing#
                Just# a -> Just# (f a)
          in
            (# consumed, pos', ex', rb #)

instance Applicative (Parser s) where
  pure a = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Just# a #)
  Parser pf <*> Parser pa =
    Parser $ \(# input, pos, ex #) ->
      case pf (# input, pos, ex #) of
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

instance Alternative (Parser s) where
  empty = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Nothing# #)

  Parser pa <|> Parser pb =
    Parser $ \(# input, pos, ex #) ->
      case pa (# input, pos, ex #) of
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
            let !consumed'' = orI# consumed consumed' in
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
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, pos', ex', ra #) ->
          case ra of
            Nothing# -> (# consumed, pos', ex', Nothing# #)
            Just# a -> go consumed (a :) (# input, pos', ex' #)
    where
      go consumed acc (# input, pos, ex #) =
        case p (# input, pos, ex #) of
          (# consumed', pos', ex', ra #) ->
            let !consumed'' = orI# consumed consumed'
             in case ra of
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

instance Monad (Parser s) where
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
                  let !consumed'' = orI# consumed consumed' in
                  (# consumed'', pos'', ex'', rb #)

instance MonadPlus (Parser s)

{-# INLINEABLE string #-}
string :: forall s. (Chars s) => Text -> Parser s Text
string t =
  Parser $ \state@(# _, pos, _ #) -> stringGo state t t pos
  where
    stringGo ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      Text ->
      Text ->
      Pos# ->
      (# Consumed#, Pos#, Set Label, Maybe# Text #)
    stringGo state@(# input, _, _ #) t' expect pos' =
      -- passing around t' prevents some re-boxing
      case Text.uncons expect of
        Nothing ->
          (# 1#, pos', mempty, Just# t' #)
        Just (!expectedC, !expect') ->
          case Chars.uncons @s input pos' of
            (# offset, actualC #)
              | 1# <- offset ># 0#, expectedC == C# actualC ->
                  stringGo state t' expect' (pos' +# offset)
              | otherwise ->
                  let !(# _, pos, ex #) = state
                   in (# 0#, pos, Set.insert (Text t') ex, Nothing# #)

count :: Parser s a -> Parser s Int
count (Parser p) =
  Parser (go 0# 0#)
  where
    go n consumed (# input, pos, ex #) =
      case p (# input, pos, ex #) of
        (# consumed', pos', ex', res #) ->
          let !consumed'' = orI# consumed consumed'
           in case res of
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

skipMany :: Parser s a -> Parser s ()
skipMany (Parser p) =
  Parser (go 0#)
  where
    go consumed (# input, pos, ex #) =
      case p (# input, pos, ex #) of
        (# consumed', pos', ex', res #) ->
          let !consumed'' = orI# consumed consumed'
           in case res of
                Nothing# ->
                  let
                    !result =
                      case consumed' of
                        1# -> Nothing#
                        _ -> Just# ()
                  in
                    (# consumed'', pos', ex', result #)
                Just# _ -> go consumed'' (# input, pos', ex' #)

{-# inline unsafeDupablePerformIO' #-}
unsafeDupablePerformIO'  :: IO a -> a
unsafeDupablePerformIO' (IO m) = case runRW# m of (# _, a #) -> a

{-# inline unsafePerformIO' #-}
unsafePerformIO' :: IO a -> a
unsafePerformIO' m = unsafeDupablePerformIO' (noDuplicate >> m)

{-# inline skipWhile #-}
skipWhile :: forall s. Chars s => (Char -> Bool) -> Parser s ()
skipWhile f =
  Parser $ \(# input, pos, ex #) ->
  let 
    !(# consumed', pos', ex', result# #) =
      go (# input, pos, Set.insert l ex #) 0#
  in
  case result# of
    Nothing# ->
      (# consumed', pos', ex', Nothing# #)
    Just# () ->
      (# consumed', pos', ex', Just# () #)
  where
    l = String "character (skipWhile)"
    
    go ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      Consumed# ->
      (# Consumed#, Pos#, Set Label, Maybe# () #)
    go (# input, pos, ex #) consumed =
      case Chars.uncons @s input pos of
        (# off, c #)
          | 1# <- off ># 0#, f (C# c) ->
              go (# input, pos +# off, Set.singleton l #) 1#
          | otherwise ->
              (# consumed, pos, ex, Just# () #)

-- | @'takeWhile' f@ is equivalent to @'ShortText.pack' '<$>' 'many' ('satisfy' f)@
takeWhile :: forall s. Chars s => (Char -> Bool) -> Parser s ShortText
takeWhile f =
  Parser $ \(# input@(# addr, _ #), pos, ex #) ->
  let 
    !(# consumed', pos', ex', result# #) =
      go (# input, pos, Set.insert l ex #) 0#
  in
  case result# of
    Nothing# ->
      (# consumed', pos', ex', Nothing# #)
    Just# () ->
      let !size = pos' -# pos in
      let
        !(ByteArray ba) = unsafePerformIO' (do
            MutableByteArray mba <- newByteArray (I# size)
            copyPtrToMutableByteArray
              (MutableByteArray mba)
              (I# 0#)
              (Ptr (plusAddr# addr pos) :: Ptr Word8)
              (I# size)
            unsafeFreezeByteArray (MutableByteArray mba)
            )
      in
      let !result = fromShortByteStringUnsafe (SBS ba) in
      (# consumed', pos', ex', Just# result #)
  where
    l = String "character (takeWhile)"
    
    go ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      Consumed# ->
      (# Consumed#, Pos#, Set Label, Maybe# () #)
    go (# input, pos, ex #) consumed =
      case Chars.uncons @s input pos of
        (# off, c #)
          | 1# <- off ># 0#, f (C# c) ->
              go (# input, pos +# off, Set.singleton l #) 1#
          | otherwise ->
              (# consumed, pos, ex, Just# () #)

-- | @'takeWhile1' f@ is equivalent to @'ShortText.pack' '<$>' 'some' ('satisfy' f)@
{-# inline takeWhile1 #-}
takeWhile1 :: forall s. Chars s => (Char -> Bool) -> Parser s ShortText
takeWhile1 f =
  Parser $ \(# input@(# addr, _ #), pos, ex #) ->
  let 
    !(# consumed', pos', ex', result# #) =
      go0 (# input, pos, Set.insert l ex #)
  in
  case result# of
    Nothing# ->
      (# consumed', pos', ex', Nothing# #)
    Just# () ->
      let !size = pos' -# pos in
      let
        !(ByteArray ba) = unsafePerformIO' (do
            MutableByteArray mba <- newByteArray (I# size)
            copyPtrToMutableByteArray
              (MutableByteArray mba)
              (I# 0#)
              (Ptr (plusAddr# addr pos) :: Ptr Word8)
              (I# size)
            unsafeFreezeByteArray (MutableByteArray mba)
            )
      in
      let !result = fromShortByteStringUnsafe (SBS ba) in
      (# consumed', pos', ex', Just# result #)
  where
    l = String "character (takeWhile1)"

    go0 ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      (# Consumed#, Pos#, Set Label, Maybe# () #)
    go0 (# input, pos, ex #) =
      case Chars.uncons @s input pos of
        (# off, c #)
          | 1# <- off ># 0#, f (C# c) ->
                go (# input, pos +# off, Set.singleton l #)
          | otherwise ->
              (# 0#, pos, ex, Nothing# #)
    
    go ::
      (# (# Addr#, Int# #), Pos#, Set Label #) ->
      (# Consumed#, Pos#, Set Label, Maybe# () #)
    go (# input, pos, ex #) =
      case Chars.uncons @s input pos of
        (# off, c #)
          | 1# <- off ># 0#, f (C# c) ->
                go (# input, pos +# off, ex #)
          | otherwise ->
              (# 1#, pos, ex, Just# () #)

label :: Label -> Parser s a -> Parser s a
label l (Parser p) =
  Parser $ \(# input, pos, ex #) ->
    case p (# input, pos, ex #) of
      (# consumed, pos', _, res #) ->
        (# consumed, pos', Set.insert l ex, res #)

instance (Chars s) => Parsing (Parser s) where
  {-# INLINEABLE try #-}
  try (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# consumed, pos', ex', res #) ->
          case res of
            Nothing# ->
              (# 0#, pos, ex, res #)
            Just# _ ->
              (# consumed, pos', ex', res #)

  {-# INLINEABLE (<?>) #-}
  (<?>) p n = label (String n) p

  {-# INLINE skipMany #-}
  skipMany = Text.Sage.skipMany

  {-# INLINEABLE skipSome #-}
  skipSome (Parser p) =
    Parser $ \(# input, pos, ex #)->
      case p (# input, pos, ex #) of
        (# consumed, pos', ex', res #) ->
          case res of
            Nothing# -> (# consumed, pos', ex', Nothing# #)
            Just# _ -> go consumed (# input, pos', ex' #)
    where
      go consumed (# input, pos, ex #) =
        case p (# input, pos, ex #) of
          (# consumed', pos', ex', res #) ->
            let !consumed'' = orI# consumed consumed'
             in case res of
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

  {-# INLINEABLE notFollowedBy #-}
  notFollowedBy (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# _, _, _, res #) ->
          case res of
            Nothing# -> (# 0#, pos, ex, Just# () #)
            Just# _ -> (# 0#, pos, ex, Nothing# #)

  {-# INLINEABLE unexpected #-}
  unexpected _ = empty

  {-# INLINEABLE eof #-}
  eof =
    Parser $ \(# input, pos, ex #) ->
      case Chars.uncons @s input pos of
        (# offset, _c #)
          | 0# <- offset ->
              (# 0#, pos, ex, Just# () #)
          | otherwise ->
              (# 0#, pos, Set.insert Eof ex, Nothing# #)

instance (Chars s) => CharParsing (Parser s) where
  {-# INLINE satisfy #-}
  satisfy f =
    Parser $ \(# input, pos, ex #) ->
      case Chars.uncons @s input pos of
        (# offset, c# #)
          | 1# <- offset ># 0#, let c = C# c#, f c ->
              (# 1#, pos +# offset, mempty, Just# c #)
          | otherwise ->
              (# 0#, pos, ex, Nothing# #)

  {-# inline char #-}
  char x@(C# x#) =
    Parser (go x x#)
    where
      go c c# (# input, pos, ex #) =
        case Chars.uncons @s input pos of
          (# offset, c'# #)
            | 1# <- offset ># 0# , 1# <- eqChar# c# c'# ->
                (# 1#, pos +# offset, mempty, Just# c #)
            | otherwise ->
                (# 0#, pos, Set.insert (Char (C# c#)) ex, Nothing# #)

  {-# INLINEABLE text #-}
  text = Text.Sage.string

instance (Chars s) => TokenParsing (Parser s) where
  {-# INLINEABLE token #-}
  token p = p <* (someSpace <|> pure ())

instance (Chars s) => LookAheadParsing (Parser s) where
  {-# INLINEABLE lookAhead #-}
  lookAhead (Parser p) =
    Parser $ \(# input, pos, ex #) ->
      case p (# input, pos, ex #) of
        (# _,  _, _, res #) ->
          (# 0#, pos, ex, res #)

getOffset :: Parser s Int
getOffset = Parser $ \(# _input, pos, ex #) -> (# 0#, pos, ex, Just# (I# pos) #)

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
