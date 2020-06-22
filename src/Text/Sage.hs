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
  , char
  , text
  , symbol
  , digit
  , decimal
  , eof
  , lower
  , matchMany
  , matchSome
  , try
  , Predicate(..)
  , pDigit
  , pLower
  , pUpper
  , satisfy
  , takeWhile1
  , sepBy
  , between
  , (<?>)
  , Span(..), spanStart, spanLength
  , spanned
  )
where

import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData)
import qualified Data.Char as Char
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Primitive.MachDeps (sIZEOF_INT)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text ()
import qualified Data.Text as Text
import Data.Text.Array (Array(..))
import Data.Text.Internal (Text(Text))
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts
  ( ByteArray#, Int#, MutableByteArray#, State#, RealWorld
  , (<#), (>#), (+#), (-#), (*#)
  , indexWord16Array#
  , newByteArray#, readWord8ArrayAsInt#, writeWord8ArrayAsInt#
  , orI#, word2Int#
  , runRW#
  )
import GHC.Generics (Generic)
import GHC.Int (Int(I#))
import GHC.Word (Word16(W16#))

type Input = ByteArray#
type ByteOffset = Int#
type CharOffset = Int#
type ByteLength = Int#

-- Note: byteOffset and byteLength are in terms of Word16, not Word8
type State = (# ByteOffset, ByteLength, CharOffset #)

{-

               sizeof_int                          3*sizeof_int
                   |                                     |
0                  v                                     v
| byteOffset: Int# | byteLength: Int# | charOffset: Int# |
                                      ^
                                      |
                                 2*sizeof_int

-}
type MState s = MutableByteArray# s

newMState :: (# ByteOffset, ByteLength, CharOffset #) -> State# s -> (# State# s, MState s #)
readState :: MState s -> State# s -> (# State# s, (# ByteOffset, ByteLength, CharOffset #) #)
readCharOffset :: MState s -> State# s -> (# State# s, CharOffset #)
readByteOffset :: MState s -> State# s -> (# State# s, ByteOffset #)
writeState :: MState s -> (# ByteOffset, ByteLength, CharOffset #) -> State# s -> State# s
writeByteOffset, writeByteLength, writeCharOffset :: MState s -> Int# -> State# s -> State# s
(newMState, readState, readCharOffset, readByteOffset, writeState, writeByteOffset, writeByteLength, writeCharOffset) =
  (newMState_, readState_, readCharOffset_, readByteOffset_, writeState_, writeByteOffset_, writeByteLength_, writeCharOffset_)
  where
    sizeof_int =
      case sIZEOF_INT of
        I# i -> i

    boOffset =
      0#

    blOffset =
      sizeof_int

    coOffset =
      2# *# sizeof_int

    mstateSize = 3# *# sizeof_int

    writeState_ :: MState s -> (# ByteOffset, ByteLength, CharOffset #) -> State# s -> State# s
    writeState_ mstate (# bo, bl, co #) s =
      let
        s' = writeByteOffset_ mstate bo s
        s'' = writeByteLength_ mstate bl s'
        s''' = writeCharOffset_ mstate co s''
      in
        s'''

    newMState_ :: (# ByteOffset, ByteLength, CharOffset #) -> State# s -> (# State# s, MState s #)
    newMState_ val s =
      case newByteArray# mstateSize s of
        (# s', mstate #) ->
          let
            s'' = writeState mstate val s'
          in
            (# s'', mstate #)

    readCharOffset_ :: MState s -> State# s -> (# State# s, CharOffset #)
    readCharOffset_ mstate s =
      case readWord8ArrayAsInt# mstate coOffset s of
        (# s', co #) ->
          (# s', co #)

    readByteOffset_ :: MState s -> State# s -> (# State# s, ByteOffset #)
    readByteOffset_ mstate s =
      case readWord8ArrayAsInt# mstate boOffset s of
        (# s', bo #) ->
          (# s', bo #)

    readState_ :: MState s -> State# s -> (# State# s, (# ByteOffset, ByteLength, CharOffset #) #)
    readState_ mstate s =
      case readWord8ArrayAsInt# mstate boOffset s of
        (# s', bo #) ->
          case readWord8ArrayAsInt# mstate blOffset s' of
            (# s'', bl #) ->
              case readWord8ArrayAsInt# mstate coOffset s'' of
                (# s''', co #) ->
                  (# s''', (# bo, bl, co #) #)

    writeByteOffset_ :: MState s -> Int# -> State# s -> State# s
    writeByteOffset_ mstate = writeWord8ArrayAsInt# mstate boOffset

    writeByteLength_ :: MState s -> Int# -> State# s -> State# s
    writeByteLength_ mstate = writeWord8ArrayAsInt# mstate blOffset

    writeCharOffset_ :: MState s -> Int# -> State# s -> State# s
    writeCharOffset_ mstate = writeWord8ArrayAsInt# mstate coOffset

{-# inline byteOffset #-}
byteOffset :: State -> ByteOffset
byteOffset (# bo, _, _ #) = bo

{-# inline charOffset #-}
charOffset :: State -> CharOffset
charOffset (# _, _, co #) = co

{-# inline addByteCharOffset #-}
addByteCharOffset :: Int# -> Int# -> State -> State
addByteCharOffset m n (# bo, bl, co #) = (# m +# bo, bl, n +# co #)

{-# inline byteLength #-}
byteLength :: State -> ByteLength
byteLength (# _, bl, _ #) = bl

-- | Ported from Data.Text.Unsafe
{-# inline iter #-}
iter :: (# Input, State #) -> (# Char, State #)
iter (# input, state #) =
  case orI# ((<#) (word2Int# m) 0xD800#) ((>#) (word2Int# m) 0xDBFF#) of
    0# ->
      (# chr2 (W16# m) (W16# n)
      , addByteCharOffset 2# 1# state
      #)
    _ ->
      (# unsafeChr (W16# m)
      , addByteCharOffset 1# 1# state
      #)
  where
    m = indexWord16Array# input j
    n = indexWord16Array# input k
    j = byteOffset state
    k = j +# 1#

{-# inline iterByteArray #-}
iterByteArray :: (# ByteArray#, Int# #) -> (# Char, Int# #)
iterByteArray (# input, bo #) =
  case orI# ((<#) (word2Int# m) 0xD800#) ((>#) (word2Int# m) 0xDBFF#) of
    0# ->
      (# chr2 (W16# m) (W16# n)
      , 2# +# bo
      #)
    _ ->
      (# unsafeChr (W16# m)
      , 1# +# bo
      #)
  where
    m = indexWord16Array# input j
    n = indexWord16Array# input k
    j = bo
    k = j +# 1#

data Label
  = Named Text
  | Symbol Text
  | Char Char
  | Eof
  deriving (Eq, Ord, Show, Generic)

instance NFData Label

data ParseError = Unexpected !Int !(Set Label)
  deriving (Eq, Show, Generic)

instance NFData ParseError

type ParseError# = (# Int#, Set Label #)

type Consumed = Int#
type ExpectedSet = Set Label

newtype Parser s a
  = Parser
    { unParser ::
        (# ExpectedSet, Input, MState s, State# s #) ->
        (# State# s, Consumed, ExpectedSet, (# ParseError# | a #) #)
    }

{-# inline parse #-}
parse :: forall a. (forall s. Parser s a) -> Text -> Either ParseError a
parse (Parser p) (Text (Array arr) (I# off) (I# len)) =
  case runRW# run of
    (# _, _, _, output #) ->
      case output of
        (# (# co, es #) | #) -> Left (Unexpected (I# co) $! es)
        (# | res #) -> Right res
  where
    run :: State# RealWorld -> (# State# RealWorld, Consumed, ExpectedSet, (# ParseError# | a #) #)
    run s =
      case newMState (# off, len, 0# #) s of
        (# s', mstate #) ->
          p (# mempty, arr, mstate, s' #)

instance Functor (Parser s) where
  fmap f (Parser p) =
    Parser $ \state ->
    case p state of
      (# consumed, es, s, output #) ->
        (# consumed
        , es
        , s
        , case output of
            (# e | #) ->
              (# e | #)
            (# | result #) ->
              (# | f result #)
        #)

instance Applicative (Parser s) where
  pure a = Parser $ \(# es, _, _, s #) -> (# s, 0#, es, (# | a #) #)
  {-# inline (<*>) #-}
  Parser pf <*> Parser pa =
    Parser $ \(# es, input, state, s #) ->
    case pf (# es, input, state, s #) of
      (# s', consumed, es', output #) ->
        case output of
          (# e | #) -> (# s', consumed, es', (# e | #) #)
          (# | f #) ->
            case pa (# es', input, state, s' #) of
              (# s'', consumed', es'', output' #) ->
                case output' of
                  (# e | #) ->
                    (# s'', orI# consumed consumed', es'', (# e | #) #)
                  (# | a #) ->
                    (# s'', orI# consumed consumed', es'', (# | f a #) #)

instance Alternative (Parser s) where
  empty =
    Parser $ \(# es, _, state, s #) ->
    case readCharOffset state s of
      (# s', co #) ->
        (# s', 0#, mempty, (# (# co, es #) | #) #)

  {-# inline (<|>) #-}
  Parser pa <|> Parser pb =
    Parser $ \(# es, input, state, s #) ->
    case pa (# es, input, state, s #) of
      (# s', consumed, es', output #) ->
        case consumed of
          1# -> (# s', consumed, es', output #)
          _ ->
            case output of
              (# | _ #) -> (# s', 1#, es', output #)
              (# _ | #) ->
                case pb (# es', input, state, s' #) of
                  (# s'', consumed', es'', output' #) ->
                    (# s'', consumed', es'', output' #)

instance Monad (Parser s) where
  Parser pa >>= f =
    Parser $ \(# es, input, state, s #) ->
    case pa (# es, input, state, s #) of
      (# s', consumed, es', output #) ->
        case output of
          (# e | #) -> (# s', consumed, es', (# e | #) #)
          (# | a #) ->
            case unParser (f a) (# es', input, state, s' #) of
             (# s'', consumed', es'', output' #) ->
               (# s'', orI# consumed consumed', es'', output' #)

{-# inline char #-}
char :: Char -> Parser s Char
char c =
  Parser $
  \(# es, input, state, s #) ->
  let es' = Char c `Set.insert` es in
  case readState state s of
    (# s', state_ #) ->
      case (<#) (byteOffset state_) (byteLength state_) of
        1# ->
          case iter (# input, state_ #) of
            (# c', state_' #) ->
              if c == c' then
                case writeState state state_' s' of
                  s'' ->
                    (# s''
                    , 1#
                    , mempty
                    , (# | c' #)
                    #) else
                (# s'
                , 0#
                , es'
                , (# (# charOffset state_, es' #) | #)
                #)
        _ ->
          (# s'
          , 0#
          , es'
          , (# (# charOffset state_, es' #) | #)
          #)

-- | Equivalent to `traverse_ char . Text.unpack`
text :: Text -> Parser s ()
text (Text (Array arr) (I# off) (I# len)) =
  Parser $ \(# es, input, state, s #) ->
  case readState state s of
    (# s', state_ #) ->
      case go (# arr, off, len, input, 0#, state_ #) of
        (# consumed, state_', result #) ->
          let
            s'' = writeState state state_' s'
          in
          case result of
            (# expected | #) ->
              let
                es' = Set.insert (Char expected) es
              in
                (# s''
                , consumed
                , es'
                , (# (# charOffset state_', es' #) | #)
                #)
            (# | () #) ->
                (# s''
                , consumed
                , case consumed of
                    1# -> mempty
                    _ -> es
                , (# | () #)
                #)
  where
    go ::
      (# ByteArray# -- val's data
      , Int# -- val's offset
      , Int# -- amount still to parse
      , Input
      , Consumed
      , State
      #) ->
      (# Consumed, State, (# Char | () #) #)
    go (# vData, vOff, vRemaining, input, consumed, state #) =
      case vRemaining of
        0# -> (# consumed, state, (# | () #) #)
        _ ->
          case iterByteArray (# vData, vOff #) of
            (# expected, vOff' #) ->
              case iter (# input, state #) of
                (# actual, state' #) ->
                  if expected == actual
                  then go (# vData, vOff', vRemaining -# 1#, input, 1#, state' #)
                  else (# consumed, state, (# expected | #) #)

{-# inline satisfySome_ #-}
satisfySome_ :: (Char -> Bool) -> Set Label -> Parser s Text
satisfySome_ p expecteds =
  Parser $ \(# es, input, state, s #) ->
  case readState state s of
    (# s', state_ #) ->
      case iter (# input, state_ #) of
        (# c, state_' #) ->
          let
            es' = expecteds <> es
          in
            if p c
            then
              case go (# input, state_' #) of
                state_'' ->
                  let
                    s'' = writeState state state_'' s'
                  in
                    (# s''
                    , 1#
                    , es'
                    , (# | Text (Array input) (I# (byteOffset state_)) (I# (byteOffset state_'' -# byteOffset state_)) #)
                    #)
            else
              (# s'
              , 0#
              , es'
              , (# (# charOffset state_, es' #) | #)
              #)
  where
    go ::
      (# Input
      , State
      #) ->
      State
    go (# input, state #) =
      case iter (# input, state #) of
        (# c, state' #) ->
          if p c
          then go (# input, state' #)
          else state

{-# inline matchMany #-}
matchMany :: Parser s a -> Parser s Text
matchMany (Parser p) =
  Parser $ \(# es, input, state, s #) ->
  case readByteOffset state s of
    (# s', initialOff #) ->
      case go (# es, input, state, s' #) of
        (# s'', consumed, es', result #) ->
          case readByteOffset state s'' of
            (# s''', finalOff #) ->
              case result of
                (# err | #) ->
                  (# s''', consumed, es', (# err | #) #)
                (# | _ #) ->
                  (# s''', consumed, es', (# | Text (Array input) (I# initialOff) (I# (finalOff -# initialOff)) #) #)
  where
    go (# es, input, state, s #) =
      case p (# es, input, state, s #) of
        (# s', consumed, es', result #) ->
          case result of
            (# err | #) ->
              case consumed of
                1# -> (# s', consumed, es', (# err | #) #)
                _ -> (# s', consumed, es', (# | () #) #)
            (# | _ #) ->
              go (# es', input, state, s' #)

{-# inline matchSome #-}
matchSome :: Parser s a -> Parser s Text
matchSome (Parser p) =
  Parser $ \(# es, input, state, s #) ->
  case readByteOffset state s of
    (# s', initialOff #) ->
      case p (# es, input, state, s' #) of
        (# s'', consumed, es', result #) ->
          case result of
            (# err | #) ->
              (# s'', consumed, es', (# err | #) #)
            (# | _ #) ->
              case go (# es, input, state, s'' #) of
                (# s''', consumed', es'', result' #) ->
                  case readByteOffset state s''' of
                    (# s'''', finalOff #) ->
                      case result' of
                        (# err | #) ->
                          (# s''''
                          , orI# consumed consumed'
                          , es''
                          , (# err | #)
                          #)
                        (# | _ #) ->
                          (# s''''
                          , orI# consumed consumed'
                          , es''
                          , (# | Text (Array input) (I# initialOff) (I# (finalOff -# initialOff)) #)
                          #)
  where
    go (# es, input, state, s #) =
      case p (# es, input, state, s #) of
        (# s', consumed, es', result #) ->
          case result of
            (# err | #) ->
              case consumed of
                1# -> (# s', consumed, es', (# err | #) #)
                _ -> (# s', consumed, es', (# | () #) #)
            (# | _ #) ->
              go (# es', input, state, s' #)

-- | Only consumes input if the entire value is matched
{-# inline symbol #-}
symbol :: Text -> Parser s ()
symbol val = labelled (try (text val)) (Symbol val)

try :: Parser s a -> Parser s a
try (Parser p) =
  Parser $
  \(# es, input, state, s #) ->
    case readState state s of
      (# s', state_ #) ->
        case p (# es, input, state, s' #) of
          (# s'', consumed, es', output #) ->
            case output of
              (# (# _, expecteds #) | #) ->
                let
                  s''' = writeState state state_ s''
                in
                  (# s'''
                  , 0#
                  , es'
                  , (# (# charOffset state_, expecteds #) | #)
                  #)
              (# | _ #) ->
                (# s'', consumed, es', output #)

eof :: Parser s ()
eof =
  Parser $
  \(# es, _, state, s #) ->
  case readState state s of
    (# s', state_ #) ->
      let
        es' = Set.insert Eof es
      in
        case (<#) (byteOffset state_) (byteLength state_) of
          1# ->
            (# s', 0#, es', (# (# charOffset state_, es' #) | #) #)
          _ ->
            (# s', 0#, es', (# | () #) #)

labelled :: Parser s a -> Label -> Parser s a
labelled (Parser p) lbl =
  Parser $ \(# es, input, state, s #) ->
  case p (# es, input, state, s #) of
    (# s', consumed, _, output #) ->
      let
        es' = lbl `Set.insert` es
      in
        (# s'
        , consumed
        , es'
        , case output of
            (# (# pos, _ #) | #) ->
              (# (# pos, es' #) | #)
            _ -> output
        #)

infixl 4 <?>
(<?>) :: Parser s a -> Text -> Parser s a
(<?>) p name = labelled p (Named name)

data Span = Span {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Show)

spanStart :: Span -> Int
spanStart (Span s _) = s

spanLength :: Span -> Int
spanLength (Span _ l) = l

spanned :: Parser s a -> Parser s (Span, a)
spanned (Parser p) =
  Parser $ \(# es, input, state, s #) ->
  case readCharOffset state s of
    (# s', start #) ->
      case p (# es, input, state, s' #) of
        (# s'', consumed, es', output #) ->
          case output of
            (# e | #) ->
              (# s'', consumed, es', (# e | #) #)
            (# | a #) ->
              case readCharOffset state s'' of
                (# s''', end #) ->
                  (# s''', consumed, es', (# | (Span (I# start) (I# (end -# start)), a)  #) #)

{-# inline satisfyMaybe_ #-}
satisfyMaybe_ :: (Char -> Maybe a) -> Set Label -> Parser s a
satisfyMaybe_ p expected =
  Parser $
  \(# es, input, state, s #) ->
  let es' = expected <> es in
  case readState state s of
    (# s', state_ #) ->
      case (<#) (byteOffset state_) (byteLength state_) of
        1# ->
          case iter (# input, state_ #) of
            (# c', state_' #) ->
              case p c' of
                Nothing ->
                  (# s'
                  , 0#
                  , es'
                  , (# (# charOffset state_, es' #) | #)
                  #)
                Just res ->
                  case writeState state state_' s' of
                    s'' ->
                      (# s''
                      , 1#
                      , mempty
                      , (# | res #)
                      #)
        _ ->
          (# s'
          , 0#
          , es'
          , (# (# charOffset state_, es' #) | #)
          #)

{-# inline satisfy_ #-}
satisfy_ :: (Char -> Bool) -> Set Label -> Parser s Char
satisfy_ p = satisfyMaybe_ (\c -> if p c then Just c else Nothing)

{-# inline satisfy #-}
satisfy :: Predicate Char -> Parser s Char
satisfy (Predicate p n) = satisfy_ p n

data Predicate a = Predicate { predFunc :: a -> Bool, predLabels :: Set Label }

instance Semigroup (Predicate a) where
  p1 <> p2 =
    Predicate (\a -> predFunc p1 a || predFunc p2 a) (predLabels p1 <> predLabels p2)

instance Monoid (Predicate a) where
  mempty = Predicate (const True) mempty

{-# inline pDigit #-}
pDigit :: Predicate Char
pDigit = Predicate Char.isDigit (Set.singleton $ Named "digit")

{-# inline pLower #-}
pLower :: Predicate Char
pLower = Predicate Char.isLower (Set.singleton $ Named "lowercase character")

{-# inline pUpper #-}
pUpper :: Predicate Char
pUpper = Predicate Char.isUpper (Set.singleton $ Named "uppercase character")

{-# inline digit #-}
digit :: Parser s Char
digit = satisfy pDigit

{-# inline lower #-}
lower :: Parser s Char
lower = satisfy pLower

decimal :: Num a => Parser s a
decimal =
  Text.foldl' (\acc d -> 10 * acc + fromIntegral (Char.ord d) - 48) 0 <$>
  takeWhile1 pDigit
{-# SPECIALISE decimal :: Parser s Int #-}
{-# SPECIALISE decimal :: Parser s Int8 #-}
{-# SPECIALISE decimal :: Parser s Int16 #-}
{-# SPECIALISE decimal :: Parser s Int32 #-}
{-# SPECIALISE decimal :: Parser s Int64 #-}
{-# SPECIALISE decimal :: Parser s Integer #-}
{-# SPECIALISE decimal :: Parser s Word #-}
{-# SPECIALISE decimal :: Parser s Word8 #-}
{-# SPECIALISE decimal :: Parser s Word16 #-}
{-# SPECIALISE decimal :: Parser s Word32 #-}
{-# SPECIALISE decimal :: Parser s Word64 #-}

{-# inline takeWhile1 #-}
takeWhile1 :: Predicate Char -> Parser s Text
takeWhile1 (Predicate p n) = satisfySome_ p n

{-# inline sepBy #-}
sepBy :: Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep =
  (:) <$> p <*> many (sep *> p) <|>
  pure []

between :: Parser s l -> Parser s r -> Parser s a -> Parser s a
between l r a = l *> a <* r
