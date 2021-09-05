{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Sage.Indentation (
  Indented (..),
  runIndented,
  Amount (..),
  indented,
  current,
  indent,
) where

import Control.Applicative (Alternative, empty, many)
import Control.Monad (guard)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity)
import Data.Functor.Of (Of)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Streaming.Class (Stream)
import Text.Parser.Char (CharParsing, char)
import Text.Parser.Combinators (Parsing, try, (<?>))
import Text.Parser.LookAhead (lookAhead)
import Text.Sage (Label (..), Parser, count, label)

newtype Indented s a = Indented {unIndented :: StateT (NonEmpty Int) (Parser s) a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    )

deriving instance Stream (Of Char) Identity () s => Parsing (Indented s)

deriving instance Stream (Of Char) Identity () s => CharParsing (Indented s)

runIndented :: Int -> Indented s a -> Parser s a
runIndented lvl (Indented m) =
  evalStateT m (pure lvl)

indentation :: Stream (Of Char) Identity () s => Int -> Parser s ()
indentation expected =
  try
    ( do
        actual <- length <$> many (char ' ')
        guard $ actual == expected
    )
    <?> ("indent ==" <> show expected)

currentLevel :: Indented s Int
currentLevel =
  Indented $ do
    lvl :| _ <- get
    pure lvl

relative :: Int -> Indented s ()
relative lvl' = do
  lvl <- currentLevel
  let !lvl'' = lvl + lvl'
  Indented . modify $ NonEmpty.cons lvl''

data Amount = Add Int | Any

indented :: Stream (Of Char) Identity () s => Amount -> Indented s a -> Indented s a
indented amt p =
  case amt of
    Add n ->
      relative n *> p <* dedent
    Any -> do
      lvl <- currentLevel
      ( Indented $
          lift (lookAhead $ parseIndent lvl)
            >>= modify . NonEmpty.cons
        )
        *> p
        <* dedent

parseIndent :: Stream (Of Char) Identity () s => Int -> Parser s Int
parseIndent lvl =
  label
    (String $ "indent >" <> show lvl)
    ( do
        n <- count $ char ' '
        n <$ guard (n > lvl)
    )

indent :: Stream (Of Char) Identity () s => Indented s Int
indent = currentLevel >>= Indented . lift . parseIndent

showDedentLevels :: NonEmpty Int -> String
showDedentLevels lvls =
  "dedent "
    <> ( let x :| xs = NonEmpty.sort lvls
          in "==" <> show x
              <> foldMap ((", ==" <>) . show) xs
       )

dedent :: Stream (Of Char) Identity () s => Indented s ()
dedent =
  Indented $ do
    _currentLvl :| levels <- get
    case levels of
      [] -> error "already at base indentation"
      previousLvl : rest -> do
        mRes <-
          lift $
            label
              (String $ showDedentLevels (previousLvl :| rest))
              ( do
                  n <- lookAhead $ count (char ' ')
                  pure $
                    if n `elem` levels
                      then Just (previousLvl :| rest)
                      else Nothing
              )
        maybe empty put mRes

current :: Stream (Of Char) Identity () s => Indented s ()
current = do
  lvl <- currentLevel
  Indented . lift $ indentation lvl
