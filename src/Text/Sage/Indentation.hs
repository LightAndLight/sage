{-# language BangPatterns #-}
{-# language UnboxedSums, UnboxedTuples #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
module Text.Sage.Indentation
  ( Indented(..)
  , runIndented
  , nest
  , relative
  , absolute
  , level
  , dedent
  )
where

import Control.Applicative (Alternative, empty, many)
import Control.Monad (guard)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Text.Sage (Parser, Label(..), label)
import Text.Parser.Combinators ((<?>), Parsing, try)
import Text.Parser.Char (CharParsing, char)
import Text.Parser.LookAhead (lookAhead)

newtype Indented a
  = Indented { unIndented :: StateT (NonEmpty Int) Parser a }
  deriving
    ( Functor, Applicative, Alternative, Monad
    , Parsing, CharParsing
    )

runIndented :: Int -> Indented a -> Parser a
runIndented lvl (Indented m) =
  evalStateT m (NonEmpty.singleton lvl)

indentation :: Int -> Parser ()
indentation expected =
  try (do
    actual <- length <$> many (char ' ')
    guard $ actual == expected
  ) <?>
  ("indent level: " <> show expected)

currentLevel :: Indented Int
currentLevel =
  Indented $ do
    lvl :| _ <- get
    pure lvl

absolute :: Int -> Indented ()
absolute lvl =
  Indented . lift $ indentation lvl

relative :: Int -> Indented ()
relative lvl' = do
  lvl <- currentLevel
  let !lvl'' = lvl + lvl'
  Indented . modify $ NonEmpty.cons lvl''

nest :: Int -> Indented a -> Indented a
nest n p = relative n *> p <* dedent

showDedentLevels :: NonEmpty Int -> String
showDedentLevels lvls =
  "dedent to level: " <>
  (let x :| xs = NonEmpty.sort lvls in show x <> foldMap ((", " <>) . show) xs)

dedent :: Indented ()
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
            (do
              count <- lookAhead $ length <$> many (char ' ')
              pure $
                if count `elem` levels
                then Just (previousLvl :| rest)
                else Nothing
            )
        maybe empty put mRes

level :: Indented ()
level = do
  lvl <- currentLevel
  Indented . lift $ indentation lvl
