{-# language BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Parser.Indentation (
  IndentationT (..),
  runIndentationT,
  Amount (..),
  indented,
  current,
  indent,
) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Control.Monad (guard, MonadPlus)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans.Class (lift)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Char (CharParsing(..))
import Text.Parser.LookAhead (LookAheadParsing(..))
import Text.Parser.Token (TokenParsing)

newtype IndentationT m a = IndentationT {unIndentationT :: StateT (NonEmpty Int) m a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , Parsing
    , CharParsing
    , LookAheadParsing
    , TokenParsing
    )

runIndentationT :: Monad m => Int -> IndentationT m a -> m a
runIndentationT lvl (IndentationT m) =
  evalStateT m (pure lvl)

indentation :: (CharParsing m, Monad m) => Int -> m ()
indentation expected =
  try
    ( do
        actual <- length <$> many (char ' ')
        guard $ actual == expected
    )
    <?> ("indent ==" <> show expected)

currentLevel :: Monad m => IndentationT m Int
currentLevel =
  IndentationT $ do
    lvl :| _ <- get
    pure lvl

relative :: Monad m => Int -> IndentationT m ()
relative lvl' = do
  lvl <- currentLevel
  let !lvl'' = lvl + lvl'
  IndentationT . modify $ NonEmpty.cons lvl''

data Amount = Add Int | Any

indented :: (CharParsing m, LookAheadParsing m, MonadPlus m) => Amount -> IndentationT m a -> IndentationT m a
indented amt p =
  case amt of
    Add n ->
      relative n *> p <* dedent
    Any -> do
      lvl <- currentLevel
      IndentationT
          (lift (lookAhead $ parseIndent lvl)
            >>= modify . NonEmpty.cons)
        *> p
        <* dedent

count :: (Alternative f, Num a) => f x -> f a
count p = go 0
  where
    go !acc = p *> go (acc + 1) <|> pure acc

parseIndent :: (CharParsing m, Monad m) => Int -> m Int
parseIndent lvl =
  ( do
      n <- count $ char ' '
      n <$ guard (n > lvl)
  ) <?>
    ("indent >" ++ show lvl)

indent :: (CharParsing m, Monad m) => IndentationT m Int
indent = currentLevel >>= IndentationT . lift . parseIndent

showDedentLevels :: NonEmpty Int -> String
showDedentLevels lvls =
  "dedent "
    <> ( let x :| xs = NonEmpty.sort lvls
          in "=="
              <> show x
              <> foldMap ((", ==" <>) . show) xs
       )

dedent :: (CharParsing m, LookAheadParsing m, MonadPlus m) => IndentationT m ()
dedent =
  IndentationT $ do
    _currentLvl :| levels <- get
    case levels of
      [] -> error "already at base indentation"
      previousLvl : rest -> do
        mRes <-
          lift $
            ( do
                n <- lookAhead $ count (char ' ')
                pure $
                  if n `elem` levels
                    then Just (previousLvl :| rest)
                    else Nothing
            ) <?>
              showDedentLevels (previousLvl :| rest)
        maybe empty put mRes

current :: (CharParsing m, Monad m) => IndentationT m ()
current = do
  lvl <- currentLevel
  IndentationT . lift $ indentation lvl
