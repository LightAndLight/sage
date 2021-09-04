{-# language DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language OverloadedStrings #-}
module Parsers (parsersBench) where

import Control.Applicative ((<|>), many, some)
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data.Char (isLower)
import GHC.Generics (Generic)
import Data.Text (Text, unpack)
import Data.Void (Void)

import qualified Text.Sage as Sage
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec.Parsers (ParsecT(unParsecT))
import Text.Parser.Combinators (between, skipMany)
import Text.Parser.Char (CharParsing, char, satisfy, string)
import Streaming.Class (Stream)
import Data.Functor.Identity (Identity)
import Data.Functor.Of (Of)
import Streaming.Text.Strict (StreamText(StreamText))

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving Generic

instance NFData Expr

{-# inline expr #-}
expr :: CharParsing m => m Expr
expr =
  lam <|>
  app
  where
    ident = some (satisfy isLower)
    spaces = skipMany (char ' ')
    lam = Lam <$ char '\\' <*> ident <* spaces <* string "->" <* spaces <*> expr
    atom =
      (between (char '(') (char ')') expr <|>
       Var <$> ident
      ) <*
      spaces
    app = foldl App <$> atom <*> many atom

exprSage :: Stream (Of Char) Identity () s => Sage.Parser s Expr
exprSage = expr

exprMP :: Megaparsec.Parsec Void Text Expr
exprMP = unParsecT expr

exprAP :: Attoparsec.Parser Expr
exprAP = expr

parsersBench :: Benchmark
parsersBench =
  bgroup "parsers"
  [ let
      input = "\\x -> \\y -> x (\\z -> z y) y"
    in
      bgroup (unpack input)
        [ bench "sage" $ nf (Sage.parse exprSage . StreamText) input
        , bench "megaparsec" $ nf (Megaparsec.parse exprMP "") input
        , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
        ]
  , let
      input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
    in
      bgroup (unpack input)
        [ bench "sage" $ nf (Sage.parse exprSage . StreamText) input
        , bench "megaparsec" $ nf (Megaparsec.parse exprMP "") input
        , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
        ]
  ]
