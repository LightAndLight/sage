{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers (parsersBench) where

import qualified Data.ByteString.Char8 as ByteString.Char8
import Control.Applicative (many, some, (<|>))
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.Char (isLower)
import GHC.Generics (Generic)
import Text.Parser.Char (CharParsing, char, satisfy, string)
import Text.Parser.Combinators (between, skipMany)
import Text.Parser.Sage ()
import qualified Text.Sage as Sage

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Generic)

instance NFData Expr

{-# INLINE expr #-}
expr :: CharParsing m => m Expr
expr =
  lam
    <|> app
  where
    ident = some (satisfy isLower)
    spaces = skipMany (char ' ')
    lam = Lam <$ char '\\' <*> ident <* spaces <* string "->" <* spaces <*> expr
    atom =
      ( between (char '(') (char ')') expr
          <|> Var <$> ident
      )
        <* spaces
    app = foldl App <$> atom <*> many atom

{-# NOINLINE exprSage #-}
exprSage :: Sage.Parser Expr
exprSage = expr

{-# NOINLINE exprAP #-}
exprAP :: Attoparsec.Parser Expr
exprAP = expr

parsersBench :: Benchmark
parsersBench =
  bgroup
    "parsers"
    [ let
        input = "\\x -> \\y -> x (\\z -> z y) y"
      in
        bgroup
          (ByteString.Char8.unpack input)
          [ bench "sage" $ nf (Sage.parse exprSage) input
          , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
          ]
    , let
        input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
      in
        bgroup
          (ByteString.Char8.unpack input)
          [ bench "sage" $ nf (Sage.parse exprSage) input
          , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
          ]
    ]
