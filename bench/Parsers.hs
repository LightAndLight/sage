{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers (parsersBench) where

import Control.Applicative (many, some, (<|>))
import Control.DeepSeq (NFData)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import qualified Data.Attoparsec.Text as Attoparsec
import Data.Char (isLower)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Streaming.Chars (Chars)
import Text.Parser.Char (CharParsing, char, satisfy, string)
import Text.Parser.Combinators (between, skipMany)
import qualified Text.Sage as Sage
import Streaming.Chars.ByteString.Utf8 (StreamUtf8(..))
import Data.Text.Encoding (encodeUtf8)

data Expr = Var String | Lam String Expr | App Expr Expr
  deriving (Generic)

instance NFData Expr

{-# INLINE expr #-}
expr :: (CharParsing m) => m Expr
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

exprSage :: (Chars s) => Sage.Parser s Expr
exprSage = expr

exprAP :: Attoparsec.Parser Expr
exprAP = expr

parsersBench :: Benchmark
parsersBench =
  bgroup
    "parsers"
    [ let input = "\\x -> \\y -> x (\\z -> z y) y"
       in bgroup
            (unpack input)
            [ bench "sage" $ nf (Sage.parse exprSage . StreamUtf8) (encodeUtf8 input)
            , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
            ]
    , let input = "\\x -> \\y -> x (\\z -> z y) y (\\x -> (\\y -> ((x y) z) (\\w -> x y w)))"
       in bgroup
            (unpack input)
            [ bench "sage" $ nf (Sage.parse exprSage . StreamUtf8) (encodeUtf8 input)
            , bench "attoparsec" $ nf (Attoparsec.parseOnly exprAP) input
            ]
    ]
