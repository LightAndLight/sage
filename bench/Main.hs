{-# language DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language OverloadedStrings #-}
{-# OPTIONS_GHC
    -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
#-}

module Main where

import Control.Applicative ((<|>), some, many)
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Char (isLower)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import Data.Void (Void)
import GHC.Generics (Generic)
import System.Environment (getArgs, withArgs)
import Weigh

import qualified Text.Sage as Parser
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Text.Megaparsec.Parsers (unParsecT, CharParsing (anyChar))
import Text.Parser.Char (char, satisfy, text)
import Text.Parser.Combinators (between, eof, sepBy)

import Parsers (parsersBench)
import qualified Data.Either as Either
import Streaming.Class (Stream)
import Data.Functor.Identity (Identity)
import Data.Functor.Of (Of)
import Streaming.Text.Strict (StreamText(StreamText))

data Expr = Var Text | Lam Text Expr | App Expr Expr
  deriving (Generic, Show)
instance NFData Expr

{-# inline expr #-}
expr :: CharParsing m => m Expr
expr =
  lambda <|>
  app
  where
    spaces = (char ' ' *> spaces) <|> pure ()

    ident = fmap Text.pack (some $ satisfy isLower) <* spaces

    lambda =
      Lam <$ char '\\' <* spaces <*>
      ident <* char '-' <* char '>' <* spaces <*>
      expr

    atom =
      Var <$> ident <* spaces <|>
      between (char '(' *> spaces) (char ')' <* spaces) expr

    app = foldl App <$> atom <*> many atom

{-# noinline parseLambda #-}
parseLambda :: Text -> Either Parser.ParseError Expr
parseLambda = Parser.parse expr . StreamText

{-# noinline parseLambdaMP #-}
parseLambdaMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) Expr
parseLambdaMP = Megaparsec.parse (unParsecT expr) ""

{-# noinline parseLambdaAP #-}
parseLambdaAP :: Text -> Either String Expr
parseLambdaAP = Attoparsec.parseOnly expr

manySymbols :: Text -> Either Parser.ParseError Int
manySymbols = Parser.parse (ps <* eof) . StreamText
  where
    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ text "hello" <|>
      2 <$ text "goopy" <|>
      3 <$ text "wonder" <|>
      4 <$ text "several" <|>
      5 <$ text "plato" <|>
      6 <$ text "ticklish"

manyTextsNaive :: Text -> Either Parser.ParseError Int
manyTextsNaive = Parser.parse (ps <* eof) . StreamText
  where
    t :: Stream (Of Char) Identity () s => Text -> Parser.Parser s ()
    t = Text.foldr (\c rest -> char c *> rest) (pure ())

    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ t "hello" <|>
      2 <$ t "goopy" <|>
      3 <$ t "wonder" <|>
      4 <$ t "several" <|>
      5 <$ t "plato" <|>
      6 <$ t "ticklish"

manyTexts :: Text -> Either Parser.ParseError Int
manyTexts = Parser.parse (ps <* eof) . StreamText
  where
    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ Parser.string "hello" <|>
      2 <$ Parser.string "goopy" <|>
      3 <$ Parser.string "wonder" <|>
      4 <$ Parser.string "several" <|>
      5 <$ Parser.string "plato" <|>
      6 <$ Parser.string "ticklish"

manyTextsMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) Int
manyTextsMP = Megaparsec.parse (ps <* Megaparsec.eof) ""
  where
    ps = (+) <$> p <*> (Megaparsec.char ' ' *> ps <|> pure 0)

    p =
      1 <$ Megaparsec.string "hello" <|>
      2 <$ Megaparsec.string "goopy" <|>
      3 <$ Megaparsec.string "wonder" <|>
      4 <$ Megaparsec.string "several" <|>
      5 <$ Megaparsec.string "plato" <|>
      6 <$ Megaparsec.string "ticklish"

manyTextsAP :: Text -> Attoparsec.Result Int
manyTextsAP = Attoparsec.parse (ps <* Attoparsec.endOfInput)
  where
    ps = (+) <$> p <*> (Attoparsec.char ' ' *> ps <|> pure 0)

    p =
      1 <$ Attoparsec.string "hello" <|>
      2 <$ Attoparsec.string "goopy" <|>
      3 <$ Attoparsec.string "wonder" <|>
      4 <$ Attoparsec.string "several" <|>
      5 <$ Attoparsec.string "plato" <|>
      6 <$ Attoparsec.string "ticklish"

commasep :: Text -> Either Parser.ParseError [Char]
commasep = Parser.parse (sepBy (char 'a') (char ',') <* eof) . StreamText

commasepMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) [Char]
commasepMP = Megaparsec.parse (Megaparsec.sepBy (Megaparsec.char 'a') (Megaparsec.char ',') <* Megaparsec.eof) ""

commasepAP :: Text -> Attoparsec.Result [Char]
commasepAP = Attoparsec.parse (Attoparsec.sepBy (Attoparsec.char 'a') (Attoparsec.char ',') <* Attoparsec.endOfInput)

lipsum :: Text
lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin consequat sodales elit eget egestas. Suspendisse eget augue accumsan velit accumsan fringilla. Nam dolor ex, pulvinar id elit quis, eleifend porta quam. Vivamus tristique fringilla enim quis cursus. Sed ex eros, volutpat quis iaculis ut, mollis quis odio. Sed id turpis quis libero varius dictum. Aliquam ut massa non diam aliquam feugiat. Vestibulum condimentum mauris vel orci aliquet iaculis. Maecenas nec est dictum, sodales lorem eu, venenatis elit. Vestibulum eu eros ac ipsum maximus bibendum eu luctus magna. Nulla vitae lorem interdum, efficitur nibh non, auctor diam. In maximus quis arcu dignissim euismod. Sed maximus et augue quis fringilla. Donec sit amet felis nec nisi finibus sagittis eget ac est. Nam at sollicitudin sapien. Cras commodo felis ac sodales eleifend. Integer vitae iaculis risus. Fusce aliquam vel leo et tristique. Sed fringilla, metus non consequat pellentesque, eros ligula vehicula ante, eget volutpat."

{-# NOINLINE sageMany #-}
sageMany :: Text -> [Char]
sageMany = Either.fromRight undefined . Parser.parse (many anyChar) . StreamText

{-# NOINLINE sageSome #-}
sageSome :: Text -> [Char]
sageSome = Either.fromRight undefined . Parser.parse (some anyChar) . StreamText

a1000 :: Text
a1000 = Text.replicate 1000 "a"

{-# NOINLINE sageChar #-}
sageChar :: Text -> [Char]
sageChar = Either.fromRight undefined . Parser.parse (many $ char 'a') . StreamText

hello1000 :: Text
hello1000 = Text.replicate 1000 "hello"

{-# NOINLINE sageString #-}
sageString :: Text -> [Text]
sageString = Either.fromRight undefined . Parser.parse (many $ Parser.string "hello") . StreamText

main :: IO ()
main = do
  print $ parseLambda "x"
  print $ parseLambda "x y"
  print $ parseLambda "\\x -> y"
  print $ parseLambda "x (\\y -> z)"
  print . parseLambda =<< Text.readFile "bench/res/depth_5.lam"
  benchtype:args <- getArgs
  case benchtype of
    "memory" -> do
      file_5 <- Text.readFile "bench/res/depth_5.lam"
      mainWith $ do
        wgroup "sage" $ do
          func' "many" sageMany lipsum
          func' "some" sageSome lipsum
          func' "char" sageChar a1000
          func' "string" sageString hello1000
        func "sage x (\\y -> z)" parseLambda "x (\\y -> z)"
        func "megaparsec x (\\y -> z)" parseLambdaMP "x (\\y -> z)"
        func "attoparsec x (\\y -> z)" parseLambdaAP "x (\\y -> z)"
        func "sage x (\\y -> a b c d e)" parseLambda "x (\\y -> a b c d e)"
        func "megaparsec x (\\y -> a b c d e)" parseLambdaMP "x (\\y -> a b c d e)"
        func "attoparsec x (\\y -> a b c d e)" parseLambdaAP "x (\\y -> a b c d e)"
        func "sage x (\\y -> a b c d ~)" parseLambda "x (\\y -> a b c d ~)"
        func "megaparsec x (\\y -> a b c d ~)" parseLambdaMP "x (\\y -> a b c d ~)"
        func "attoparsec x (\\y -> a b c d ~)" parseLambdaAP "x (\\y -> a b c d ~)"
        wgroup "32B file" $ do
          func' "sage" parseLambda file_5
          func' "megaparsec" parseLambdaMP file_5
          func' "attoparsec" parseLambdaAP file_5
    "time" ->
      withArgs args . defaultMain $
        [ bgroup "sage"
          [ bench "many" $ nf sageMany lipsum
          , bench "some" $ nf sageSome lipsum
          , bench "char" $ nf sageChar a1000
          , bench "string" $ nf sageString hello1000
          ]
        , parsersBench
        , let
            manyGoodInput = "hello goopy wonder several plato ticklish"
            manyBadInput = "hello goopy wonder several plato ticklish boomy"
          in
            bgroup "combinator comparisons"
              [ bench "sage symbols good" $ nf (\input -> let output@Right{} = manySymbols input in output) manyGoodInput
              , bench "sage symbols bad" $ nf (\input -> let output@Left{} = manySymbols input in output) manyBadInput
              , bench "sage texts good" $ nf (\input -> let output@Right{} = manyTexts input in output) manyGoodInput
              , bench "sage texts bad" $ nf (\input -> let output@Left{} = manyTexts input in output) manyBadInput
              , bench "sage texts naive good" $ nf (\input -> let output@Right{} = manyTextsNaive input in output) manyGoodInput
              , bench "sage texts naive bad" $ nf (\input -> let output@Left{} = manyTextsNaive input in output) manyBadInput
              , bench "megaparsec texts good" $ nf (\input -> let output@Right{} = manyTextsMP input in output) manyGoodInput
              , bench "megaparsec texts bad" $ nf (\input -> let output@Left{} = manyTextsMP input in output) manyBadInput
              , bench "attoparsec texts good" $ nf (\input -> let output@Attoparsec.Partial{} = manyTextsAP input in output) manyGoodInput
              , bench "attoparsec texts bad" $ nf (\input -> let output@Attoparsec.Fail{} = manyTextsAP input in output) manyBadInput
              ]
        , bench "sage x (\\y -> z)" $ nf parseLambda "x (\\y -> z)"
        , bench "megaparsec x (\\y -> z)" $ nf parseLambdaMP "x (\\y -> z)"
        , bench "attoparsec x (\\y -> z)" $ nf parseLambdaAP "x (\\y -> z)"
        , bench "sage x (\\y -> a b c d e)" $ nf parseLambda "x (\\y -> a b c d e)"
        , bench "megaparsec x (\\y -> a b c d e)" $ nf parseLambdaMP "x (\\y -> a b c d e)"
        , bench "attoparsec x (\\y -> a b c d e)" $ nf parseLambdaAP "x (\\y -> a b c d e)"
        , bench "sage x (\\y -> a b c d ~)" $ nf parseLambda "x (\\y -> a b c d ~)"
        , bench "megaparsec x (\\y -> a b c d ~)" $ nf parseLambdaMP "x (\\y -> a b c d ~)"
        , bench "attoparsec x (\\y -> a b c d ~)" $ nf parseLambdaAP "x (\\y -> a b c d ~)"
        , let input = "\\x -> \\y -> x (\\z -> z y) y" in
          bgroup "\\x -> \\y -> x (\\z -> z y) y"
          [ bench "sage" $ nf parseLambda input
          , bench "megaparsec" $ nf parseLambdaMP input
          , bench "attoparsec" $ nf (\i -> case parseLambdaAP i of Right x -> x; Left e -> error e) input
          ]
        , env (Text.readFile "bench/res/depth_5.lam") $ \file ->
            bgroup "32B file"
            [ bench "sage" $ nf parseLambda file
            , bench "megaparsec" $ nf parseLambdaMP file
            , bench "attoparsec" $ nf parseLambdaAP file
            ]
        , let
            input = "a,a,a,a,a,a,a,a"
          in
            bgroup "commasep"
              [ bench "sage" $ nf commasep input
              , bench "megaparsec" $ nf commasepMP input
              , bench "attoparsec" $ nf commasepAP input
              ]
        ]
    arg -> error $ "Unknown argument " <> show arg
