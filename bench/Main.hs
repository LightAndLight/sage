{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -ddump-simpl
    -ddump-to-file
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes #-}

module Main where

import Control.Applicative (many, some, (<|>))
import Control.DeepSeq (NFData)
import Criterion.Main
import qualified Data.Attoparsec.Text as Attoparsec
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isLower)
import qualified Data.Either as Either
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.IO as Text (readFile)
import Data.Void (Void)
import GHC.Generics (Generic)
import Parsers (parsersBench)
import Streaming.Chars (Chars)
import Streaming.Chars.ByteString.Utf8 (StreamUtf8 (StreamUtf8))
import Streaming.Chars.Text (StreamText (StreamText))
import System.Environment (getArgs, withArgs)
import qualified System.IO.MMap as Mmap
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import Text.Megaparsec.Parsers (CharParsing (anyChar), unParsecT)
import Text.Parser.Char (char, satisfy, text)
import Text.Parser.Combinators (between, eof, sepBy)
import qualified Text.Sage as Parser
import Weigh

data Expr = Var Text | Lam Text Expr | App Expr Expr
  deriving (Generic, Show)

instance NFData Expr

{-# INLINE expr #-}
expr :: CharParsing m => m Expr
expr =
  lambda
    <|> app
  where
    spaces = (char ' ' *> spaces) <|> pure ()

    ident = fmap Text.pack (some $ satisfy isLower) <* spaces

    lambda =
      Lam <$ char '\\' <* spaces
        <*> ident <* char '-' <* char '>' <* spaces
        <*> expr

    atom =
      Var <$> ident <* spaces
        <|> between (char '(' *> spaces) (char ')' <* spaces) expr

    app = foldl App <$> atom <*> many atom

{-# INLINEABLE parseLambda #-}
parseLambda :: Chars s => s -> Either Parser.ParseError Expr
parseLambda = Parser.parse expr

{-# NOINLINE parseLambdaText #-}
parseLambdaText :: Text -> Either Parser.ParseError Expr
parseLambdaText = Parser.parse expr . StreamText

{-# NOINLINE parseLambdaBS #-}
parseLambdaBS :: ByteString -> Either Parser.ParseError Expr
parseLambdaBS = Parser.parse expr . StreamUtf8

{-# NOINLINE parseLambdaMP #-}
parseLambdaMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) Expr
parseLambdaMP = Megaparsec.parse (unParsecT expr) ""

{-# NOINLINE parseLambdaAP #-}
parseLambdaAP :: Text -> Either String Expr
parseLambdaAP = Attoparsec.parseOnly expr

manySymbols :: Text -> Either Parser.ParseError Int
manySymbols = Parser.parse (ps <* eof) . StreamText
  where
    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ text "hello"
        <|> 2 <$ text "goopy"
        <|> 3 <$ text "wonder"
        <|> 4 <$ text "several"
        <|> 5 <$ text "plato"
        <|> 6 <$ text "ticklish"

manyTextsNaive :: Text -> Either Parser.ParseError Int
manyTextsNaive = Parser.parse (ps <* eof) . StreamText
  where
    t :: Chars s => Text -> Parser.Parser s ()
    t = Text.foldr (\c rest -> char c *> rest) (pure ())

    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ t "hello"
        <|> 2 <$ t "goopy"
        <|> 3 <$ t "wonder"
        <|> 4 <$ t "several"
        <|> 5 <$ t "plato"
        <|> 6 <$ t "ticklish"

manyTexts :: Text -> Either Parser.ParseError Int
manyTexts = Parser.parse (ps <* eof) . StreamText
  where
    ps = (+) <$> p <*> (char ' ' *> ps <|> pure 0)
    p =
      1 <$ Parser.string "hello"
        <|> 2 <$ Parser.string "goopy"
        <|> 3 <$ Parser.string "wonder"
        <|> 4 <$ Parser.string "several"
        <|> 5 <$ Parser.string "plato"
        <|> 6 <$ Parser.string "ticklish"

manyTextsMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) Int
manyTextsMP = Megaparsec.parse (ps <* Megaparsec.eof) ""
  where
    ps = (+) <$> p <*> (Megaparsec.char ' ' *> ps <|> pure 0)

    p =
      1 <$ Megaparsec.string "hello"
        <|> 2 <$ Megaparsec.string "goopy"
        <|> 3 <$ Megaparsec.string "wonder"
        <|> 4 <$ Megaparsec.string "several"
        <|> 5 <$ Megaparsec.string "plato"
        <|> 6 <$ Megaparsec.string "ticklish"

manyTextsAP :: Text -> Attoparsec.Result Int
manyTextsAP = Attoparsec.parse (ps <* Attoparsec.endOfInput)
  where
    ps = (+) <$> p <*> (Attoparsec.char ' ' *> ps <|> pure 0)

    p =
      1 <$ Attoparsec.string "hello"
        <|> 2 <$ Attoparsec.string "goopy"
        <|> 3 <$ Attoparsec.string "wonder"
        <|> 4 <$ Attoparsec.string "several"
        <|> 5 <$ Attoparsec.string "plato"
        <|> 6 <$ Attoparsec.string "ticklish"

commasep :: Text -> Either Parser.ParseError [Char]
commasep = Parser.parse (sepBy (char 'a') (char ',') <* eof) . StreamText

commasepMP :: Text -> Either (Megaparsec.ParseErrorBundle Text Void) [Char]
commasepMP = Megaparsec.parse (Megaparsec.sepBy (Megaparsec.char 'a') (Megaparsec.char ',') <* Megaparsec.eof) ""

commasepAP :: Text -> Attoparsec.Result [Char]
commasepAP = Attoparsec.parse (Attoparsec.sepBy (Attoparsec.char 'a') (Attoparsec.char ',') <* Attoparsec.endOfInput)

lipsum :: IsString s => s
lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin consequat sodales elit eget egestas. Suspendisse eget augue accumsan velit accumsan fringilla. Nam dolor ex, pulvinar id elit quis, eleifend porta quam. Vivamus tristique fringilla enim quis cursus. Sed ex eros, volutpat quis iaculis ut, mollis quis odio. Sed id turpis quis libero varius dictum. Aliquam ut massa non diam aliquam feugiat. Vestibulum condimentum mauris vel orci aliquet iaculis. Maecenas nec est dictum, sodales lorem eu, venenatis elit. Vestibulum eu eros ac ipsum maximus bibendum eu luctus magna. Nulla vitae lorem interdum, efficitur nibh non, auctor diam. In maximus quis arcu dignissim euismod. Sed maximus et augue quis fringilla. Donec sit amet felis nec nisi finibus sagittis eget ac est. Nam at sollicitudin sapien. Cras commodo felis ac sodales eleifend. Integer vitae iaculis risus. Fusce aliquam vel leo et tristique. Sed fringilla, metus non consequat pellentesque, eros ligula vehicula ante, eget volutpat."

{-# INLINEABLE sageMany #-}
sageMany :: Chars s => s -> [Char]
sageMany = Either.fromRight undefined . Parser.parse (many anyChar)

{-# NOINLINE sageManyText #-}
sageManyText :: Text -> [Char]
sageManyText = sageMany . StreamText

{-# NOINLINE sageManyBS #-}
sageManyBS :: ByteString -> [Char]
sageManyBS = sageMany . StreamUtf8

{-# INLINEABLE sageSome #-}
sageSome :: Chars s => s -> [Char]
sageSome = Either.fromRight undefined . Parser.parse (some anyChar)

{-# NOINLINE sageSomeText #-}
sageSomeText :: Text -> [Char]
sageSomeText = sageSome . StreamText

{-# NOINLINE sageSomeBS #-}
sageSomeBS :: ByteString -> [Char]
sageSomeBS = sageSome . StreamUtf8

a1000Text :: Text
a1000Text = Text.replicate 1000 "a"

a1000BS :: ByteString
a1000BS = Text.Encoding.encodeUtf8 a1000Text

{-# INLINEABLE sageChar #-}
sageChar :: Chars s => s -> [Char]
sageChar = Either.fromRight undefined . Parser.parse (many $ char 'a')

{-# NOINLINE sageCharText #-}
sageCharText :: Text -> [Char]
sageCharText = sageChar . StreamText

{-# NOINLINE sageCharBS #-}
sageCharBS :: ByteString -> [Char]
sageCharBS = sageChar . StreamUtf8

hello1000Text :: Text
hello1000Text = Text.replicate 1000 "hello"

hello1000BS :: ByteString
hello1000BS = Text.Encoding.encodeUtf8 hello1000Text

{-# INLINEABLE sageString #-}
sageString :: Chars s => s -> [Text]
sageString = Either.fromRight undefined . Parser.parse (many $ Parser.string "hello")

{-# NOINLINE sageStringText #-}
sageStringText :: Text -> [Text]
sageStringText = sageString . StreamText

{-# NOINLINE sageStringBS #-}
sageStringBS :: ByteString -> [Text]
sageStringBS = sageString . StreamUtf8

main :: IO ()
main = do
  benchtype : args <- getArgs
  case benchtype of
    "memory" -> do
      file_5 <- Text.readFile "bench/res/depth_5.lam"
      file_5BS <- ByteString.readFile "bench/res/depth_5.lam"
      file_15 <- Text.readFile "bench/res/depth_15.lam"
      file_15BS <- ByteString.readFile "bench/res/depth_15.lam"
      mainWith $ do
        wgroup "sage" $ do
          wgroup "Text" $ do
            func' "many" sageManyText lipsum
            func' "some" sageSomeText lipsum
            func' "char" sageCharText a1000Text
            func' "string" sageStringText hello1000Text
          wgroup "UTF-8 ByteString" $ do
            func' "many" sageManyBS lipsum
            func' "some" sageSomeBS lipsum
            func' "char" sageCharBS a1000BS
            func' "string" sageStringBS hello1000BS
        func "sage x (\\y -> z)" parseLambdaText "x (\\y -> z)"
        func "megaparsec x (\\y -> z)" parseLambdaMP "x (\\y -> z)"
        func "attoparsec x (\\y -> z)" parseLambdaAP "x (\\y -> z)"
        func "sage x (\\y -> a b c d e)" parseLambdaText "x (\\y -> a b c d e)"
        func "megaparsec x (\\y -> a b c d e)" parseLambdaMP "x (\\y -> a b c d e)"
        func "attoparsec x (\\y -> a b c d e)" parseLambdaAP "x (\\y -> a b c d e)"
        func "sage x (\\y -> a b c d ~)" parseLambdaText "x (\\y -> a b c d ~)"
        func "megaparsec x (\\y -> a b c d ~)" parseLambdaMP "x (\\y -> a b c d ~)"
        func "attoparsec x (\\y -> a b c d ~)" parseLambdaAP "x (\\y -> a b c d ~)"
        wgroup "32B file" $ do
          wgroup "just parsing" $ do
            func' "megaparsec" parseLambdaMP file_5
            func' "attoparsec" parseLambdaAP file_5
            wgroup "sage" $ do
              func' "Text" parseLambdaText file_5
              func' "UTF-8 ByteString" parseLambdaBS file_5BS
          wgroup "read file and parse" $ do
            io "Text" (\path -> parseLambdaText <$> Text.readFile path) "bench/res/depth_5.lam"
            io "UTF-8 ByteString" (\path -> parseLambdaBS <$> ByteString.readFile path) "bench/res/depth_5.lam"
            io "UTF-8 ByteString mmapped" (\path -> parseLambdaBS <$> Mmap.mmapFileByteString path Nothing) "bench/res/depth_5.lam"
        wgroup "10KB file" $ do
          wgroup "just parsing" $ do
            func' "megaparsec" parseLambdaMP file_15
            func' "attoparsec" parseLambdaAP file_15
            wgroup "sage" $ do
              func' "Text" parseLambdaText file_15
              func' "UTF-8 ByteString" parseLambdaBS file_15BS
          wgroup "read file and parse" $ do
            io "Text" (\path -> parseLambdaText <$> Text.readFile path) "bench/res/depth_15.lam"
            io "UTF-8 ByteString" (\path -> parseLambdaBS <$> ByteString.readFile path) "bench/res/depth_15.lam"
            io "UTF-8 ByteString mmapped" (\path -> parseLambdaBS <$> Mmap.mmapFileByteString path Nothing) "bench/res/depth_15.lam"
    "time" ->
      withArgs args . defaultMain $
        [ bgroup
            "sage"
            [ bgroup
                "Text"
                [ bench "many" $ nf sageManyText lipsum
                , bench "some" $ nf sageSomeText lipsum
                , bench "char" $ nf sageCharText a1000Text
                , bench "string" $ nf sageStringText hello1000Text
                ]
            , bgroup
                "UTF-8 ByteString"
                [ bench "many" $ nf sageManyBS lipsum
                , bench "some" $ nf sageSomeBS lipsum
                , bench "char" $ nf sageCharBS a1000BS
                , bench "string" $ nf sageStringBS hello1000BS
                ]
            ]
        , parsersBench
        , let manyGoodInput = "hello goopy wonder several plato ticklish"
              manyBadInput = "hello goopy wonder several plato ticklish boomy"
           in bgroup
                "combinator comparisons"
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
        , bench "sage x (\\y -> z)" $ nf parseLambdaText "x (\\y -> z)"
        , bench "megaparsec x (\\y -> z)" $ nf parseLambdaMP "x (\\y -> z)"
        , bench "attoparsec x (\\y -> z)" $ nf parseLambdaAP "x (\\y -> z)"
        , bench "sage x (\\y -> a b c d e)" $ nf parseLambdaText "x (\\y -> a b c d e)"
        , bench "megaparsec x (\\y -> a b c d e)" $ nf parseLambdaMP "x (\\y -> a b c d e)"
        , bench "attoparsec x (\\y -> a b c d e)" $ nf parseLambdaAP "x (\\y -> a b c d e)"
        , bench "sage x (\\y -> a b c d ~)" $ nf parseLambdaText "x (\\y -> a b c d ~)"
        , bench "megaparsec x (\\y -> a b c d ~)" $ nf parseLambdaMP "x (\\y -> a b c d ~)"
        , bench "attoparsec x (\\y -> a b c d ~)" $ nf parseLambdaAP "x (\\y -> a b c d ~)"
        , let input = "\\x -> \\y -> x (\\z -> z y) y"
           in bgroup
                "\\x -> \\y -> x (\\z -> z y) y"
                [ bench "sage" $ nf parseLambdaText input
                , bench "megaparsec" $ nf parseLambdaMP input
                , bench "attoparsec" $ nf (\i -> case parseLambdaAP i of Right x -> x; Left e -> error e) input
                ]
        , env ((,) <$> Text.readFile "bench/res/depth_5.lam" <*> ByteString.readFile "bench/res/depth_5.lam") $ \ ~(file, fileBS) ->
            bgroup
              "32B file"
              [ bgroup
                  "sage"
                  [ bench "Text" $ nf parseLambdaText file
                  , bench "UTF-8 ByteString" $ nf parseLambdaBS fileBS
                  ]
              , bench "megaparsec" $ nf parseLambdaMP file
              , bench "attoparsec" $ nf parseLambdaAP file
              ]
        , env ((,) <$> Text.readFile "bench/res/depth_15.lam" <*> ByteString.readFile "bench/res/depth_15.lam") $ \ ~(file, fileBS) ->
            bgroup
              "10KB file"
              [ bgroup
                  "just parsing"
                  [ bgroup
                      "sage"
                      [ bench "Text" $ nf parseLambdaText file
                      , bench "UTF-8 ByteString" $ nf parseLambdaBS fileBS
                      ]
                  , bench "megaparsec" $ nf parseLambdaMP file
                  , bench "attoparsec" $ nf parseLambdaAP file
                  ]
              , bgroup
                  "read file and parse"
                  [ bgroup
                      "sage"
                      [ bench "Text" $ nfAppIO (\path -> parseLambdaText <$> Text.readFile path) "bench/res/depth_15.lam"
                      , bench "UTF-8 ByteString" $ nfAppIO (\path -> parseLambdaBS <$> ByteString.readFile path) "bench/res/depth_15.lam"
                      , bench "UTF-8 ByteString mmapped" $ nfAppIO (\path -> parseLambdaBS <$> Mmap.mmapFileByteString path Nothing) "bench/res/depth_15.lam"
                      ]
                  ]
              ]
        , let input = "a,a,a,a,a,a,a,a"
           in bgroup
                "commasep"
                [ bench "sage" $ nf commasep input
                , bench "megaparsec" $ nf commasepMP input
                , bench "attoparsec" $ nf commasepAP input
                ]
        ]
    arg -> error $ "Unknown argument " <> show arg
