module Main where

import Test.Hspec
import Test.Parser (parserTests)
import Test.Span (spanTests)

main :: IO ()
main =
  hspec $ do
    parserTests
    spanTests
