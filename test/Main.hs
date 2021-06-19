{-# language OverloadedLists, OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
module Main where

import Test.Hspec

import Test.Indentation (indentationTests)
import Test.Parser (parserTests)
import Test.Span (spanTests)

main :: IO ()
main =
  hspec $ do
    indentationTests
    parserTests
    spanTests
