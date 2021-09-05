{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

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
