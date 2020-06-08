{-# language OverloadedLists, OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TypeApplications #-}
module Main where

import Test.Hspec

import Test.Parser (parserTests)

main :: IO ()
main =
  hspec $ do
    parserTests
