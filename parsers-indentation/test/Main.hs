module Main where

import Test.Hspec
import Test.Indentation (indentationTests)

main :: IO ()
main =
  hspec $ do
    indentationTests
