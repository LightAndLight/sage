{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
module Test.Indentation (indentationTests) where

import Control.Applicative ((<|>), many, optional, some)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parser.Char (char, letter, space, string)
import Text.Sage (Label(..), ParseError(..), parse)
import Text.Sage.Indentation (Amount(..), Indented, runIndented, current, indented)

data Def
  = Def Text [Def]
  | Print Text
  deriving (Eq, Show)

pythonish :: Indented Def
pythonish =
  def <|>
  print'
  where
    def =
      Def . Text.pack <$ string "def" <* space <*>
      some letter <* string "():" <* char '\n' <*>
      indented Any (some (current *> pythonish <* optional (char '\n')))
    print' =
      Print . Text.pack <$ string "print(\"" <*>
      many letter <* string "\")"

indentationTests :: Spec
indentationTests = do
  describe "Text.Sage.Indentation" $ do
    describe "indent" $ do
      it "2 spaces" $ do
        let
          input =
            Text.unlines
            [ "a"
            , "  b"
            ]
        parse
          (runIndented 0 $
           (,) <$>
           char 'a' <* char '\n' <*>
           indented (Add 2) (current *> char 'b')
          ) input
          `shouldBe`
          Right ('a', 'b')
      it "4 spaces" $ do
        let
          input =
            Text.unlines
            [ "a"
            , "    b"
            ]
        parse
          (runIndented 0 $
           (,) <$>
           char 'a' <* char '\n' <*>
           indented (Add 4) (current *> char 'b')
          )
          input
          `shouldBe`
          Right ('a', 'b')
      it "expected 2 spaces but got 0" $ do
        let
          input =
            Text.unlines
            [ "a"
            , "b"
            ]
        parse
          (runIndented 0 $
           (,) <$>
           char 'a' <* char '\n' <*>
           indented (Add 2) (current *> char 'b')
          )
          input
          `shouldBe`
          Left (Unexpected 2 [String "indent ==2"])
    describe "dedent" $ do
      it "2 spaces then back to 0" $ do
        let
          input =
            Text.unlines
            [ "a"
            , "  b"
            , "c"
            ]
        parse
          (runIndented 0 $
           (,,) <$>
           char 'a' <* char '\n' <*>
           indented (Add 2) (current *> char 'b' <* char '\n') <*>
           char 'c'
          )
          input
          `shouldBe`
          Right ('a', 'b', 'c')
      it "2 spaces then back to 0 but stayed at 2" $ do
        let
          input =
            Text.unlines
            [ "a"
            , "  b"
            , "  c"
            ]
        parse
          (runIndented 0 $
           (,,) <$>
           char 'a' <* char '\n' <*>
           indented (Add 2) (current *> char 'b' <* char '\n') <*>
           char 'c'
          )
          input
          `shouldBe`
          Left (Unexpected 6 [String "dedent ==0"])
    describe "python-style, enforced 2-space indents" $ do
      it "1" $ do
        let
          input =
            Text.unlines
            [ "def hi():"
            , "  print(\"yes\")"
            ]
        parse (runIndented 0 pythonish) input `shouldBe`
          Right (Def "hi" [Print "yes"])
      it "2" $ do
        let
          input =
            Text.unlines
            [ "def f():"
            , "  def g():"
            , "    print(\"yes\")"
            ]
        parse (runIndented 0 pythonish) input `shouldBe`
          Right (Def "f" [Def "g" [Print "yes"]])
      it "3" $ do
        let
          input =
            Text.unlines
            [ "def f():"
            , "  def g():"
            , "    print(\"yes\")"
            , "  print(\"no\")"
            ]
        parse (runIndented 0 pythonish) input `shouldBe`
          Right (Def "f" [Def "g" [Print "yes"], Print "no"])
      it "4" $ do
        let
          input =
            Text.unlines
            [ "def f():"
            , "  def g():"
            , "      print(\"yes\")"
            , "    print(\"no\")"
            ]
        parse (runIndented 0 pythonish) input `shouldBe`
          Left (Unexpected 39 [String "dedent ==0, ==2", String "indent ==6"])
