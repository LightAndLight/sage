{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
module Test.Indentation (indentationTests) where

import Control.Applicative ((<|>), many, optional, some)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Parser.Char (char, letter, space, string)
import Text.Sage (Label(..), ParseError(..), parse)
import Text.Sage.Indentation (Indented, runIndented, level, nest)

data Def
  = Def Text [Def]
  | Print Text
  deriving (Eq, Show)

program :: Indented Def
program =
  def <|>
  print'
  where
    def =
      Def . Text.pack <$ string "def" <* space <*>
      some letter <* string "():" <* char '\n' <*>
      nest 2 (some (level *> program <* optional (char '\n')))
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
           nest 2 (level *> char 'b')
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
           nest 4 (level *> char 'b')
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
           nest 2 (level *> char 'b')
          )
          input
          `shouldBe`
          Left (Unexpected 2 [String "indent level: 2"])
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
           nest 2 (level *> char 'b' <* char '\n') <*>
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
           nest 2 (level *> char 'b' <* char '\n') <*>
           char 'c'
          )
          input
          `shouldBe`
          Left (Unexpected 6 [String "dedent to level: 0"])
    describe "python-style, enforced 2-space indents" $ do
      it "1" $ do
        let
          input =
            Text.unlines
            [ "def hi():"
            , "  print(\"yes\")"
            ]
        parse (runIndented 0 program) input `shouldBe`
          Right (Def "hi" [Print "yes"])
      it "2" $ do
        let
          input =
            Text.unlines
            [ "def f():"
            , "  def g():"
            , "    print(\"yes\")"
            ]
        parse (runIndented 0 program) input `shouldBe`
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
        parse (runIndented 0 program) input `shouldBe`
          Right (Def "f" [Def "g" [Print "yes"], Print "no"])
      it "4" $ do
        let
          input =
            Text.unlines
            [ "def f():"
            , "  def g():"
            , "    print(\"yes\")"
            , "   print(\"no\")"
            ]
        parse (runIndented 0 program) input `shouldBe`
          Left (Unexpected 37 [String "dedent to level: 0, 2", String "indent level: 4"])
