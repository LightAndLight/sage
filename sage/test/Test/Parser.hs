{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Test.Parser (parserTests) where

import Control.Applicative (empty, many, some, (<|>))
import Data.Char (isDigit)
import qualified Data.Set as Set
import Test.Hspec
import Text.Sage
import Text.Sage.Utf8 (uncons, toByteString#)
import GHC.Exts (Char(..), Int (..))
import qualified Data.Text.Encoding as Text.Encoding

digit :: Parser Char
digit = satisfy isDigit <?> "digit"

decimal :: Parser Int
decimal = read <$> some digit

parserTests :: Spec
parserTests = do
  describe "uncons" $ do
    it "Β" $ do
      case uncons (toByteString# $ Text.Encoding.encodeUtf8 "Β") of
        (# | (# c, offset #) #) -> do
          C# c `shouldBe` 'Β'
          I# offset `shouldBe` 2
        (# (# #) | #) -> expectationFailure "Done"
    it "위" $ do
      case uncons (toByteString# $ Text.Encoding.encodeUtf8 "위") of
        (# | (# c, offset #) #) -> do
          C# c `shouldBe` '위'
          I# offset `shouldBe` 3
        (# (# #) | #) -> expectationFailure "Done"
    it "𐍅" $ do
      case uncons (toByteString# $ Text.Encoding.encodeUtf8 "𐍅") of
        (# | (# c, offset #) #) -> do
          C# c `shouldBe` '𐍅'
          I# offset `shouldBe` 4
        (# (# #) | #) -> expectationFailure "Done"
    it "😎" $ do
      case uncons (toByteString# $ Text.Encoding.encodeUtf8 "😎") of
        (# | (# c, offset #) #) -> do
          C# c `shouldBe` '😎'
          I# offset `shouldBe` 4
        (# (# #) | #) -> expectationFailure "Done"

  describe "parser" $ do
    it "parse (char 'a') \"a\"" $ do
      let
        input = "a"
        output = Right 'a'
      parse (char 'a') input `shouldBe` output
    it "parse (char 'a') \"b\"" $ do
      let
        input = "b"
        output = Left (Unexpected 0 $ Set.fromList [Char 'a'])
      parse (char 'a') input `shouldBe` output
    it "parse characters '😎' and 'a'" $ do
      let
        input = Text.Encoding.encodeUtf8 "😎a"
        output = Right ('😎', 'a')
      parse ((,) <$> satisfy (const True) <*> satisfy (const True)) input `shouldBe` output
    it "parse digit \"5\"" $ do
      let
        input = "5"
        output = Right '5'
      parse digit input `shouldBe` output
    it "parse digits '5' and '6'" $ do
      let
        input = "56"
        output = Right ('5', '6')
      parse ((,) <$> digit <*> digit) input `shouldBe` output
    it "parse digit \"a\"" $ do
      let
        input = "a"
        output = Left (Unexpected 0 $ Set.fromList [String "digit"])
      parse digit input `shouldBe` output
    it "parse decimal \"11223344\"" $ do
      let
        input = "11223344"
        output = Right (11223344 :: Int)
      parse decimal input `shouldBe` output
    it "parse decimal \"a1223344\"" $ do
      let
        input = "a1223344"
        output = Left (Unexpected 0 $ Set.fromList [String "digit"]) :: Either ParseError Int
      parse decimal input `shouldBe` output
    it "parse (decimal <* eof) \"1122a344\"" $ do
      let
        input = "1122a344"
        output = Left (Unexpected 4 $ Set.fromList [Eof, String "digit"]) :: Either ParseError Int
      parse (decimal <* eof) input `shouldBe` output
    it "parse (string \"ab\") \"ab\"" $ do
      let
        input = "ab"
        output = Right "ab"
      parse (string "ab") input `shouldBe` output
    it "parse (string \"ab\") \"ac\"" $ do
      let
        input = "ac"
        output = Left (Unexpected 0 $ Set.fromList [Text "ab"])
      parse (string "ab") input `shouldBe` output
    it "parse (string \"ab\") \"ac\"" $ do
      let
        input = "ac"
        output = Left (Unexpected 0 $ Set.fromList [Text "ab"])
      parse (string "ab") input `shouldBe` output
    it "parse (sepBy (char 'a') (char 'b')) \"a\"" $ do
      let
        input = "a"
        output = Right ['a']
      parse (sepBy (char 'a') (char 'b')) input `shouldBe` output
    it "parse (sepBy (char 'a') (char 'b')) \"ababa\"" $ do
      let
        input = "ababa"
        output = Right ['a', 'a', 'a']
      parse (sepBy (char 'a') (char 'b')) input `shouldBe` output
    it "parse (1 <$ string \"toast\" <|> 2 <$ string \"toot\" <|> 3 <$ string \"tock\") \"toot\"" $ do
      let
        input = "toot"
        output = Right (2 :: Int)
      parse (1 <$ string "toast" <|> 2 <$ string "toot" <|> 3 <$ string "tock") input `shouldBe` output
    it "parse (1 <$ string \"toast\" <|> 2 <$ string \"toot\" <|> 3 <$ string \"tock\") \"tool\"" $ do
      let
        input = "tool"
        output = Left (Unexpected 0 $ Set.fromList [Text "toast", Text "toot", Text "tock"])
      parse ((1 :: Int) <$ string "toast" <|> 2 <$ string "toot" <|> 3 <$ string "tock") input `shouldBe` output
    it "parse (char 'a' *> char 'b') \"ab\"" $ do
      let
        input = "ab"
        output = Right 'b'
      parse (char 'a' *> char 'b') input `shouldBe` output
    it "parse (char 'a' *> char 'b') \"ac\"" $ do
      let
        input = "ac"
        output = Left (Unexpected 1 $ Set.fromList [Char 'b'])
      parse (char 'a' *> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> empty) \"b\"" $ do
      let
        input = "b"
        output = Left (Unexpected 0 $ Set.fromList [Char 'a'])
      parse (char 'a' <|> empty) input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"a\"" $ do
      let
        input = "a"
        output = Right 'a'
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"b\"" $ do
      let
        input = "b"
        output = Right 'b'
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"c\"" $ do
      let
        input = "c"
        output = Left (Unexpected 0 $ Set.fromList [Char 'a', Char 'b'])
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"d\"" $ do
      let
        input = "d"
        output = Left (Unexpected 0 $ Set.fromList [Char 'a', Char 'b', Char 'c'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"bz\"" $ do
      let
        input = "bz"
        output = Left (Unexpected 1 $ Set.fromList [Char 'y'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"c\"" $ do
      let
        input = "c"
        output = Left (Unexpected 1 $ Set.fromList [Char 'z'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <?> \"ax\" <|> char 'b' *> char 'y' <?> \"by\" <|> char 'c' *> char 'z' <?> \"cz\") \"d\"" $ do
      let
        input = "d"
        output = Left (Unexpected 0 $ Set.fromList [String "ax", String "by", String "cz"])
      parse
        ( (char 'a' *> char 'x' <?> "ax")
            <|> (char 'b' *> char 'y' <?> "by")
            <|> (char 'c' *> char 'z' <?> "cz")
        )
        input
        `shouldBe` output
    it "parse (some (char 'a') <* char 'b') \"aaac\"" $ do
      let
        input = "aaac"
        output = Left (Unexpected 3 $ Set.fromList [Char 'a', Char 'b'])
      parse (some (char 'a') <* char 'b') input `shouldBe` output
    it "parse (char '(' *> some (char 'x') <* char ')') \"(xx)\"" $ do
      let
        input = "(xx)"
        output = Right ['x', 'x']
      parse (char '(' *> some (char 'x') <* char ')') input `shouldBe` output
    it "parse (char '(' *> some (char 'x') <* char ')') \"(xxy\"" $ do
      let
        input = "(xxy"
        output = Left (Unexpected 3 $ Set.fromList [Char ')', Char 'x'])
      parse (char '(' *> some (char 'x') <* char ')') input `shouldBe` output
    it "parse (try (char 'a' <* empty)) \"a\"" $ do
      let
        input = "a"
        output = Left $ Unexpected 0 []
      parse (try (char 'a' <* empty)) input `shouldBe` output
    it "parse (try (char 'a' <* empty) <?> \"thing\") \"a\"" $ do
      let
        input = "a"
        output = Left $ Unexpected 0 [String "thing"]
      parse (try (char 'a' <* empty) <?> "thing") input `shouldBe` output
    it "parse (char 'a' *> (try (False <$ char 'b' <* char 'c') <|> True <$ char 'b')) \"abc\"" $ do
      let
        input = "abc"
        output = Right False
      parse
        ( char 'a'
            *> ( try (False <$ char 'b' <* char 'c')
                  <|> True <$ char 'b'
               )
        )
        input
        `shouldBe` output
    it "parse (char 'a' *> (try (False <$ char 'b' <* char 'c') <|> True <$ char 'b')) \"ab\"" $ do
      let
        input = "ab"
        output = Right True
      parse
        ( char 'a'
            *> ( try (False <$ char 'b' <* char 'c')
                  <|> True <$ char 'b'
               )
        )
        input
        `shouldBe` output
    it "parse (char 'a' *> (try (False <$ char 'b' <* char 'c') <|> True <$ char 'b')) \"ac\"" $ do
      let
        input = "ac"
        output = Left $ Unexpected 1 [Char 'b']
      parse
        ( char 'a'
            *> ( try (False <$ char 'b' <* char 'c')
                  <|> True <$ char 'b'
               )
        )
        input
        `shouldBe` output
    describe "let atom = 1 <$ char 'x' <|> char '(' *> fmap sum (many atom) <* char ')' in fmap sum (some atom) <* eof" $ do
      let
        atom = 1 <$ char 'x' <|> char '(' *> fmap sum (many atom) <* char ')'
        p = fmap sum (some atom) <* eof
      it "\"()\"" $ do
        let
          input = "()"
          output = Right (0 :: Int)
        parse p input `shouldBe` output
      it "\"()xxx\"" $ do
        let
          input = "()xxx"
          output = Right (3 :: Int)
        parse p input `shouldBe` output
      it "\"()xxx(y\"" $ do
        let
          input = "()xxx(y"
          output = Left (Unexpected 6 $ Set.fromList [Char '(', Char 'x', Char ')'])
        parse p input `shouldBe` output
      it "\"()xxx()\"" $ do
        let
          input = "()xxx()y"
          output = Left (Unexpected 7 $ Set.fromList [Char '(', Char 'x', Eof])
        parse p input `shouldBe` output
