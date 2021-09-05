{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser (parserTests) where

import Control.Applicative (empty, (<|>))
import Data.Char (isDigit)
import Data.Functor.Identity (Identity)
import qualified Data.Set as Set
import Streaming.Class (Of, Stream)
import Streaming.Text.Strict (StreamText)
import Test.Hspec
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Sage

decimal :: Stream (Of Char) Identity () s => Parser s Int
decimal = read <$> some (satisfy isDigit <?> "digit")

parserTests :: Spec
parserTests =
  describe "parser" $ do
    it "parse (char 'a') \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Right 'a'
      parse (char 'a') input `shouldBe` output
    it "parse (char 'a') \"b\"" $ do
      let input :: StreamText
          input = "b"
          output = Left (Unexpected 0 $ Set.fromList [Char 'a'])
      parse (char 'a') input `shouldBe` output
    it "parse digit \"5\"" $ do
      let input :: StreamText
          input = "5"
          output = Right '5'
      parse digit input `shouldBe` output
    it "parse digit \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Left (Unexpected 0 $ Set.fromList [String "digit"])
      parse digit input `shouldBe` output
    it "parse decimal \"11223344\"" $ do
      let input :: StreamText
          input = "11223344"
          output = Right (11223344 :: Int)
      parse decimal input `shouldBe` output
    it "parse decimal \"a1223344\"" $ do
      let input :: StreamText
          input = "a1223344"
          output = Left (Unexpected 0 $ Set.fromList [String "digit"]) :: Either ParseError Int
      parse decimal input `shouldBe` output
    it "parse (decimal <* eof) \"1122a344\"" $ do
      let input :: StreamText
          input = "1122a344"
          output = Left (Unexpected 4 $ Set.fromList [Eof, String "digit"]) :: Either ParseError Int
      parse (decimal <* eof) input `shouldBe` output
    it "parse (text \"ab\") \"ab\"" $ do
      let input :: StreamText
          input = "ab"
          output = Right "ab"
      parse (text "ab") input `shouldBe` output
    it "parse (text \"ab\") \"ac\"" $ do
      let input :: StreamText
          input = "ac"
          output = Left (Unexpected 0 $ Set.fromList [Text "ab"])
      parse (text "ab") input `shouldBe` output
    it "parse (text \"ab\") \"ac\"" $ do
      let input :: StreamText
          input = "ac"
          output = Left (Unexpected 0 $ Set.fromList [Text "ab"])
      parse (text "ab") input `shouldBe` output
    it "parse (sepBy (char 'a') (char 'b')) \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Right ['a']
      parse (sepBy (char 'a') (char 'b')) input `shouldBe` output
    it "parse (sepBy (char 'a') (char 'b')) \"ababa\"" $ do
      let input :: StreamText
          input = "ababa"
          output = Right ['a', 'a', 'a']
      parse (sepBy (char 'a') (char 'b')) input `shouldBe` output
    it "parse (1 <$ text \"toast\" <|> 2 <$ text \"toot\" <|> 3 <$ text \"tock\") \"toot\"" $ do
      let input :: StreamText
          input = "toot"
          output = Right (2 :: Int)
      parse (1 <$ text "toast" <|> 2 <$ text "toot" <|> 3 <$ text "tock") input `shouldBe` output
    it "parse (1 <$ text \"toast\" <|> 2 <$ text \"toot\" <|> 3 <$ text \"tock\") \"tool\"" $ do
      let input :: StreamText
          input = "tool"
          output = Left (Unexpected 0 $ Set.fromList [Text "toast", Text "toot", Text "tock"])
      parse ((1 :: Int) <$ text "toast" <|> 2 <$ text "toot" <|> 3 <$ text "tock") input `shouldBe` output
    it "parse (char 'a' *> char 'b') \"ab\"" $ do
      let input :: StreamText
          input = "ab"
          output = Right 'b'
      parse (char 'a' *> char 'b') input `shouldBe` output
    it "parse (char 'a' *> char 'b') \"ac\"" $ do
      let input :: StreamText
          input = "ac"
          output = Left (Unexpected 1 $ Set.fromList [Char 'b'])
      parse (char 'a' *> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> empty) \"b\"" $ do
      let input :: StreamText
          input = "b"
          output = Left (Unexpected 0 $ Set.fromList [Char 'a'])
      parse (char 'a' <|> empty) input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Right 'a'
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"b\"" $ do
      let input :: StreamText
          input = "b"
          output = Right 'b'
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' <|> char 'b') \"c\"" $ do
      let input :: StreamText
          input = "c"
          output = Left (Unexpected 0 $ Set.fromList [Char 'a', Char 'b'])
      parse (char 'a' <|> char 'b') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"d\"" $ do
      let input :: StreamText
          input = "d"
          output = Left (Unexpected 0 $ Set.fromList [Char 'a', Char 'b', Char 'c'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"bz\"" $ do
      let input :: StreamText
          input = "bz"
          output = Left (Unexpected 1 $ Set.fromList [Char 'y'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') \"c\"" $ do
      let input :: StreamText
          input = "c"
          output = Left (Unexpected 1 $ Set.fromList [Char 'z'])
      parse (char 'a' *> char 'x' <|> char 'b' *> char 'y' <|> char 'c' *> char 'z') input `shouldBe` output
    it "parse (char 'a' *> char 'x' <?> \"ax\" <|> char 'b' *> char 'y' <?> \"by\" <|> char 'c' *> char 'z' <?> \"cz\") \"d\"" $ do
      let input :: StreamText
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
      let input :: StreamText
          input = "aaac"
          output = Left (Unexpected 3 $ Set.fromList [Char 'a', Char 'b'])
      parse (some (char 'a') <* char 'b') input `shouldBe` output
    it "parse (char '(' *> some (char 'x') <* char ')') \"(xx)\"" $ do
      let input :: StreamText
          input = "(xx)"
          output = Right ['x', 'x']
      parse (char '(' *> some (char 'x') <* char ')') input `shouldBe` output
    it "parse (char '(' *> some (char 'x') <* char ')') \"(xxy\"" $ do
      let input :: StreamText
          input = "(xxy"
          output = Left (Unexpected 3 $ Set.fromList [Char ')', Char 'x'])
      parse (char '(' *> some (char 'x') <* char ')') input `shouldBe` output
    it "parse (try (char 'a' <* empty)) \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Left $ Unexpected 0 []
      parse (try (char 'a' <* empty)) input `shouldBe` output
    it "parse (try (char 'a' <* empty) <?> \"thing\") \"a\"" $ do
      let input :: StreamText
          input = "a"
          output = Left $ Unexpected 0 [String "thing"]
      parse (try (char 'a' <* empty) <?> "thing") input `shouldBe` output
    it "parse (char 'a' *> (try (False <$ char 'b' <* char 'c') <|> True <$ char 'b')) \"abc\"" $ do
      let input :: StreamText
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
      let input :: StreamText
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
      let input :: StreamText
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
      let atom = 1 <$ char 'x' <|> char '(' *> fmap sum (many atom) <* char ')'
          p = fmap sum (some atom) <* eof
      it "\"()\"" $ do
        let input :: StreamText
            input = "()"
            output = Right (0 :: Int)
        parse p input `shouldBe` output
      it "\"()xxx\"" $ do
        let input :: StreamText
            input = "()xxx"
            output = Right (3 :: Int)
        parse p input `shouldBe` output
      it "\"()xxx(y\"" $ do
        let input :: StreamText
            input = "()xxx(y"
            output = Left (Unexpected 6 $ Set.fromList [Char '(', Char 'x', Char ')'])
        parse p input `shouldBe` output
      it "\"()xxx()\"" $ do
        let input :: StreamText
            input = "()xxx()y"
            output = Left (Unexpected 7 $ Set.fromList [Char '(', Char 'x', Eof])
        parse p input `shouldBe` output
