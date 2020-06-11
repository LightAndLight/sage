{-# language OverloadedStrings #-}
module Test.Parser (parserTests) where

import Control.Applicative ((<|>), empty, some, many)
import Test.Hspec
import qualified Data.Set as Set

import Text.Sage

parserTests :: Spec
parserTests =
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
    it "parse (matchMany (char 'a') <* char 'b') \"b\"" $ do
      let
        input = "b"
        output = Right ""
      parse (matchMany (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchMany (char 'a') <* char 'b') \"ab\"" $ do
      let
        input = "ab"
        output = Right "a"
      parse (matchMany (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchMany (char 'a') <* char 'b') \"aab\"" $ do
      let
        input = "aab"
        output = Right "aa"
      parse (matchMany (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchMany (char 'a' *> char 'b') <* char 'c') \"ababc\"" $ do
      let
        input = "ababc"
        output = Right "abab"
      parse (matchMany (char 'a' *> char 'b') <* char 'c') input `shouldBe` output
    it "parse (matchMany (char 'a' *> char 'b') <* char 'c') \"abaxc\"" $ do
      let
        input = "abaxc"
        output = Left (Unexpected 3 $ Set.fromList [Char 'b'])
      parse (matchMany (char 'a' *> char 'b') <* char 'c') input `shouldBe` output
    it "parse (matchSome (char 'a') <* char 'b') \"b\"" $ do
      let
        input = "b"
        output = Left (Unexpected 0 $ Set.fromList [Char 'a'])
      parse (matchSome (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchSome (char 'a') <* char 'b') \"ab\"" $ do
      let
        input = "ab"
        output = Right "a"
      parse (matchSome (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchSome (char 'a') <* char 'b') \"aab\"" $ do
      let
        input = "aab"
        output = Right "aa"
      parse (matchSome (char 'a') <* char 'b') input `shouldBe` output
    it "parse (matchSome (char 'a' *> char 'b') <* char 'c') \"ababc\"" $ do
      let
        input = "ababc"
        output = Right "abab"
      parse (matchSome (char 'a' *> char 'b') <* char 'c') input `shouldBe` output
    it "parse (matchSome (char 'a' *> char 'b') <* char 'c') \"abaxc\"" $ do
      let
        input = "abaxc"
        output = Left (Unexpected 3 $ Set.fromList [Char 'b'])
      parse (matchSome (char 'a' *> char 'b') <* char 'c') input `shouldBe` output
    it "parse digit \"5\"" $ do
      let
        input = "5"
        output = Right '5'
      parse digit input `shouldBe` output
    it "parse digit \"a\"" $ do
      let
        input = "a"
        output = Left (Unexpected 0 $ Set.fromList [Named "digit"])
      parse digit input `shouldBe` output
    it "parse decimal \"11223344\"" $ do
      let
        input = "11223344"
        output = Right (11223344 :: Int)
      parse decimal input `shouldBe` output
    it "parse decimal \"a1223344\"" $ do
      let
        input = "a1223344"
        output = Left (Unexpected 0 $ Set.fromList $ fmap Char ['0'..'9']) :: Either ParseError Int
      parse decimal input `shouldBe` output
    it "parse (decimal <* eof) \"1122a344\"" $ do
      let
        input = "1122a344"
        output = Left (Unexpected 4 $ Set.fromList $ Eof : fmap Char ['0'..'9']) :: Either ParseError Int
      parse (decimal <* eof) input `shouldBe` output
    it "parse (symbol \"ab\") \"ab\"" $ do
      let
        input = "ab"
        output = Right ()
      parse (symbol "ab") input `shouldBe` output
    it "parse (text \"ab\") \"ac\"" $ do
      let
        input = "ac"
        output = Left (Unexpected 1 $ Set.fromList [Char 'b'])
      parse (text "ab") input `shouldBe` output
    it "parse (symbol \"ab\") \"ac\"" $ do
      let
        input = "ac"
        output = Left (Unexpected 0 $ Set.fromList [Symbol "ab"])
      parse (symbol "ab") input `shouldBe` output
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
    it "parse (1 <$ symbol \"toast\" <|> 2 <$ symbol \"toot\" <|> 3 <$ symbol \"tock\") \"toot\"" $ do
      let
        input = "toot"
        output = Right (2 :: Int)
      parse (1 <$ symbol "toast" <|> 2 <$ symbol "toot" <|> 3 <$ symbol "tock") input `shouldBe` output
    it "parse (1 <$ symbol \"toast\" <|> 2 <$ symbol \"toot\" <|> 3 <$ symbol \"tock\") \"tool\"" $ do
      let
        input = "tool"
        output = Left (Unexpected 0 $ Set.fromList [Symbol "toast", Symbol "toot", Symbol "tock"])
      parse ((1::Int) <$ symbol "toast" <|> 2 <$ symbol "toot" <|> 3 <$ symbol "tock") input `shouldBe` output
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
        output = Left (Unexpected 0 $ Set.fromList [Named "ax", Named "by", Named "cz"])
      parse
        (char 'a' *> char 'x' <?> "ax" <|> char 'b' *> char 'y' <?> "by" <|> char 'c' *> char 'z' <?> "cz")
        input
        `shouldBe`
        output
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
    describe "let atom = 1 <$ char 'x' <|> char '(' *> fmap sum (many atom) <* char ')' in fmap sum (some atom) <* eof" $ do
      let
        atom = 1 <$ char 'x' <|> char '(' *> fmap sum (many atom) <* char ')'
        p = fmap sum (some atom) <* eof
      it "\"()\"" $ do
        let
          input = "()"
          output = Right (0::Int)
        parse p input `shouldBe` output
      it "\"()xxx\"" $ do
        let
          input = "()xxx"
          output = Right (3::Int)
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
