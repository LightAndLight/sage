module Test.Span (spanTests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, modifyMaxSuccess)

import Text.Sage (Span(..), spanContains)

genSpan :: Gen Span
genSpan = do
  p <- Gen.int $ Range.constant 0 maxBound
  l <- Gen.int $ Range.constant 0 (maxBound - p)
  pure $ Span p l

spanTests :: Spec
spanTests =
  modifyMaxSuccess (const 10000) . describe "span" $ do
    it "associativity" . hedgehog $ do
      a <- forAll genSpan
      b <- forAll genSpan
      c <- forAll genSpan
      a <> b <> c === (a <> b) <> c
    it "commutativity" . hedgehog $ do
      a <- forAll genSpan
      b <- forAll genSpan
      a <> b === b <> a
    it "idempotence" . hedgehog $ do
      a <- forAll genSpan
      a <> a === a
    it "lower bound" . hedgehog $ do
      a <- forAll genSpan
      b <- forAll genSpan
      let c = a <> b
      assert $ spanContains c a
      assert $ spanContains c b
