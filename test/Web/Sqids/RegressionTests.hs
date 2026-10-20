{-# LANGUAGE OverloadedStrings #-}
-- | Cases that are easy to get wrong when the internals are rewritten.
--
--   Every expectation here was produced by sqids 0.2.2.0 and is checked against
--   a differential corpus generated from it, so these pin observable behaviour
--   rather than anybody's idea of what it ought to be.
module Web.Sqids.RegressionTests (testRegressions) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec (HasCallStack, SpecWith, describe, expectationFailure, it, shouldBe)
import Web.Sqids
  ( SqidsContext
  , SqidsError (..)
  , SqidsOptions (..)
  , decode
  , defaultSqidsOptions
  , encode
  , mkSqidsContext
  , runSqids
  , runSqidsWith
  , sqids
  )

import qualified Data.Text as Text

-- | Run an expectation against an encoding that is expected to succeed.
withEncoded
  :: (HasCallStack)
  => Either SqidsError Text
  -> (Text -> IO ())
  -> IO ()
withEncoded (Left err) _ = expectationFailure ("expected an ID, got " <> show err)
withEncoded (Right sqid) k = k sqid

testRegressions :: SpecWith ()
testRegressions =
  describe "regressions" $ do

    it "an ID consisting of only a prefix byte decodes to no numbers" $
      -- Every single character of the alphabet is a valid prefix, but carries
      -- no chunk after it. Decoding one must yield [], not [0].
      forM_ (Text.unpack (alphabet defaultSqidsOptions)) $ \c ->
        sqids (decode (Text.singleton c)) `shouldBe` (Right [] :: Either e [Int])

    it "an ID that is only a separator decodes to no numbers" $ do
      -- "bM" encodes [0]; the prefix alone is an empty first chunk.
      sqids (encode [0 :: Int]) `shouldBe` Right ("bM" :: Text)
      sqids (decode "b") `shouldBe` (Right [] :: Either e [Int])

    it "the empty ID and out-of-alphabet bytes decode to no numbers" $ do
      sqids (decode "") `shouldBe` (Right [] :: Either e [Int])
      sqids (decode "*") `shouldBe` (Right [] :: Either e [Int])
      sqids (decode "86Rf07*") `shouldBe` (Right [] :: Either e [Int])
      -- multi-byte characters can never be in an (ASCII-only) alphabet
      sqids (decode "86Rf07\228") `shouldBe` (Right [] :: Either e [Int])
      sqids (decode "\8364") `shouldBe` (Right [] :: Either e [Int])

    it "every encoded ID decodes back to its input" $
      forM_ ([[0], [1], [1, 2, 3], [0, 0]] :: [[Int]]) $ \ns ->
        withEncoded (sqids (encode ns)) $ \sqid ->
          sqids (decode sqid) `shouldBe` Right ns

    it "zero round-trips at every minLength" $
      forM_ [0 .. 255] $ \ml -> do
        let opts = defaultSqidsOptions { minLength = ml }
        withEncoded (runSqids opts (encode [0 :: Int])) $ \sqid -> do
          Text.length sqid `shouldBe` max 2 ml
          runSqids opts (decode sqid) `shouldBe` Right [0 :: Int]

    it "the shortest legal alphabet still round-trips" $
      forM_ ([[0], [1], [61], [1, 2], [0, 0, 0]] :: [[Int]]) $ \ns -> do
        let opts = defaultSqidsOptions { alphabet = "abc", blocklist = [] }
        runSqids opts (encode ns >>= decode) `shouldBe` Right ns

    it "a sliced Text decodes the same as an unsliced one" $ do
      -- Text values carry an offset into a shared array; reading the bytes
      -- directly has to respect it.
      withEncoded (sqids (encode [1, 2, 3 :: Int])) $ \sqid -> do
        let padded = "xx" <> sqid <> "yy"
            sliced = Text.take (Text.length sqid) (Text.drop 2 padded)
        sliced `shouldBe` sqid
        sqids (decode sliced) `shouldBe` Right [1, 2, 3 :: Int]

    it "a blocklist word that is also a valid ID still decodes" $ do
      let opts = defaultSqidsOptions { blocklist = ["86Rf07"] }
      runSqids opts (decode "86Rf07") `shouldBe` Right [1, 2, 3 :: Int]
      runSqids opts (encode [1, 2, 3 :: Int]) `shouldBe` Right ("se8ojk" :: Text)

    it "words shorter than four characters only block equal-length IDs" $ do
      -- The spec compares such words to the whole ID for equality, so they can
      -- never match a longer ID.
      let opts = defaultSqidsOptions { blocklist = ["pnd"] }
      runSqids opts (encode [1000 :: Int] >>= decode) `shouldBe` Right [1000 :: Int]

    it "a reused context gives the same answers as rebuilding one" $
      forM_ optionSets $ \opts -> do
        let ctx = either (error . show) id (mkSqidsContext opts) :: SqidsContext Int
        forM_ inputs $ \ns -> do
          runSqidsWith ctx (encode ns) `shouldBe` runSqids opts (encode ns)
          runSqidsWith ctx (encode ns >>= decode) `shouldBe` Right ns

    it "a context stays correct across many uses" $ do
      -- Nothing in the context may be mutated by encoding or decoding: the
      -- working alphabet is a buffer allocated per call, not shared state.
      let ctx = either (error . show) id (mkSqidsContext defaultSqidsOptions) :: SqidsContext Int
          once = runSqidsWith ctx (encode [1, 2, 3])
      forM_ [1 .. 200 :: Int] $ \i -> do
        runSqidsWith ctx (encode [i]) `shouldBe` runSqids defaultSqidsOptions (encode [i])
        runSqidsWith ctx (encode [1, 2, 3]) `shouldBe` once

    it "mkSqidsContext rejects what sqidsContext rejects" $
      forM_ badOptions $ \(opts, err) ->
        (mkSqidsContext opts :: Either SqidsError (SqidsContext Int)) `shouldBe` Left err
  where
    optionSets =
      [ defaultSqidsOptions
      , defaultSqidsOptions { minLength = 24 }
      , defaultSqidsOptions { minLength = 255 }
      , defaultSqidsOptions { blocklist = [] }
      , defaultSqidsOptions { alphabet = "0123456789abcdef" }
      , defaultSqidsOptions { alphabet = "abc", blocklist = [] }
      ]
    inputs =
      [[0], [1], [1, 2, 3], [0, 0, 0], [100000], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]] :: [[Int]]
    badOptions =
      [ (defaultSqidsOptions { alphabet = "ab" },  SqidsAlphabetTooShort)
      , (defaultSqidsOptions { alphabet = "aab" }, SqidsAlphabetRepeatedCharacters)
      , (defaultSqidsOptions { alphabet = "\235\&1092" }, SqidsAlphabetContainsMultibyteCharacters)
      , (defaultSqidsOptions { minLength = -1 },   SqidsInvalidMinLength)
      , (defaultSqidsOptions { minLength = 256 },  SqidsInvalidMinLength)
      ]
