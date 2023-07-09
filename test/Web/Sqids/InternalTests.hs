{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.InternalTests (testInternals) where

import Data.List (unfoldr)
import Data.Text (Text, unpack)
import Test.Hspec hiding (it)
import Web.Sqids.Internal (toId, toNumber, shuffle, filteredBlocklist, sqidsOptions, sqids, runSqids, encode, decodeStep, decodeWithAlphabet, isBlockedId, defaultSqidsOptions, SqidsOptions(..), SqidsError(..), SqidsContext(..))
import Web.Sqids.Utils.Internal (swapChars)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Test.Hspec as Hspec

withTestData :: FilePath -> ([Text] -> SpecWith ()) -> SpecWith ()
withTestData name mu = do
  file <- runIO $ Text.readFile ("test/data/" <> name <> ".txt")
  describe name $ mapM_ (mu . Text.splitOn "|") (Text.lines file)

it :: (HasCallStack, Example a) => Text -> a -> SpecWith (Arg a)
it = Hspec.it . unpack

textRead :: Read a => Text.Text -> a
textRead = read . unpack

testSwapChars :: SpecWith ()
testSwapChars = do
  withTestData "swapChars" $ \case
    m : n : input : result : _ ->
      let msg = input <> " " <> m <> " " <> n
       in it msg (swapChars (textRead m) (textRead n) input `shouldBe` result)
    _ ->
      error "testSwapChars: bad input"

testSqidsOptions :: SpecWith ()
testSqidsOptions =
  describe "sqidsOptions" $ do
    it "too short alphabet" $
      sqids (sqidsOptions optionsWithShortAlphabet) `shouldBe` Left SqidsAlphabetTooShort
    it "invalid alphabet" $
      sqids (sqidsOptions optionsWithInvalidAlphabet) `shouldBe` Left SqidsAlphabetRepeatedCharacters
    it "invalid min length" $
      sqids (sqidsOptions optionsWithInvalidMinLength) `shouldBe` Left SqidsInvalidMinLength
    it "valid options" $
      sqids (sqidsOptions optionsValid) `shouldBe`
        Right (SqidsContext (shuffle (alphabet optionsValid)) (minLength optionsValid) (blocklist optionsValid))
  where
    optionsWithShortAlphabet = SqidsOptions
      { alphabet = "abc"
      , minLength = 5
      , blocklist = []
      }
    optionsWithInvalidAlphabet = SqidsOptions
      { alphabet = "abcdefghijklmnopqrstuvwxyza"
      , minLength = 5
      , blocklist = []
      }
    optionsWithInvalidMinLength = SqidsOptions
      { alphabet = "abcdefghijklmnopqrstuvwxyz"
      , minLength = (-1)
      , blocklist = []
      }
    optionsValid = SqidsOptions
      { alphabet = "abcdefghijklmnopqrstuvwxyz"
      , minLength = 5
      , blocklist = []
      }

testCuratedBlocklist :: SpecWith ()
testCuratedBlocklist =
  withTestData "filteredBlocklist" $ \case
    alph : bls : result : _ ->
      let msg = alph <> " " <> bls
          ws = Text.splitOn "," bls
          results = Text.splitOn "," result
       in it msg (filteredBlocklist alph ws `shouldBe` results)
    _ ->
      error "testCuratedBlocklist: bad input"

testShuffle :: SpecWith ()
testShuffle = do
  withTestData "shuffle" $ \case
    input : result : _ ->
      it input (shuffle input `shouldBe` result)
    _ ->
      error "testShuffle: bad input"

testToId :: SpecWith ()
testToId = do
  withTestData "toId" $ \case
    num : alph : result : _ ->
      let msg = num <> " " <> alph
       in it msg (toId (textRead num) alph `shouldBe` result)
    _ ->
      error "testToId: bad input"

testToNumber :: SpecWith ()
testToNumber = do
  withTestData "toNumber" $ \case
    sqid : alph : result : _ ->
      let msg = sqid <> " " <> alph
       in it msg (toNumber sqid alph `shouldBe` textRead result)
    _ ->
      error "testToNumber: bad input"

testIsBlockedId :: SpecWith ()
testIsBlockedId = do
  withTestData "isBlockedId" $ \case
    bls : sqid : result : _ ->
      let msg = bls <> " " <> sqid
          ws = Text.splitOn "," bls
       in it msg (isBlockedId ws sqid == textRead result)
    _ ->
      error "testIsBlockedId: bad input"

testEncode :: SpecWith ()
testEncode = do
  describe "encode" $ do
    it "emtpy list" $
      sqids (encode []) `shouldBe` Right ""
    it "list with negative values" $
      sqids (encode [1,2,3,-1,4]) `shouldBe` Left SqidsNegativeNumberInInput

  withTestData "encode" $ \case
    alph : numbers : result : _ ->
      let msg = alph <> " " <> numbers 
          nums = textRead <$> (Text.splitOn "," numbers)
       in it msg (runSqids defaultSqidsOptions{ alphabet = alph } (encode nums) `shouldBe` Right result)
    _ ->
      error "testEncode: bad input"

testEncodeWithMinLength :: SpecWith ()
testEncodeWithMinLength = do
  withTestData "encodeWithMinLength" $ \case
    numbers : minlen : result : _ ->
      let msg = numbers <> " " <> minlen
          nums = textRead <$> (Text.splitOn "," numbers)
       in it msg $ do
         runSqids (defaultSqidsOptions{ minLength = textRead minlen }) (encode nums) `shouldBe` Right result
    _ ->
      error "testEncodeWithMinLength: bad input"

testDecodeId :: SpecWith ()
testDecodeId = do
  withTestData "decodeId" $ \case
    sqid : alph : result : _ ->
      let msg = sqid <> " " <> alph
       in it msg (unfoldr decodeStep (sqid, alph) `shouldBe` textRead result)
    _ ->
      error "testDecodeId: bad input"

testDecodeWithAlphabet :: SpecWith ()
testDecodeWithAlphabet = do
  withTestData "decodeWithAlphabet" $ \case
    alph : sqid : result : _ ->
      let msg = alph <> " " <> sqid
       in it msg (decodeWithAlphabet alph sqid `shouldBe` textRead result)
    _ ->
      error "testDecodeWithAlphabet: bad input"

testInternals :: SpecWith ()
testInternals = do
  testSwapChars
  testSqidsOptions
  testToId
  testToNumber
  testShuffle
  testCuratedBlocklist
  testIsBlockedId
  testEncode
  testEncodeWithMinLength
  testDecodeId
  testDecodeWithAlphabet
