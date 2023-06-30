{-# LANGUAGE LambdaCase #-}
import Test.Hspec

import Data.List.Split (splitOn)
import Web.Sqids.Internal (toId, toNumber, shuffle, curatedBlacklist, sqidsOptions, sqids, encode, SqidsOptions(..), SqidsError(..), Valid(..))
import Web.Sqids.Utils.Internal (swapChars)

withTestData :: FilePath -> ([String] -> SpecWith ()) -> SpecWith ()
withTestData name mu = do
  file <- runIO $ readFile ("test/data/" <> name <> ".txt")
  describe name $ mapM_ (mu . splitOn "|") (lines file)

testSwapChars :: SpecWith ()
testSwapChars = do
  withTestData "swapChars" $ \case
    m : n : input : result : _ ->
      let msg = input <> " " <> m <> " " <> n
       in it msg (swapChars (read m) (read n) input `shouldBe` result)
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
      sqids (sqidsOptions optionsValid) `shouldBe` Right (Valid optionsValid{ alphabet = shuffle (alphabet optionsValid) })
  where
    optionsWithShortAlphabet = SqidsOptions 
      { alphabet = "abc"
      , minLength = 5
      , blacklist = []
      }
    optionsWithInvalidAlphabet = SqidsOptions 
      { alphabet = "abcdefghijklmnopqrstuvwxyza"
      , minLength = 5
      , blacklist = []
      }
    optionsWithInvalidMinLength = SqidsOptions
      { alphabet = "abcdefghijklmnopqrstuvwxyz"
      , minLength = -1
      , blacklist = []
      }
    optionsValid = SqidsOptions
      { alphabet = "abcdefghijklmnopqrstuvwxyz"
      , minLength = 5 
      , blacklist = []
      }

testCuratedBlacklist :: SpecWith ()
testCuratedBlacklist =
  withTestData "curatedBlacklist" $ \case
    alphabet : wlist : result : _ ->
      let msg = alphabet <> " " <> wlist
          ws = splitOn "," wlist
          results = splitOn "," result
       in it msg (curatedBlacklist alphabet ws `shouldBe` results)
    _ ->
      error "testCuratedBlacklist: bad input"

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
    num : alphabet : result : _ ->
      let msg = num <> " " <> alphabet
       in it msg (toId (read num) alphabet `shouldBe` result)
    _ ->
      error "testToId: bad input"

testToNumber :: SpecWith ()
testToNumber = do
  withTestData "toNumber" $ \case
    sqid : alphabet : result : _ ->
      let msg = sqid <> " " <> alphabet
       in it msg (toNumber sqid alphabet `shouldBe` read result)
    _ ->
      error "testToNumber: bad input"

testEncode :: SpecWith ()
testEncode = do
  describe "encode" $ do
    it "emtpy list" $ 
      sqids (encode []) `shouldBe` Right ""
    it "list with negative values" $ 
      sqids (encode [1,2,3,-1,4]) `shouldBe` Left SqidsNegativeNumberInInput

main :: IO ()
main =
  hspec $ do
    testSwapChars
    testSqidsOptions
    testToId
    testToNumber
    testShuffle
    testCuratedBlacklist
    testEncode
