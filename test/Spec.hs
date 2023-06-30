{-# LANGUAGE LambdaCase #-}
import Test.Hspec

import Data.List.Split (splitOn)
import Web.Sqids.Internal (toId, toNumber, shuffle, curatedBlacklist)
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
       in it msg (swapChars (read m) (read n) input == result)
    _ ->
      error "testSwapChars: bad input"

testCuratedBlacklist :: SpecWith ()
testCuratedBlacklist =
  withTestData "curatedBlacklist" $ \case
    alphabet : wlist : result : _ ->
      let msg = alphabet <> " " <> wlist
          ws = splitOn "," wlist
          results = splitOn "," result
       in it msg (curatedBlacklist alphabet ws == results)
    _ ->
      error "testCuratedBlacklist: bad input"

testShuffle :: SpecWith ()
testShuffle = do
  withTestData "shuffle" $ \case
    input : result : _ ->
      it input (shuffle input == result)
    _ ->
      error "testShuffle: bad input"

testToId :: SpecWith ()
testToId = do
  withTestData "toId" $ \case
    num : alphabet : result : _ ->
      let msg = num <> " " <> alphabet
       in it msg (toId (read num) alphabet == result)
    _ ->
      error "testToId: bad input"

testToNumber :: SpecWith ()
testToNumber = do
  withTestData "toNumber" $ \case
    sqid : alphabet : result : _ ->
      let msg = sqid <> " " <> alphabet
       in it msg (toNumber sqid alphabet == read result)
    _ ->
      error "testToNumber: bad input"

-- TODO
testSqidsOptions :: SpecWith ()
testSqidsOptions =
  undefined

main :: IO ()
main =
  hspec $ do
    testSwapChars
    testToId
    testToNumber
    testShuffle
    testCuratedBlacklist
