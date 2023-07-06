import Test.Hspec (hspec, describe)
import Web.Sqids.AlphabetTests (testAlphabet)
import Web.Sqids.BlocklistTests (testBlocklist)
import Web.Sqids.EncodingTests (testEncoding)
import Web.Sqids.InternalTests (testInternals)
import Web.Sqids.ShuffleTests (testShuffle)

main :: IO ()
main =
  hspec $ do
    describe "\nTest internals\n" $ do
      testInternals
    --
    -- Official tests from sqids-spec
    --
    describe "\nOfficial sqids-spec test cases\n" $ do
      testAlphabet
      testShuffle
      testBlocklist
      testEncoding
