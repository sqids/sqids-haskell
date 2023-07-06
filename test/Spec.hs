import Test.Hspec (hspec, describe)
import Web.Sqids.AlphabetTests (testAlphabet)
import Web.Sqids.BlocklistTests (testBlocklist)
import Web.Sqids.EncodingTests (testEncoding)
import Web.Sqids.InternalTests (testInternals)
import Web.Sqids.MinLengthTests (testMinLength)
import Web.Sqids.ShuffleTests (testShuffle)
import Web.Sqids.UniquesTests (testUniques)

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
      testBlocklist
      testEncoding
      testMinLength
      testShuffle
      testUniques
