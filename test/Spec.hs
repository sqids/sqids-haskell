import Test.Hspec (hspec)
import Web.Sqids.BlocklistTests (testBlocklist)
import Web.Sqids.InternalTests (testInternals)
import Web.Sqids.ShuffleTests (testShuffle)

main :: IO ()
main =
  hspec $ do
    testInternals
    --
    -- Official tests from sqids-spec
    --
    testShuffle
--    testBlocklist
