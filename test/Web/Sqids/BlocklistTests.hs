{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.BlocklistTests where

import Test.Hspec
import Web.Sqids.Internal

testBlocklist :: SpecWith ()
testBlocklist = do
  describe "blocklist" $ do
    it "if no custom blocklist param, use the default blocklist" $ do
      (sqids $ decode "sexy") `shouldBe` Right [ 200044 ]
      (sqids $ encode [ 200044 ]) `shouldBe` Right "d171vI"
