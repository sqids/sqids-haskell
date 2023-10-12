{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.BlocklistTests (testBlocklist) where

import Control.Monad ((>=>))
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids (SqidsOptions(..), SqidsError(..), Sqids, defaultSqidsOptions, sqidsOptions, runSqids, sqids, decode, encode)
import Web.Sqids.Internal (sqidsAlphabet, sqidsBlocklist)
import qualified Data.Text as Text

withEmptyBlocklist :: Sqids a -> Either SqidsError a
withEmptyBlocklist = runSqids defaultSqidsOptions{ blocklist = [] }

withNonEmptyBlocklist :: Sqids a -> Either SqidsError a
withNonEmptyBlocklist = runSqids defaultSqidsOptions{ blocklist = ["ArUO"] }

withCustomBlocklist :: [Text] -> Sqids a -> Either SqidsError a
withCustomBlocklist bls = runSqids defaultSqidsOptions { blocklist = bls }

testBlocklist :: SpecWith ()
testBlocklist = do

  describe "blocklist" $ do
    it "if no custom blocklist param, use the default blocklist" $ do
      sqids (decode "aho1e") `shouldBe` Right [ 4572721 ]
      sqids (encode [ 4572721 ]) `shouldBe` Right "JExTR"

    it "if an empty blocklist param passed, don't use any blocklist" $ do
      withEmptyBlocklist (decode "aho1e") `shouldBe` Right [ 4572721 ]
      withEmptyBlocklist (encode [ 4572721 ]) `shouldBe` Right "aho1e"

    it "if a non-empty blocklist param passed, use only that" $ do
      -- Make sure we don't use the default blocklist
      withNonEmptyBlocklist (decode "aho1e") `shouldBe` Right [ 4572721 ]
      withNonEmptyBlocklist (encode [ 4572721 ]) `shouldBe` Right "aho1e"

      -- Make sure we are using the passed blocklist
      withNonEmptyBlocklist (decode "ArUO") `shouldBe` Right [ 100000 ]
      withNonEmptyBlocklist (encode [ 100000 ]) `shouldBe` Right "QyG4"
      withNonEmptyBlocklist (decode "QyG4") `shouldBe` Right [ 100000 ]

    it "blocklist" $ do
      let bls =
            [ "JSwXFaosAN"   -- Normal result of 1st encoding. Let's block that word on purpose
            , "OCjV9JK64o"   -- Result of 2nd encoding
            , "rBHf"         -- Result of 3rd encoding is `4rBHfOiqd3`. Let's block a substring
            , "79SM"         -- Result of 4th encoding is `dyhgw479SM`. Let's block the postfix
            , "7tE6"         -- Result of 4th encoding is `7tE6jdAHLe`. Let's block the prefix
            ]

      withCustomBlocklist bls (encode [1000000, 2000000]) `shouldBe` Right "1aYeB7bRUt"
      withCustomBlocklist bls (decode "1aYeB7bRUt") `shouldBe` Right [1000000, 2000000]

    it "decoding blocklist words should still work" $ do
      let bls =
            [ "86Rf07"
            , "se8ojk"
            , "ARsz1p"
            , "Q8AI49"
            , "5sQRZO"
            ]

      withCustomBlocklist bls (decode "86Rf07") `shouldBe` Right [ 1, 2, 3 ]
      withCustomBlocklist bls (decode "se8ojk") `shouldBe` Right [ 1, 2, 3 ]
      withCustomBlocklist bls (decode "ARsz1p") `shouldBe` Right [ 1, 2, 3 ]
      withCustomBlocklist bls (decode "Q8AI49") `shouldBe` Right [ 1, 2, 3 ]
      withCustomBlocklist bls (decode "5sQRZO") `shouldBe` Right [ 1, 2, 3 ]

    it "match against a short blocklist word" $
      withCustomBlocklist [ "pnd" ] ((encode >=> decode) [1000]) `shouldBe` Right [1000]

    it "blocklist filtering in constructor" $ do
      let options = defaultSqidsOptions { alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", blocklist = ["sxnzkl"] }
          testFn = do
            p <- encode [1, 2, 3]
            q <- decode p
            pure (p, q)
      runSqids options testFn `shouldBe` Right ("IBSHOZ", [1, 2, 3])

    it "max encoding attempts" $ do
      let _alphabet  = "abc"
          _blocklist = [ "cab", "abc", "bca" ]
          _minLength = 3
          options = defaultSqidsOptions
            { alphabet  = _alphabet
            , blocklist = _blocklist
            , minLength = _minLength
            }

      case runSqids defaultSqidsOptions (sqidsOptions options) of
        Left _ ->
            error "Unexpected failure"
        Right config -> do
            Text.length (sqidsAlphabet config) `shouldBe` _minLength
            length (sqidsBlocklist config) `shouldBe` _minLength

            runSqids options ((encode >=> decode) [0]) `shouldBe` Left SqidsMaxEncodingAttempts
