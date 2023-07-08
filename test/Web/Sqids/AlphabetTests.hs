{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.AlphabetTests (testAlphabet) where

import Control.Monad ((<=<))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids

testAlphabet :: SpecWith ()
testAlphabet = do
  describe "alphabet" $ do
    it "simple" $ do
      let numbers = [1, 2, 3]
          sqid = "4d9fd2"

          options = defaultSqidsOptions{ alphabet = "0123456789abcdef" }

      runSqids options (encode numbers) `shouldBe` Right sqid
      runSqids options (decode sqid) `shouldBe` Right numbers

    it "short alphabet" $ do
      let numbers = [1, 2, 3]
          options = defaultSqidsOptions{ alphabet = "abcde" }

      runSqids options ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "long alphabet" $ do
      let numbers = [1, 2, 3]
          options = defaultSqidsOptions{ alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+|{}[];:'\"/?.>,<`~" }

      runSqids options ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "repeating alphabet characters" $
      sqids (sqidsOptions (defaultSqidsOptions{ alphabet = "aabcdefg" }))
        `shouldBe` Left SqidsAlphabetRepeatedCharacters

    it "too short of an alphabet" $
      sqids (sqidsOptions (defaultSqidsOptions{ alphabet = "abcd" }))
        `shouldBe` Left SqidsAlphabetTooShort
