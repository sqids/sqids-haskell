{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.AlphabetTests (testAlphabet) where

import Control.Monad ((<=<))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids (SqidsOptions(..), SqidsError(..), defaultSqidsOptions, sqidsContext, runSqids, sqids, decode, encode)
import Web.Sqids.Internal (SqidsContext(..))

createContext :: SqidsOptions -> Either SqidsError (SqidsContext Int)
createContext options = sqids (sqidsContext options)

testAlphabet :: SpecWith ()
testAlphabet = do

  describe "alphabet" $ do
    it "simple" $ do
      let numbers = [1, 2, 3]
      let sqid = "489158"
      let options = defaultSqidsOptions{ alphabet = "0123456789abcdef" }

      runSqids options (encode numbers) `shouldBe` Right sqid
      runSqids options (decode sqid) `shouldBe` Right numbers

    it "short alphabet" $ do
      let numbers = [1, 2, 3]
          options = defaultSqidsOptions{ alphabet = "abc" }

      runSqids options ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "long alphabet" $ do
      let numbers = [1, 2, 3]
          options = defaultSqidsOptions{ alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+|{}[];:'\"/?.>,<`~" }

      runSqids options ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "multibyte characters" $ do
      createContext (defaultSqidsOptions{ alphabet = "ë1092" })
        `shouldBe` Left SqidsAlphabetContainsMultibyteCharacters

    it "repeating characters" $ do
      createContext (defaultSqidsOptions{ alphabet = "aabcdefg" })
        `shouldBe` Left SqidsAlphabetRepeatedCharacters

    it "too short of an alphabet" $ do
      createContext (defaultSqidsOptions{ alphabet = "ab" })
        `shouldBe` Left SqidsAlphabetTooShort
