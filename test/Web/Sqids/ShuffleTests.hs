{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.ShuffleTests (testShuffle) where

import Test.Hspec
import Data.Function ((&))
import Web.Sqids.Internal (defaultSqidsOptions, alphabet, shuffle)

testShuffle :: SpecWith ()
testShuffle = do
  describe "shuffle" $ do
    it "default shuffle, checking for randomness" $
      shuffle (defaultSqidsOptions & alphabet) 
        `shouldBe` "fwjBhEY2uczNPDiloxmvISCrytaJO4d71T0W3qnMZbXVHg6eR8sAQ5KkpLUGF9"

    it "numbers in the front, another check for randomness" $
      shuffle "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 
        `shouldBe` "ec38UaynYXvoxSK7RV9uZ1D2HEPw6isrdzAmBNGT5OCJLk0jlFbtqWQ4hIpMgf"

    it "swapping front 2 characters" $ do
      shuffle "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        `shouldBe` "ec38UaynYXvoxSK7RV9uZ1D2HEPw6isrdzAmBNGT5OCJLk0jlFbtqWQ4hIpMgf"
      shuffle "1023456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        `shouldBe` "xI3RUayk1MSolQK7e09zYmFpVXPwHiNrdfBJ6ZAT5uCWbntgcDsEqjv4hLG28O"

    it "swapping last 2 characters" $ do
      shuffle "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        `shouldBe` "ec38UaynYXvoxSK7RV9uZ1D2HEPw6isrdzAmBNGT5OCJLk0jlFbtqWQ4hIpMgf"
      shuffle "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZY"
        `shouldBe` "x038UaykZMSolIK7RzcbYmFpgXEPHiNr1d2VfGAT5uJWQetjvDswqn94hLC6BO"

    it "short alphabet" $ shuffle "0123456789" `shouldBe` "4086517392"

    it "really short alphabet" $ shuffle "12345" `shouldBe` "24135"

    it "lowercase alphabet" $ do
      shuffle "abcdefghijklmnopqrstuvwxyz"
        `shouldBe` "lbfziqvscptmyxrekguohwjand"

    it "uppercase alphabet" $ do
      shuffle "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        `shouldBe` "ZXBNSIJQEDMCTKOHVWFYUPLRGA"

    it "bars" $ do
      shuffle "▁▂▃▄▅▆▇█" `shouldBe` "▂▇▄▅▆▃▁█"

    it "bars with numbers" $ do
      shuffle "▁▂▃▄▅▆▇█0123456789" `shouldBe` "14▅▂▇320▆75▄█96▃8▁"
