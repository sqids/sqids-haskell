{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.EncodingTests (testEncoding) where

import Control.Monad (forM_, (<=<))
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids (SqidsError(..), sqids, decode, encode)

testEncodeDecodeAll :: [(Text, [Int])] -> IO ()
testEncodeDecodeAll ss =
  forM_ ss $ \(sqid, numbers) -> do
    sqids (encode numbers) `shouldBe` Right sqid
    sqids (decode sqid) `shouldBe` Right numbers

testEncoding :: SpecWith ()
testEncoding = do

  describe "encoding" $ do
    it "simple" $ do
      let numbers = [1, 2, 3]
          sqid = "86Rf07"

      sqids (encode numbers) `shouldBe` Right sqid
      sqids (decode sqid) `shouldBe` Right numbers

    it "different inputs" $ do
      let numbers = [0, 0, 0, 1, 2, 3, 100, 1000, 100000, 1000000, maxBound]

      sqids ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "incremental numbers" $ do
      testEncodeDecodeAll
        [ ( "bM", [0] )
        , ( "Uk", [1] )
        , ( "gb", [2] )
        , ( "Ef", [3] )
        , ( "Vq", [4] )
        , ( "uw", [5] )
        , ( "OI", [6] )
        , ( "AX", [7] )
        , ( "p6", [8] )
        , ( "nJ", [9] )
        ]

    it "incremental numbers, same index" $ do
      testEncodeDecodeAll
        [ ( "SvIz", [0, 0] )
        , ( "n3qa", [0, 1] )
        , ( "tryF", [0, 2] )
        , ( "eg6q", [0, 3] )
        , ( "rSCF", [0, 4] )
        , ( "sR8x", [0, 5] )
        , ( "uY2M", [0, 6] )
        , ( "74dI", [0, 7] )
        , ( "30WX", [0, 8] )
        , ( "moxr", [0, 9] )
        ]

    it "incremental numbers, same index 1" $ do
      testEncodeDecodeAll
        [ ( "SvIz", [0, 0] )
        , ( "nWqP", [1, 0] )
        , ( "tSyw", [2, 0] )
        , ( "eX68", [3, 0] )
        , ( "rxCY", [4, 0] )
        , ( "sV8a", [5, 0] )
        , ( "uf2K", [6, 0] )
        , ( "7Cdk", [7, 0] )
        , ( "3aWP", [8, 0] )
        , ( "m2xn", [9, 0] )
        ]

    it "multi input" $ do
      let numbers =
            [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
            , 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49
            , 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73
            , 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97
            , 98, 99
            ]

      Right numbers `shouldBe` sqids ((decode <=< encode) numbers)

    it "encoding no numbers" $

      sqids (encode []) `shouldBe` Right ""

    it "decoding empty string" $

      sqids (decode "") `shouldBe` Right []

    it "decoding an ID with an invalid character" $

      sqids (decode "*") `shouldBe` Right []

    it "encoding out-of-range numbers" $

      sqids (encode [-1]) `shouldBe` Left SqidsNegativeNumberInInput
