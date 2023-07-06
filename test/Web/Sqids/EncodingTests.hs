{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.EncodingTests (testEncoding) where

import Control.Monad (forM_, (<=<))
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids.Internal (decode, encode, sqids, SqidsError(..))

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
          sqid = "8QRLaD"

      sqids (encode numbers) `shouldBe` Right sqid
      sqids (decode sqid) `shouldBe` Right numbers

    it "different inputs" $ do
      let numbers = [0, 0, 0, 1, 2, 3, 100, 1000, 100000, 1000000, maxBound]

      sqids ((decode <=< encode) numbers) `shouldBe` Right numbers

    it "incremental numbers" $ do
      testEncodeDecodeAll
        [ ( "bV", [0] )
        , ( "U9", [1] )
        , ( "g8", [2] )
        , ( "Ez", [3] )
        , ( "V8", [4] )
        , ( "ul", [5] )
        , ( "O3", [6] )
        , ( "AF", [7] )
        , ( "ph", [8] )
        , ( "n8", [9] )
        ]

    it "incremental numbers, same index" $ do
      testEncodeDecodeAll
        [ ( "SrIu", [0, 0] )
        , ( "nZqE", [0, 1] )
        , ( "tJyf", [0, 2] )
        , ( "e86S", [0, 3] )
        , ( "rtC7", [0, 4] )
        , ( "sQ8R", [0, 5] )
        , ( "uz2n", [0, 6] )
        , ( "7Td9", [0, 7] )
        , ( "3nWE", [0, 8] )
        , ( "mIxM", [0, 9] )
        ]

    it "incremental numbers, same index 1" $ do
      testEncodeDecodeAll
        [ ( "SrIu", [0, 0] )
        , ( "nbqh", [1, 0] )
        , ( "t4yj", [2, 0] )
        , ( "eQ6L", [3, 0] )
        , ( "r4Cc", [4, 0] )
        , ( "sL82", [5, 0] )
        , ( "uo2f", [6, 0] )
        , ( "7Zdq", [7, 0] )
        , ( "36Wf", [8, 0] )
        , ( "m4xT", [9, 0] )
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

    it "encoding no numbers" $ do

      sqids (encode []) `shouldBe` Right ""

    it "decoding empty string" $ do

      sqids (decode "") `shouldBe` Right []

    it "decoding an ID with an invalid character" $ do

      sqids (decode "*") `shouldBe` Right []

    it "encoding no numbers" $ do

      sqids (encode [-1]) `shouldBe` Left SqidsNegativeNumberInInput
