{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.MinLengthTests (testMinLength) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)
import Web.Sqids (SqidsOptions(..), SqidsError(..), defaultSqidsOptions, sqidsContext, runSqids, sqids, decode, encode)
import Web.Sqids.Internal (SqidsContext)

import qualified Data.Text as Text

createContext :: SqidsOptions -> Either SqidsError (SqidsContext Int)
createContext options = sqids (sqidsContext options)

testEncodeDecodeAll :: [(Int, Text)] -> IO ()
testEncodeDecodeAll ss =
  forM_ ss $ \(len, sqid) -> do
    runSqids defaultSqidsOptions{ minLength = len } (encode [1, 2, 3]) `shouldBe` Right sqid
    runSqids defaultSqidsOptions{ minLength = len } (decode sqid) `shouldBe` Right [1, 2, 3]

testMinLength :: SpecWith ()
testMinLength = do

  describe "minLength" $ do
    it "simple" $ do
      let numbers = [1, 2, 3]
          sqid = "86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTM"
          len = Text.length (defaultSqidsOptions & alphabet)

      runSqids defaultSqidsOptions{ minLength = len } (encode numbers) `shouldBe` Right sqid
      runSqids defaultSqidsOptions{ minLength = len } (decode sqid) `shouldBe` Right numbers

    it "incremental numbers" $ do
      let len = Text.length (defaultSqidsOptions & alphabet)
      testEncodeDecodeAll
        [ ( 6, "86Rf07" )
        , ( 7, "86Rf07x" )
        , ( 8, "86Rf07xd" )
        , ( 9, "86Rf07xd4" )
        , ( 10, "86Rf07xd4z" )
        , ( 11, "86Rf07xd4zB" )
        , ( 12, "86Rf07xd4zBm" )
        , ( 13, "86Rf07xd4zBmi" )
        , ( len + 0, "86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTM" )
        , ( len + 1, "86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMy" )
        , ( len + 2, "86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMyf" )
        , ( len + 3, "86Rf07xd4zBmiJXQG6otHEbew02c3PWsUOLZxADhCpKj7aVFv9I8RquYrNlSTMyf1" )
        ]

    it "min lengths" $ do
      let len = Text.length (defaultSqidsOptions & alphabet)
          inputMinLengths = [0, 1, 5, 10, len]
          inputNumbers =
              [ [0]
              , [0, 0, 0, 0, 0]
              , [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
              , [100, 200, 300]
              , [1000, 2000, 3000]
              , [1000000]
              , [maxBound :: Int]
              ]

      forM_ ((,) <$> inputMinLengths <*> inputNumbers) $ \(mlen, numbers) -> do
        let result = runSqids defaultSqidsOptions{ minLength = mlen } (encode numbers)
        case result of
          Left _ -> error "error: min lengths"
          Right sqid -> do
            sqid `shouldSatisfy` ((>= mlen) . Text.length)
            sqids (decode sqid) `shouldBe` Right numbers

    it "out-of-range invalid min length" $ do
      createContext (defaultSqidsOptions{ minLength = -1 }) `shouldBe` Left SqidsInvalidMinLength
      createContext (defaultSqidsOptions{ minLength = 256 }) `shouldBe` Left SqidsInvalidMinLength
