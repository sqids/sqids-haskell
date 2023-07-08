{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.MinLengthTests (testMinLength) where

import Control.Monad (forM_)
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldSatisfy)
import Web.Sqids.Internal (decode, encode, sqids, runSqids, defaultSqidsOptions, sqidsOptions, SqidsOptions(..), SqidsError(..))

import qualified Data.Text as Text

-- TODO: DRY
testEncodeDecodeAll :: [(Text, [Int])] -> IO ()
testEncodeDecodeAll ss = do
  let len = Text.length (defaultSqidsOptions & alphabet)
  forM_ ss $ \(sqid, numbers) -> do
    runSqids defaultSqidsOptions{ minLength = len } (encode numbers) `shouldBe` Right sqid
    runSqids defaultSqidsOptions{ minLength = len } (decode sqid) `shouldBe` Right numbers

testMinLength :: SpecWith ()
testMinLength = do
  describe "minLength" $ do
    it "simple" $ do
      let numbers = [1, 2, 3]
          sqid = "75JILToVsGerOADWmHlY38xvbaNZKQ9wdFS0B6kcMEtnRpgizhjU42qT1cd0dL"
          len = Text.length (defaultSqidsOptions & alphabet)

      runSqids defaultSqidsOptions{ minLength = len } (encode numbers) `shouldBe` Right sqid
      runSqids defaultSqidsOptions{ minLength = len } (decode sqid) `shouldBe` Right numbers

    it "incremental numbers" $
      testEncodeDecodeAll
        [ ( "jf26PLNeO5WbJDUV7FmMtlGXps3CoqkHnZ8cYd19yIiTAQuvKSExzhrRghBlwf", [0, 0] )
        , ( "vQLUq7zWXC6k9cNOtgJ2ZK8rbxuipBFAS10yTdYeRa3ojHwGnmMV4PDhESI2jL", [0, 1] )
        , ( "YhcpVK3COXbifmnZoLuxWgBQwtjsSaDGAdr0ReTHM16yI9vU8JNzlFq5Eu2oPp", [0, 2] )
        , ( "OTkn9daFgDZX6LbmfxI83RSKetJu0APihlsrYoz5pvQw7GyWHEUcN2jBqd4kJ9", [0, 3] )
        , ( "h2cV5eLNYj1x4ToZpfM90UlgHBOKikQFvnW36AC8zrmuJ7XdRytIGPawqYEbBe", [0, 4] )
        , ( "7Mf0HeUNkpsZOTvmcj836P9EWKaACBubInFJtwXR2DSzgYGhQV5i4lLxoT1qdU", [0, 5] )
        , ( "APVSD1ZIY4WGBK75xktMfTev8qsCJw6oyH2j3OnLcXRlhziUmpbuNEar05QCsI", [0, 6] )
        , ( "P0LUhnlT76rsWSofOeyRGQZv1cC5qu3dtaJYNEXwk8Vpx92bKiHIz4MgmiDOF7", [0, 7] )
        , ( "xAhypZMXYIGCL4uW0te6lsFHaPc3SiD1TBgw5O7bvodzjqUn89JQRfk2Nvm4JI", [0, 8] )
        , ( "94dRPIZ6irlXWvTbKywFuAhBoECQOVMjDJp53s2xeqaSzHY8nc17tmkLGwfGNl", [0, 9] )
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

      forM_ ((,) <$> inputMinLengths <*> inputNumbers) $ \(minLength, numbers) -> do
        let result = runSqids defaultSqidsOptions{ minLength = minLength } (encode numbers)
        case result of
          Left _ -> error "error: min lengths"
          Right sqid -> do
            sqid `shouldSatisfy` ((>= minLength) . Text.length)
            sqids (decode sqid) `shouldBe` Right numbers

    it "out-of-range invalid min length" $ do
      let len = Text.length (defaultSqidsOptions & alphabet)

      sqids (sqidsOptions defaultSqidsOptions{ minLength = (-1) }) `shouldBe` Left SqidsInvalidMinLength
      sqids (sqidsOptions defaultSqidsOptions{ minLength = len + 1 }) `shouldBe` Left SqidsInvalidMinLength
