{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.UniquesTests where

import Control.Monad (foldM, join, forM_)
import Data.Either (fromRight)
import Debug.Trace (traceShowM)
import Data.Function ((&))
import Data.Set (Set)
import Data.List (foldl')
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids.Internal -- ()

import qualified Data.Set as Set
import qualified Data.Text as Text

upper :: Int
upper = 1000000

testUniques :: SpecWith ()
testUniques = do
  describe "uniques" $ do
    let len = Text.length (defaultSqidsOptions & alphabet)
        options = defaultSqidsOptions { minLength = len }
        f a i = (: a) <$> encode [i] 
        ids = fromRight [] (runSqids options $ foldM f [] [0 .. upper - 1])

    it "uniques, with padding" $ do
      Set.size (foldl' (flip Set.insert) mempty ids) `shouldBe` upper

    it "...cont'd" $ do
      forM_ (zip (reverse ids) [0 ..]) $ \(sqid, i) ->
        (runSqids options $ decode sqid) `shouldBe` Right [i]

-- TODO
