{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.UniquesTests (testUniques) where

import Control.Monad (foldM, forM_)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.List (foldl')
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids

import qualified Data.Set as Set
import qualified Data.Text as Text

--upper :: Int
--upper = 1000000
--
--uniqueWithConfig :: SqidsOptions -> Int -> Int -> SpecWith ()
--uniqueWithConfig options offset n = do
--  let range = [offset .. offset + upper - 1]
--      ids = fromRight [] (runSqids options $ foldM f [] range)
--  it "count" $
--    Set.size (foldl' (flip Set.insert) mempty ids) `shouldBe` upper
--  it "decode" $
--    forM_ (zip (reverse ids) [offset ..]) $ \(sqid, i) ->
--      (runSqids options $ decode sqid) `shouldBe` Right (replicate n i)
--  where
--    f a i = (: a) <$> encode (replicate n i)

testUniques :: SpecWith ()
testUniques = do
  pure ()

--  describe "uniques" $ do
--    describe "with padding" $
--      uniqueWithConfig defaultSqidsOptions { minLength = Text.length (defaultSqidsOptions & alphabet) } 0 1
--
--    describe "low ranges" $
--      uniqueWithConfig defaultSqidsOptions 0 1
--
--    describe "high ranges" $
--      uniqueWithConfig defaultSqidsOptions 100000000 1
--
--    describe "multi" $
--      uniqueWithConfig defaultSqidsOptions 0 5
