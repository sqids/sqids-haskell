{-# LANGUAGE OverloadedStrings #-}
module Web.Sqids.UniquesTests where

import Data.Function ((&))
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Web.Sqids.Internal -- ()

import qualified Data.Set as Set

upper :: Int
upper = 1000000

testUniques :: SpecWith ()
testUniques = do
  pure ()
--  describe "uniques" $ do
--    it "uniques, with padding" $ do
--      --let len = Text.length (defaultSqidsOptions & alphabet)
--      --    options = defaultSqidsOptions{ minLength = len }
--      --    set = mempty
--
--      --runSqids defaultSqidsOptions{ minLength = Just len } (encode numbers) `shouldBe` Right sqid
--
--
--  -- TODO
