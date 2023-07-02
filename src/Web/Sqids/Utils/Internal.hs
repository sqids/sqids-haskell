module Web.Sqids.Utils.Internal
  ( letterCount
  , letters
  , swapChars
  , replaceCharAtIndex
  , wordsNoLongerThan
--  , elemIndex
--  , unsafeElemIndex
  ) where

--import Data.List (elemIndex)
--import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Set as Set

letterCount :: Text -> Int
letterCount = Set.size . letters

letters :: Text -> Set Char
letters = Text.foldr' Set.insert mempty

swapChars :: Int -> Int -> Text -> Text
swapChars m n input =
  replaceCharAtIndex n (charAt m) (replaceCharAtIndex m (charAt n) input)
  where
    charAt = Text.index input

replaceCharAtIndex :: Int -> Char -> Text -> Text
replaceCharAtIndex n char input = lhs <> Text.cons char rhs
  where
    lhs = Text.take n input
    rhs = Text.drop (n + 1) input

wordsNoLongerThan :: Int -> [Text] -> [Text]
wordsNoLongerThan n = filter $ (<= n) . Text.length

-- unsafeElemIndex :: (Eq a) => a -> [a] -> Int
-- unsafeElemIndex e = fromJust . elemIndex e
