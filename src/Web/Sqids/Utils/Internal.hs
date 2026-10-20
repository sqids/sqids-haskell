module Web.Sqids.Utils.Internal
  ( letterCount
  , charSet
  , swapChars
  , replaceCharAtIndex
  , wordsNoLongerThan
  , unsafeIndex
  , unsafeUncons
  , containsMultibyteChars
  ) where

import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Set as Set

{-# INLINE letterCount #-}
letterCount :: Text -> Int
letterCount = Set.size . charSet

{-# INLINE charSet #-}
charSet :: Text -> Set Char
charSet = Text.foldr' Set.insert mempty

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

{-# INLINE wordsNoLongerThan #-}
wordsNoLongerThan :: Int -> [Text] -> [Text]
wordsNoLongerThan n = filter $ (<= n) . Text.length

{-# INLINE unsafeIndex #-}
unsafeIndex :: Char -> Text -> Int
unsafeIndex c = fromJust . Text.findIndex (== c)

{-# INLINE unsafeUncons #-}
unsafeUncons :: Text -> (Char, Text)
unsafeUncons = fromJust . Text.uncons

-- | Does this text contain any character that UTF-8 encodes to more than one
--   byte? Equivalent to comparing the character count against the encoded byte
--   count, but without encoding anything.
{-# INLINE containsMultibyteChars #-}
containsMultibyteChars :: Text -> Bool
containsMultibyteChars = Text.any (> '\DEL')
