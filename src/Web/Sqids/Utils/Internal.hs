module Web.Sqids.Utils.Internal
  ( swapChars
  , replaceCharAtIndex
  , elemIndex
  , unsafeElemIndex
  ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)

swapChars :: Int -> Int -> String -> String
swapChars m n input =
  replaceCharAtIndex n (input !! m) (replaceCharAtIndex m (input !! n) input)

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex n char input = lhs ++ (char : rhs)
  where
    lhs = take n input
    rhs = drop (n + 1) input

unsafeElemIndex :: (Eq a) => a -> [a] -> Int
unsafeElemIndex e = fromJust . elemIndex e
