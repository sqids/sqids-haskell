module Web.Sqids.Utils.Internal
  ( swapChars
  , replaceCharAtIndex
  ) where

swapChars :: Int -> Int -> String -> String
swapChars m n input =
  replaceCharAtIndex n (input !! m) (replaceCharAtIndex m (input !! n) input)

replaceCharAtIndex :: Int -> Char -> String -> String
replaceCharAtIndex n char input = lhs ++ (char : rhs)
  where
    lhs = take n input
    rhs = drop (n + 1) input
