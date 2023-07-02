{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Sqids.Internal
  ( SqidsOptions(..)
--  , SqidsError(..)
--  , Valid(..)
--  , emptySqidsOptions
--  , defaultSqidsOptions
--  , SqidsStack
--  , MonadSqids(..)
--  , sqidsOptions
--  , SqidsT(..)
--  , Sqids(..)
--  , runSqidsT
--  , sqidsT
--  , runSqids
--  , sqids
  , curatedBlacklist
--  , encodeNumbers
--  , decodeWithAlphabet
--  , decodeId
  , shuffle
  , toId
--  , toNumber
--  , isBlockedId
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, put)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord, toLower)
import Data.List (foldl', unfoldr, elemIndex, intersect, nub, null, intercalate)
import Data.List.Split (splitOn)
import Data.Text (Text)
import Debug.Trace (traceShow)
import Web.Sqids.Utils.Internal (letterCount, swapChars)

import qualified Data.Text as Text

data SqidsOptions = SqidsOptions
  { alphabet  :: Text
  -- ^ URL-safe characters
  , minLength :: Int
  -- ^ The minimum allowed length of IDs
  , blacklist :: [Text]
  -- ^ A list of words that must never appear in IDs
  } deriving (Show, Eq, Ord)

newtype Valid a = Valid { getValid :: a }
  deriving (Show, Read, Eq, Ord)

data SqidsError
  = SqidsAlphabetTooShort
  | SqidsAlphabetRepeatedCharacters
  | SqidsInvalidMinLength
  | SqidsNegativeNumberInInput
  deriving (Show, Read, Eq, Ord)

emptySqidsOptions :: SqidsOptions
emptySqidsOptions = SqidsOptions Text.empty 0 []

defaultSqidsOptions :: SqidsOptions
defaultSqidsOptions = SqidsOptions
  { alphabet  = Text.pack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = []
  }

type SqidsStack m = StateT (Valid SqidsOptions) (ExceptT SqidsError m)

class (Monad m) => MonadSqids m where
  encode       :: [Int] -> m Text
  decode       :: Text -> m [Int]
  getAlphabet  :: m Text
  setAlphabet  :: Text -> m ()
  getMinLength :: m Int
  setMinLength :: Int -> m ()
  getBlacklist :: m [Text]
  setBlacklist :: [Text] -> m ()

-- | SqidsOptions constructor
sqidsOptions
  :: (MonadSqids m, MonadError SqidsError m)
  => SqidsOptions
  -> m (Valid SqidsOptions)
sqidsOptions SqidsOptions{..} = do

  -- Check the length of the alphabet
  when (Text.length alphabet < 5) $
    throwError SqidsAlphabetTooShort

  -- Check that the alphabet has only unique characters
  when (letterCount alphabet < Text.length alphabet) $
    throwError SqidsAlphabetRepeatedCharacters

  -- Validate min. length
  when (minLength < 0 || minLength > Text.length alphabet) $
    throwError SqidsInvalidMinLength

  pure $ Valid $ SqidsOptions
    { alphabet  = shuffle alphabet
    , minLength = minLength
    , blacklist = curatedBlacklist alphabet blacklist
    }

newtype SqidsT m a = SqidsT { unwrapSqidsT :: SqidsStack m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Valid SqidsOptions)
    , MonadError SqidsError
    )

instance MonadTrans SqidsT where
  lift = SqidsT . lift . lift

instance (Monad m) => MonadSqids (SqidsT m) where
  -- | Encode a list of unsigned integers into an ID
  encode numbers
    | null numbers =
        -- If no numbers passed, return an empty string
        pure Text.empty
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise =
        pure (encodeNumbers numbers False)

  decode sqid = undefined
  --
  getAlphabet = undefined
  setAlphabet newAlphabet = undefined
  --
  getMinLength = undefined
  setMinLength newMinLength = undefined
  --
  getBlacklist = undefined
  setBlacklist newBlacklist = undefined

newtype Sqids a = Sqids { unwrapSqids :: SqidsT Identity a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Valid SqidsOptions)
    , MonadError SqidsError
    , MonadSqids
    )

runSqidsT :: (Monad m) => SqidsOptions -> SqidsT m a -> m (Either SqidsError a)
runSqidsT options _sqids =
  runExceptT (evalStateT (unwrapSqidsT withOptions) (Valid emptySqidsOptions))
  where
    withOptions = sqidsOptions options >>= put >> _sqids

sqidsT :: (Monad m) => SqidsT m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options . unwrapSqids

sqids :: Sqids a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

instance (MonadSqids m) => MonadSqids (StateT s m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

-- Clean up blacklist:
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
curatedBlacklist :: Text -> [Text] -> [Text]
curatedBlacklist _alphabet ws = (Text.map toLower) <$> filter isValid ws where
  isValid w = Text.length w >= 3 && Text.all (`Text.elem` _alphabet) w

-- | Internal function that encodes a list of unsigned integers into an ID
encodeNumbers :: [Int] -> Bool -> Text
encodeNumbers numbers partitioned =
  undefined

-- decodeId :: String -> String -> [Int]
-- decodeId = curry (unfoldr mu)
--   where
--     mu ("", _) = Nothing
--     mu (sqid, alphabet) =
--         case splitOn [separator] sqid of
--           [] -> Nothing
--           (c:cs) -> Just
--             ( toNumber c alphabetWithoutSeparator
--             , (intercalate [separator] cs, shuffle alphabet)
--             )
--       where
--         separator = last alphabet
--         alphabetWithoutSeparator = init alphabet
-- 
-- --decodeId "" _ ret = ret
-- --decodeId sqid alphabet ret =
-- --  case chunks of
-- ----    [] -> undefined -- ret -- decodeId "" alphabet ret
-- --    (chunk : as) ->
-- --        let ret' = ret <> [toNumber chunk alphabetWithoutSeparator]
-- --         in decodeId (intercalate [separator] as) (shuffle alphabet) ret'
-- --  where
-- --    chunks = splitOn [separator] sqid
-- --
-- --    separator = last alphabet
-- --
-- --    alphabetWithoutSeparator = init alphabet
-- --
-- --decodeId2 :: String -> String -> [Int] -> [Int]
-- --decodeId2 "" _ ret = ret
-- --decodeId2 sqid alphabet ret =
-- --  case chunks of
-- --    [] -> undefined -- ret -- decodeId "" alphabet ret
-- --    (chunk : as) ->
-- --        let ret' = ret <> [toNumber chunk alphabetWithoutSeparator]
-- --         in decodeId2 (intercalate [separator] as) (shuffle alphabet) ret'
-- --  where
-- --    chunks = splitOn [separator] sqid
-- --
-- --    separator = head alphabet
-- --
-- --    alphabetWithoutSeparator = tail alphabet
-- --
-- --bbbb x y z = (decodeId x y z, decodeId2 (reverse x) (reverse y) z)
-- 
-- decodeWithAlphabet :: String -> String -> [Int]
-- decodeWithAlphabet _alphabet sqid
--     | null sqid || not (all (`elem` _alphabet) sqid) = []
--     | otherwise = decodeId sqid'' alphabet''
--   where
--     prefix : sqid' = sqid
--     offset = unsafeElemIndex prefix _alphabet
-- 
--     _ : partition : alphabet' = drop offset _alphabet <> take offset _alphabet
-- 
--     (sqid'', alphabet'') =
--       case elemIndex partition sqid' of
--         Just n | n > 0 && n < length sqid' - 1 ->
--           (drop (n + 1) sqid', shuffle alphabet')
--         _ ->
--           (sqid', alphabet')
-- 
-- --decodeWithAlphabet :: String -> String -> [Int]
-- --decodeWithAlphabet _alphabet sqid
-- --  -- If an empty string is given, or if any character in the string is missing
-- --  -- from the alphabet, then return an empty list
-- --  | null sqid || not (all (`elem` _alphabet) sqid) = []
-- --  | otherwise =
-- --      case partitionIndex of
-- --        Nothing -> undefined
-- --        Just xx -> undefined
-- --  where
-- --    -- First character is always the `prefix`
-- --    prefix = head sqid
-- --
-- --    -- Semi-random position that was generated during encoding
-- --    offset = unsafeElemIndex prefix _alphabet
-- --
-- --    -- Re-arrange alphabet back into its original form
-- --    alphabet' = drop offset _alphabet <> take offset _alphabet
-- --
-- --    -- `partition` character is in second position
-- --    partition = alphabet' !! 1
-- --
-- --    -- new alphabet without reserved `prefix` and `partition` character
-- --    alphabet'' = drop 2 alphabet'
-- --
-- --    -- Now it is safe to remove the prefix character from the ID, as it isn't
-- --    -- needed anymore
-- --    sqid' = drop 1 sqid
-- --
-- --    partitionIndex = elemIndex partition sqid'
-- --
-- --    -- If this ID contains the `partition` character (between first position
-- --    -- and non-last position), throw away everything to the left of it,
-- --    -- include the `partition` character

shuffle :: Text -> Text
shuffle _alphabet = foldl' mu _alphabet ixs
  where
    len = Text.length _alphabet
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    --
    mu chars (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . (chars `Text.index`)
       in swapChars i r chars

toId :: Int -> Text -> Text
toId num _alphabet = Text.reverse (Text.unfoldr (fmap mu) (Just num))
  where
    len = Text.length _alphabet
    mu n =
      let (m, r) = n `divMod` len
          next = if m == 0 then Nothing else Just m
       in (Text.index _alphabet r, next)

-- toNumber :: String -> String -> Int
-- toNumber sqid _alphabet = foldl' mu 0 sqid
--   where
--     len = length _alphabet
--     mu v c =
--       case elemIndex c _alphabet of
--         Just n -> len * v + n
--         _ -> error "toNumber: bad input"
-- 
-- isBlockedId :: (MonadSqids m) => String -> m Bool
-- isBlockedId = undefined