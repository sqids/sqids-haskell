{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Sqids.Internal
  ( SqidsOptions(..)
  , SqidsError(..)
  , Valid(..)
  , emptySqidsOptions
  , defaultSqidsOptions
  , SqidsStack
  , MonadSqids(..)
  , sqidsOptions
  , SqidsT(..)
  , Sqids(..)
  , runSqidsT
  , sqidsT
  , runSqids
  , sqids
  , curatedBlacklist
  , encodeNumbers
  , decodeWithAlphabet
  , shuffle
  , toId
  , toNumber
  , isBlockedId
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
import Data.List (foldl', unfoldr, elemIndex, intersect, nub, null)
import Web.Sqids.Utils.Internal (swapChars, unsafeElemIndex)

data SqidsOptions = SqidsOptions
  { alphabet  :: String
  -- ^ URL-safe characters
  , minLength :: Int
  -- ^ The minimum allowed length of IDs
  , blacklist :: [String]
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
emptySqidsOptions = SqidsOptions "" 0 []

defaultSqidsOptions :: SqidsOptions
defaultSqidsOptions = SqidsOptions
  { alphabet  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blacklist = []
  }

type SqidsStack m = StateT (Valid SqidsOptions) (ExceptT SqidsError m)

class (Monad m) => MonadSqids m where
  encode :: [Int] -> m String
  decode :: String -> m [Int]
  getAlphabet :: m String
  setAlphabet :: String -> m ()
  getMinLength :: m Int
  setMinLength :: Int -> m ()
  getBlacklist :: m [String]
  setBlacklist :: [String] -> m ()

-- | SqidsOptions constructor
sqidsOptions
  :: (MonadSqids m, MonadError SqidsError m)
  => SqidsOptions
  -> m (Valid SqidsOptions)
sqidsOptions SqidsOptions{..} = do

  -- Check the length of the alphabet
  when (length alphabet < 5) $
    throwError SqidsAlphabetTooShort

  -- Check that the alphabet has only unique characters
  when (nub alphabet /= alphabet) $
    throwError SqidsAlphabetRepeatedCharacters

  -- Validate min. length
  when (minLength < 0 || minLength > length alphabet) $
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
        pure ""
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise =
        pure (encodeNumbers numbers False)

  -- | TODO
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
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode = lift . encode
  decode = lift . decode
  getAlphabet = lift getAlphabet
  setAlphabet = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlacklist = lift getBlacklist
  setBlacklist = lift . setBlacklist

-- Clean up blacklist:
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
curatedBlacklist :: String -> [String] -> [String]
curatedBlacklist _alphabet ws = (fmap toLower) <$> filter isValid ws
  where
    isValid w = length w >= 3 && length w == length (w `intersect` _alphabet)

-- | Internal function that encodes a list of unsigned integers into an ID
encodeNumbers :: [Int] -> Bool -> String
encodeNumbers numbers partitioned =
  undefined

decodeWithAlphabet2 :: String -> String -> [Int]
decodeWithAlphabet2 _alphabet sqid
    | null sqid || not (all (`elem` _alphabet) sqid) = []
    | otherwise = undefined
  where
    offset = unsafeElemIndex (head sqid) _alphabet

    _ : partition : alphabet'' = drop offset _alphabet <> take offset _alphabet

    sqid' = drop 1 sqid

    (sqid'', alphabet''') =
      case elemIndex partition sqid' of
        Just n | n > 0 && n < length sqid' - 1 ->
          (drop (n + 1) sqid', shuffle alphabet'')
        _ ->
          (sqid', alphabet'')

--decodeWithAlphabet :: String -> String -> [Int]
decodeWithAlphabet _alphabet sqid
  -- If an empty string is given, or if any character in the string is missing
  -- from the alphabet, then return an empty list
  | null sqid || not (all (`elem` _alphabet) sqid) = []
  | otherwise =
      case partitionIndex of
        Nothing -> undefined
        Just xx -> undefined
  where
    -- First character is always the `prefix`
    prefix = head sqid

    -- Semi-random position that was generated during encoding
    offset = unsafeElemIndex prefix _alphabet

    -- Re-arrange alphabet back into its original form
    alphabet' = drop offset _alphabet <> take offset _alphabet

    -- `partition` character is in second position
    partition = alphabet' !! 1

    -- new alphabet without reserved `prefix` and `partition` character
    alphabet'' = drop 2 alphabet'

    -- Now it is safe to remove the prefix character from the ID, as it isn't
    -- needed anymore
    sqid' = drop 1 sqid

    partitionIndex = elemIndex partition sqid'

    -- If this ID contains the `partition` character (between first position
    -- and non-last position), throw away everything to the left of it,
    -- include the `partition` character



shuffle :: String -> String
shuffle _alphabet = foldl' mu _alphabet ixs
  where
    len = length _alphabet
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    --
    mu chars (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . (chars !!)
       in swapChars i r chars

toId :: Int -> String -> String
toId num _alphabet = reverse (unfoldr (fmap mu) (Just num))
  where
    len = length _alphabet
    mu n =
      let (m, r) = n `divMod` len
          next = if m == 0 then Nothing else Just m
       in (_alphabet !! r, next)

toNumber :: String -> String -> Int
toNumber sqid _alphabet = foldl' mu 0 sqid
  where
    len = length _alphabet
    mu v c =
      case elemIndex c _alphabet of
        Just n -> len * v + n
        _ -> error "toNumber: bad input"

isBlockedId :: (MonadSqids m) => String -> m Bool
isBlockedId = undefined
