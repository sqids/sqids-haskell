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
  , decodeId
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
import Data.Char (ord, toLower, isDigit)
import Data.List (foldl', unfoldr, elemIndex, intersect, nub, null, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Debug.Trace (traceShow)
import Web.Sqids.Utils.Internal (letterCount, swapChars, wordsNoLongerThan, unsafeIndex, unsafeUncons)

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

  let alphabetLetterCount = letterCount alphabet

  -- Check the length of the alphabet
  when (Text.length alphabet < 5) $
    throwError SqidsAlphabetTooShort

  -- Check that the alphabet has only unique characters
  when (alphabetLetterCount < Text.length alphabet) $
    throwError SqidsAlphabetRepeatedCharacters

  -- Validate min. length
  when (minLength < 0 || minLength > alphabetLetterCount) $
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
        pure (encodeNumbers undefined numbers False)

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
--
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
--
curatedBlacklist :: Text -> [Text] -> [Text]
curatedBlacklist _alphabet ws = (Text.map toLower) <$> filter isValid ws where
  isValid w = Text.length w >= 3 && Text.all (`Text.elem` _alphabet) w

-- | Internal function that encodes a list of unsigned integers into an ID
encodeNumbers :: Text -> [Int] -> Bool -> Text
encodeNumbers _alphabet numbers partitioned =
    fst $ foldl' bu (Text.singleton prefix, alphabet') (zip numbers [0..])
    --foo alphabet' numbers (Text.singleton prefix)
  where
    len = Text.length _alphabet
    temp = Text.drop offset _alphabet <> Text.take offset _alphabet
    --
    alphabet' = Text.drop 2 temp
    prefix = Text.index temp 0
    partition = Text.index temp 1
    --
    offset = 
      foldl' mu (length numbers) (zip numbers [0..]) `mod` len
    mu a (v, i) =
      let currentChar = Text.index _alphabet (v `mod` len)
       in ord currentChar + i + a
    --
    bu (r, chars) (n, i) = 
      let
        barrier
          | i == length numbers - 1 = Text.empty
          | otherwise =
              Text.singleton $ 
                if partitioned && i == 0 then partition else separator
        bork = Text.init chars
        separator = Text.last chars
      in
        (r <> toId n bork <> barrier, shuffle chars)

decodeId :: Text -> Text -> [Int]
decodeId = curry (unfoldr mu)
  where
    mu (sqid, alphabet)
      | Text.null sqid = Nothing
      | otherwise =
          case Text.unsnoc alphabet of
            Just (alphabetBarSeparator, separatorChar) ->
              let separator = Text.singleton separatorChar in
                case Text.splitOn separator sqid of
                  [] -> Nothing
                  (chunk : chunks) -> Just
                    ( toNumber chunk alphabetBarSeparator
                    , (Text.intercalate separator chunks, shuffle alphabet)
                    )
            _ ->
              error "decodeId: bad input"

decodeWithAlphabet :: Text -> Text -> [Int]
decodeWithAlphabet _alphabet sqid
  | Text.null sqid || not (Text.all (`Text.elem` _alphabet) sqid) =
      []
  | otherwise = uncurry decodeId $
      case Text.findIndex (== partition) next of
        Just n | n > 0 && n < Text.length next - 1 ->
          (Text.drop (n + 1) next, shuffle chars)
        _ ->
          (next, chars)
  where
    offset = unsafeIndex prefix _alphabet

    (prefix, next) = unsafeUncons sqid
    (partition, chars) =
      unsafeUncons (Text.drop (offset + 1) _alphabet <> Text.take offset _alphabet)

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

toNumber :: Text -> Text -> Int
toNumber sqid _alphabet = Text.foldl' mu 0 sqid
  where
    len = Text.length _alphabet
    mu v c =
      case Text.findIndex (== c) _alphabet of
        Just n -> len * v + n
        _ -> error "toNumber: bad input"

isBlockedId :: [Text] -> Text -> Bool
isBlockedId blacklist sqid = any disallowed filteredList
  where
    filteredList = wordsNoLongerThan (Text.length sqid) blacklist
    lowercaseSqid = Text.map toLower sqid
    --
    disallowed :: Text -> Bool
    disallowed w
      | Text.length lowercaseSqid <= 3 || Text.length w <= 3 =
        -- Short words have to match exactly
        w == lowercaseSqid
      | Text.any isDigit w =
        -- Look for "leetspeak" words
        w `Text.isPrefixOf` lowercaseSqid || w `Text.isSuffixOf` lowercaseSqid
      | otherwise =
        -- Check if word appears anywhere in the string
        w `Text.isInfixOf` lowercaseSqid
