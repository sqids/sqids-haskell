{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Sqids.Internal
  ( SqidsOptions(..)
  , SqidsError(..)
  , SqidsState(..)
  , emptySqidsState
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
  , curatedBlocklist
  , encodeNumbers
  , encodeNumbers_
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
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, put, get)
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
--import Debug.Trace (traceShow)
import Web.Sqids.Blocklist (defaultBlocklist)
import Web.Sqids.Utils.Internal (letterCount, swapChars, wordsNoLongerThan, unsafeIndex, unsafeUncons)

import qualified Data.Text as Text

data SqidsOptions = SqidsOptions
  { alphabet  :: Text
  -- ^ URL-safe characters
  , minLength :: Int                             -- TODO: Maybe Int
  -- ^ The minimum allowed length of IDs
  , blocklist :: [Text]
  -- ^ A list of words that must never appear in IDs
  } deriving (Show, Eq, Ord)

defaultSqidsOptions :: SqidsOptions
defaultSqidsOptions = SqidsOptions
  { alphabet  = Text.pack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blocklist = defaultBlocklist
  }

data SqidsState = SqidsState Text Int [Text]
  deriving (Show, Eq, Ord)

emptySqidsState :: SqidsState
emptySqidsState = SqidsState Text.empty 0 []

data SqidsError
  = SqidsAlphabetTooShort
  | SqidsAlphabetRepeatedCharacters
  | SqidsInvalidMinLength
  | SqidsNegativeNumberInInput
  deriving (Show, Read, Eq, Ord)

type SqidsStack m = StateT SqidsState (ExceptT SqidsError m)

class (Monad m) => MonadSqids m where
  encode       :: [Int] -> m Text
  decode       :: Text -> m [Int]
  getAlphabet  :: m Text
  setAlphabet  :: Text -> m ()
  getMinLength :: m Int
  setMinLength :: Int -> m ()
  getBlocklist :: m [Text]
  setBlocklist :: [Text] -> m ()

-- | SqidsOptions constructor
sqidsOptions
  :: (MonadSqids m, MonadError SqidsError m)
  => SqidsOptions
  -> m SqidsState
sqidsOptions SqidsOptions{..} = do

  let alphabetLetterCount = letterCount alphabet

  -- Check the length of the alphabet
  when (Text.length alphabet < 5) $
    throwError SqidsAlphabetTooShort

  -- Check that the alphabet has only unique characters
  when (alphabetLetterCount /= Text.length alphabet) $
    throwError SqidsAlphabetRepeatedCharacters

  -- Validate min. length
  when (minLength < 0 || minLength > alphabetLetterCount) $
    throwError SqidsInvalidMinLength

  pure $ SqidsState (shuffle alphabet) minLength (curatedBlocklist alphabet blocklist)

newtype SqidsT m a = SqidsT { unwrapSqidsT :: SqidsStack m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState SqidsState
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
        encodeNumbers_ numbers False

  decode sqid = do
    chars <- getAlphabet
    pure (decodeWithAlphabet chars sqid)

  getAlphabet = do
    (SqidsState _alphabet _ _) <- get
    pure _alphabet

  setAlphabet newAlphabet = undefined
  --
  getMinLength = undefined
  setMinLength newMinLength = undefined
  --
  getBlocklist = do
    (SqidsState _ _ _blocklist) <- get
    pure _blocklist

  setBlocklist newBlocklist = undefined

-- | Internal function that encodes a list of unsigned integers into an ID
encodeNumbers_ :: (MonadState SqidsState m, MonadSqids m) => [Int] -> Bool -> m Text
encodeNumbers_ numbers partitioned = do
  (SqidsState chars minlen wlist) <- get
  let sqid = encodeNumbers chars numbers partitioned

  -- if `minLength` is used and the ID is too short, add a throwaway number
  when (minlen > Text.length sqid) $ do
    -- TODO
    undefined

  -- If the ID has a blocked word anywhere, add a throwaway number and
  -- start over
  if isBlockedId wlist sqid
    then encodeNumbers_ newNumbers True
    else pure sqid

  where
    newNumbers
      -- TODO
      | partitioned = 
          if 0 == 1  -- TODO
            then undefined
            else 
              let (m:ms) = numbers
               in (m + 1):ms
      | otherwise = 0 : numbers

newtype Sqids a = Sqids { unwrapSqids :: SqidsT Identity a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState SqidsState
    , MonadError SqidsError
    , MonadSqids
    )

runSqidsT :: (Monad m) => SqidsOptions -> SqidsT m a -> m (Either SqidsError a)
runSqidsT options _sqids =
  runExceptT (evalStateT (unwrapSqidsT withOptions) emptySqidsState)
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
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode       = lift . encode
  decode       = lift . decode
  getAlphabet  = lift getAlphabet
  setAlphabet  = lift . setAlphabet
  getMinLength = lift getMinLength
  setMinLength = lift . setMinLength
  getBlocklist = lift getBlocklist
  setBlocklist = lift . setBlocklist

-- Clean up blocklist:
--
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
--
curatedBlocklist :: Text -> [Text] -> [Text]
curatedBlocklist _alphabet ws = (Text.map toLower) <$> filter isValid ws where
  isValid w = Text.length w >= 3 && Text.all (`Text.elem` _alphabet) w

encodeNumbers :: Text -> [Int] -> Bool -> Text
encodeNumbers _alphabet numbers partitioned =
    fst $ foldl' run (Text.singleton prefix, alphabet') (zip numbers [0..])
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
    run (r, chars) (n, i) =
      let
        barrier
          | i == length numbers - 1 = Text.empty
          | otherwise = Text.singleton $
              if partitioned && i == 0 then partition else Text.last chars
      in
        (r <> toId n (Text.init chars) <> barrier, shuffle chars)

decodeId :: Text -> Text -> [Int]
decodeId = curry (unfoldr mu)
  where
    mu (sqid, alphabet)
      | Text.null sqid = Nothing
      | otherwise =
          case Text.unsnoc alphabet of
            Just (alphabetWithoutSeparator, separatorChar) ->
              let separator = Text.singleton separatorChar in
                case Text.splitOn separator sqid of
                  [] -> Nothing
                  (chunk : chunks) -> Just
                    ( toNumber chunk alphabetWithoutSeparator
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
isBlockedId blocklist sqid = any disallowed filteredList
  where
    filteredList = wordsNoLongerThan (Text.length sqid) blocklist
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
