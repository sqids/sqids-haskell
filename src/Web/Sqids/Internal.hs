{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Sqids.Internal
  ( sqidsVersion
  , SqidsOptions (..)
  , SqidsError (..)
  , SqidsContext (..)
  , emptySqidsContext
  , defaultSqidsOptions
  , SqidsStack
  , MonadSqids (..)
  , sqidsOptions
  , SqidsT (..)
  , Sqids (..)
  , runSqidsT
  , sqidsT
  , runSqids
  , sqids
  , filteredBlocklist
  , rearrangeAlphabet
  , encodeNumbers
  , decodeWithAlphabet
  , decodeStep
  , shuffle
  , toId
  , toNumber
  , isBlockedId
  )
where

import Control.Monad (when, (>=>))
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader, ReaderT, asks, local, runReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (isDigit, ord, toLower)
import Data.List (foldl', unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import Web.Sqids.Blocklist (defaultBlocklist)
import Web.Sqids.Utils.Internal (containsMultibyteChars, letterCount, swapChars, unsafeIndex, unsafeUncons, wordsNoLongerThan)

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "0.0.1"

-- | Options that can be passed to `runSqids` or `runSqidsT`.
data SqidsOptions = SqidsOptions
  { alphabet  :: !Text
  -- ^ The set of characters to use for encoding and decoding IDs.
  , minLength :: !Int
  -- ^ The minimum allowed length of IDs.
  , blocklist :: ![Text]
  -- ^ A list of words that must never appear in IDs.
  } deriving (Show, Eq, Ord)

-- | Default options
defaultSqidsOptions :: SqidsOptions
defaultSqidsOptions = SqidsOptions
  { alphabet  = Text.pack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , minLength = 0
  , blocklist = defaultBlocklist
  }

data SqidsContext = SqidsContext
  { sqidsAlphabet  :: !Text
  , sqidsMinLength :: !Int
  , sqidsBlocklist :: ![Text]
  } deriving (Show, Eq, Ord)

{-# INLINE emptySqidsContext #-}
emptySqidsContext :: SqidsContext
emptySqidsContext = SqidsContext Text.empty 0 []

data SqidsError
  = SqidsNegativeNumberInInput
  -- ^ One or more numbers in the list passed to `encode` are negative. Only
  --   non-negative integers can be used as input.
  | SqidsMaxEncodingAttempts
  -- ^ Maximum allowed attemps was reached during encoding
  | SqidsAlphabetContainsMultibyteCharacters
  -- ^ The alphabet cannot contain multi-byte characters.
  | SqidsAlphabetTooShort
  -- ^ The alphabet must be at least 3 characters long.
  | SqidsAlphabetRepeatedCharacters
  -- ^ The provided alphabet contains duplicate characters. E.g., "abcdefgg" is
  --   not a valid alphabet.
  | SqidsInvalidMinLength
  -- ^ The given `minLength` value is not within the valid range.
  deriving (Show, Read, Eq, Ord)

type SqidsStack m = ReaderT SqidsContext (ExceptT SqidsError m)

class (Monad m) => MonadSqids m where
  -- | Encode a list of integers into an ID
  sqidsEncode :: (Integral a)
         => [a]     -- ^ A list of non-negative numbers to encode
         -> m Text  -- ^ Returns the generated ID
  -- | Decode an ID back into a list of integers
  sqidsDecode :: (Integral a)
         => Text    -- ^ The encoded ID
         -> m [a]   -- ^ Returns a list of numbers

-- | Sqids constructor
sqidsOptions
  :: (MonadSqids m, MonadError SqidsError m)
  => SqidsOptions
  -> m SqidsContext
sqidsOptions SqidsOptions{..} = do

  let alphabetLetterCount = letterCount alphabet

  -- Check the alphabet doesn't contain multibyte characters
  when (containsMultibyteChars alphabet) $
    throwError SqidsAlphabetContainsMultibyteCharacters

  -- Check the length of the alphabet
  when (Text.length alphabet < 3) $
    throwError SqidsAlphabetTooShort

  -- Check that the alphabet has only unique characters
  when (alphabetLetterCount /= Text.length alphabet) $
    throwError SqidsAlphabetRepeatedCharacters

  -- Validate min. length
  when (minLength < 0 || minLength > 255) $
    throwError SqidsInvalidMinLength

  pure $ SqidsContext
    { sqidsAlphabet  = shuffle alphabet
    , sqidsMinLength = minLength
    , sqidsBlocklist = filteredBlocklist alphabet blocklist
    }

-- | Sqids monad transformer
newtype SqidsT m a = SqidsT {unwrapSqidsT :: SqidsStack m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SqidsContext
    , MonadError SqidsError
    , MonadIO
    )

instance MonadTrans SqidsT where
  lift = SqidsT . lift . lift

instance (Monad m) => MonadSqids (SqidsT m) where
  sqidsEncode numbers
    | null numbers =
        -- If no numbers passed, return an empty string
        pure Text.empty
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise =
        encodeNumbers numbers 0

  sqidsDecode sqid =
    asks (decodeWithAlphabet . sqidsAlphabet) <*> pure sqid

newtype Sqids a = Sqids {unwrapSqids :: SqidsT Identity a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SqidsContext
    , MonadError SqidsError
    , MonadSqids
    )

-- | Evaluate a `SqidsT` computation with the given options.
runSqidsT :: (Monad m) => SqidsOptions -> SqidsT m a -> m (Either SqidsError a)
runSqidsT options value =
  runExceptT (runReaderT (unwrapSqidsT withOptions) emptySqidsContext)
  where
    withOptions = sqidsOptions options >>= (`local` value) . const

-- | Evaluate a `SqidsT` computation with the default options. This is a short
--   form for `runSqidsT defaultSqidsOptions`.
sqidsT :: (Monad m) => SqidsT m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

-- | Evaluate a `Sqids` computation with the given options.
runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options . unwrapSqids

-- | Evaluate a `Sqids` computation with the default options. This is a short
--   form for `runSqids defaultSqidsOptions`.
sqids :: Sqids a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

instance (MonadSqids m) => MonadSqids (StateT s m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m) => MonadSqids (ContT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

-- Clean up blocklist:
--
--   1. All words must be lowercase
--   2. No words should be less than three characters long
--   3. Remove words that contain characters that are not in the alphabet
--
filteredBlocklist :: Text -> [Text] -> [Text]
filteredBlocklist alph ws = filter isValid (Text.map toLower <$> ws)
  where
    isValid w = Text.length w >= 3 && Text.all (`Text.elem` lowercaseAlphabet) w
    lowercaseAlphabet = Text.map toLower alph

decodeStep :: (Integral a) => (Text, Text) -> Maybe (a, (Text, Text))
decodeStep (sqid, alph)
  | Text.null sqid = Nothing
  | otherwise = do
      case Text.uncons alph of
        Just (separatorChar, alphabetWithoutSeparator) ->
          let separator = Text.singleton separatorChar
           in case Text.splitOn separator sqid of
              [] ->
                Nothing
              (chunk : chunks)
                | Text.null chunk ->
                    Nothing
                | otherwise -> Just
                    ( toNumber chunk alphabetWithoutSeparator
                    , (Text.intercalate separator chunks, shuffle alph)
                    )
        _ ->
          error "decode: bad input"

decodeWithAlphabet :: (Integral a) => Text -> Text -> [a]
decodeWithAlphabet alph sqid
  | Text.null sqid || not (Text.all (`Text.elem` alph) sqid) = []
  | otherwise = unfoldr decodeStep (slicedId, Text.reverse chars)
  where
    offset = unsafeIndex prefix alph
    (prefix, slicedId) = unsafeUncons sqid
    chars = Text.drop offset alph <> Text.take offset alph

shuffle :: Text -> Text
shuffle alph =
  foldl' mu alph [(i, j) | i <- [0 .. len - 2], let j = len - i - 1]
  where
    len = Text.length alph
    mu chars (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . (chars `Text.index`)
       in swapChars i r chars

toId :: (Integral a) => a -> Text -> Text
toId num alph = Text.reverse (Text.unfoldr (fmap mu) (Just num))
  where
    len = fromIntegral (Text.length alph)
    mu n =
      let (m, r) = n `divMod` len
          next = if m == 0 then Nothing else Just m
       in (Text.index alph (fromIntegral r), next)

toNumber :: (Integral a) => Text -> Text -> a
toNumber sqid alph = Text.foldl' mu 0 sqid
  where
    len = fromIntegral (Text.length alph)
    mu v c =
      case Text.findIndex (== c) alph of
        Just n -> len * v + fromIntegral n
        _ -> error "toNumber: bad input"

isBlockedId :: [Text] -> Text -> Bool
isBlockedId bls sqid =
  any disallowed (wordsNoLongerThan (Text.length sqid) bls)
  where
    lowercaseSqid = Text.map toLower sqid
    disallowed w
      | Text.length sqid <= 3 || Text.length w <= 3 =
          -- Short words have to match exactly
          w == lowercaseSqid
      | Text.any isDigit w =
          -- Look for "leetspeak" words
          w `Text.isPrefixOf` lowercaseSqid || w `Text.isSuffixOf` lowercaseSqid
      | otherwise =
          -- Check if word appears anywhere in the string
          w `Text.isInfixOf` lowercaseSqid

-- Rearrange alphabet so that second half goes in front of the first half
rearrangeAlphabet :: (Integral a) => Int -> Text -> [a] -> Text
rearrangeAlphabet increment alph numbers =
  Text.drop offset alph <> Text.take offset alph
  where
    offset = (increment + foldl' mu (length numbers) (zip numbers [0 ..])) `mod` len
    len = Text.length alph

    mu :: (Integral a, Num b) => b -> (a, b) -> b
    mu a (v, i) =
      let currentChar = Text.index alph (fromIntegral (v `mod` fromIntegral len))
       in fromIntegral (ord currentChar) + i + a

encodeNumbers ::
  ( Integral a
  , MonadSqids m
  , MonadError SqidsError m
  , MonadReader SqidsContext m
  ) => [a] -> Int -> m Text
encodeNumbers numbers increment = do
  alph <- asks sqidsAlphabet
  when (increment > Text.length alph) $
    throwError SqidsMaxEncodingAttempts
  let alphabet = rearrangeAlphabet increment alph numbers
  let run (r, chars) (n, i)
        | i == length numbers - 1 =
            (sqid, chars)
        | otherwise =
            (sqid <> Text.singleton head_, shuffle chars)
        where
          (head_, tail_) = unsafeUncons chars
          sqid = r <> toId n tail_
  let (sqid, chars) =
        foldl' run (Text.singleton (Text.head alphabet), Text.reverse alphabet) (zip numbers [0 ..])
  (makeMinLength chars >=> checkAgainstBlocklist) sqid
  where
    makeMinLength chars sqid = do
      minl <- asks sqidsMinLength
      if minl > Text.length sqid
        then
          let len = Text.length chars
              go (chars_, sqid_) = do
                let diff = minl - Text.length sqid_
                    shuffled = shuffle chars_
                if diff > 0
                  then go (shuffled, sqid_ <> Text.take (min diff len) shuffled)
                  else sqid_
           in pure (go (chars, Text.snoc sqid (Text.head chars)))
        else
          pure sqid

    checkAgainstBlocklist sqid = do
      blocklist <- asks sqidsBlocklist
      if isBlockedId blocklist sqid
        then encodeNumbers numbers (succ increment)
        else pure sqid
