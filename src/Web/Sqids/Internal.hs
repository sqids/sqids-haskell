{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Sqids.Internal
  ( sqidsVersion
  , SqidsOptions(..)
  , SqidsError(..)
  , SqidsContext(..)
  , emptySqidsContext
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
  , filteredBlocklist
  , rearrangeAlphabet
  , encodeNumbers
  , decodeWithAlphabet
  , decodeStep
  , shuffle
  , toId
  , toNumber
  , isBlockedId
  ) where

import Control.Monad (when, (>=>))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks, local)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord, toLower, isDigit)
import Data.List (foldl', unfoldr)
import Data.Text (Text)
import Web.Sqids.Blocklist (defaultBlocklist)
import Web.Sqids.Utils.Internal (letterCount, swapChars, wordsNoLongerThan, unsafeIndex, unsafeUncons)

import qualified Data.Text as Text

-- | Sqids spec. version
sqidsVersion :: String
sqidsVersion = "?"

data SqidsOptions = SqidsOptions
  { alphabet  :: !Text
  -- ^ URL-safe characters
  , minLength :: !Int
  -- ^ The minimum allowed length of IDs
  , blocklist :: ![Text]
  -- ^ A list of words that must never appear in IDs
  } deriving (Show, Eq, Ord)

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
  = SqidsAlphabetTooShort
  | SqidsAlphabetRepeatedCharacters
  | SqidsInvalidMinLength
  | SqidsNegativeNumberInInput
  deriving (Show, Read, Eq, Ord)

type SqidsStack m = ReaderT SqidsContext (ExceptT SqidsError m)

class (Monad m) => MonadSqids m where
  -- | Encode a list of unsigned integers into an ID
  encode :: [Int] -> m Text
  -- | Decode an ID back into an array of unsigned integers
  decode :: Text -> m [Int]

-- | Sqids constructor
sqidsOptions
  :: (MonadSqids m, MonadError SqidsError m)
  => SqidsOptions
  -> m SqidsContext
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

  pure $ SqidsContext
    { sqidsAlphabet  = shuffle alphabet
    , sqidsMinLength = minLength
    , sqidsBlocklist = filteredBlocklist alphabet blocklist
    }

newtype SqidsT m a = SqidsT { unwrapSqidsT :: SqidsStack m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SqidsContext
    , MonadError SqidsError
    )

instance MonadTrans SqidsT where
  lift = SqidsT . lift . lift

instance (Monad m) => MonadSqids (SqidsT m) where
  encode numbers
    | null numbers =
        -- If no numbers passed, return an empty string
        pure Text.empty
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise =
        encodeNumbers numbers False

  decode sqid = decodeWithAlphabet <$> asks sqidsAlphabet <*> pure sqid

newtype Sqids a = Sqids { unwrapSqids :: SqidsT Identity a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SqidsContext
    , MonadError SqidsError
    , MonadSqids
    )

runSqidsT :: (Monad m) => SqidsOptions -> SqidsT m a -> m (Either SqidsError a)
runSqidsT options value =
  runExceptT (runReaderT (unwrapSqidsT withOptions) emptySqidsContext)
  where
    withOptions = sqidsOptions options >>= (`local` value) . const

sqidsT :: (Monad m) => SqidsT m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options . unwrapSqids

sqids :: Sqids a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

instance (MonadSqids m) => MonadSqids (StateT s m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode = lift . encode
  decode = lift . decode

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode = lift . encode
  decode = lift . decode

-- Clean up blocklist:
--
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
--
filteredBlocklist :: Text -> [Text] -> [Text]
filteredBlocklist alph ws = (Text.map toLower) <$> filter isValid ws where
  isValid w = Text.length w >= 3 && Text.all (`Text.elem` alph) w

decodeStep :: (Text, Text) -> Maybe (Int, (Text, Text))
decodeStep (sqid, alph)
  | Text.null sqid = Nothing
  | otherwise =
      case Text.unsnoc alph of
        Just (alphabetWithoutSeparator, separatorChar) ->
          let separator = Text.singleton separatorChar in
            case Text.splitOn separator sqid of
              [] -> Nothing
              (chunk : chunks) -> Just
                ( toNumber chunk alphabetWithoutSeparator
                , (Text.intercalate separator chunks, shuffle alph)
                )
        _ ->
          error "decodeId: bad input"

decodeWithAlphabet :: Text -> Text -> [Int]
decodeWithAlphabet alph sqid
  | Text.null sqid || not (Text.all (`Text.elem` alph) sqid) = []
  | otherwise = unfoldr decodeStep initial
  where
    offset = unsafeIndex prefix alph
    (prefix, next) = unsafeUncons sqid
    (partition, chars) =
      unsafeUncons (Text.drop (offset + 1) alph <> Text.take offset alph)
    initial =
      case Text.findIndex (== partition) next of
        Just n | n > 0 && n < Text.length next - 1 ->
          (Text.drop (n + 1) next, shuffle chars)
        _ ->
          (next, chars)

shuffle :: Text -> Text
shuffle alph =
  foldl' mu alph [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
  where
    len = Text.length alph
    mu chars (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . (chars `Text.index`)
       in swapChars i r chars

toId :: Int -> Text -> Text
toId num alph = Text.reverse (Text.unfoldr (fmap mu) (Just num))
  where
    len = Text.length alph
    mu n =
      let (m, r) = n `divMod` len
          next = if m == 0 then Nothing else Just m
       in (Text.index alph r, next)

toNumber :: Text -> Text -> Int
toNumber sqid alph = Text.foldl' mu 0 sqid
  where
    len = Text.length alph
    mu v c =
      case Text.findIndex (== c) alph of
        Just n -> len * v + n
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
rearrangeAlphabet :: Text -> [Int] -> Text
rearrangeAlphabet alph numbers =
  Text.drop offset alph <> Text.take offset alph
  where
    len = Text.length alph
    offset = foldl' mu (length numbers) (zip numbers [0..]) `mod` len
    mu a (v, i) =
      let currentChar = Text.index alph (v `mod` len)
       in ord currentChar + i + a

encodeNumbers ::
  (MonadSqids m, MonadReader SqidsContext m) => [Int] -> Bool -> m Text
encodeNumbers numbers partitioned = do
  alph <- asks sqidsAlphabet
  let (left, right) = Text.splitAt 2 (rearrangeAlphabet alph numbers)
  case Text.unpack left of
    prefix : partition : _ -> do
      let run (r, chars) (n, i)
            | i == length numbers - 1 =
                (sqid, chars)
            | otherwise =
                (sqid <> Text.singleton delim, shuffle chars)
            where
              delim = if partitioned && i == 0 then partition else Text.last chars
              sqid = r <> toId n (Text.init chars)
      let (sqid, chars) =
            foldl' run (Text.singleton prefix, right) (zip numbers [0..])
      (makeMinLength chars >=> checkAgainstBlocklist numbers) sqid
    _ ->
      error "encodeNumbers: implementation error"
  where
    makeMinLength chars sqid = do
      minl <- asks sqidsMinLength
      sqid' <-
        if minl <= Text.length sqid || partitioned
          then pure sqid
          else encodeNumbers (0 : numbers) True
      pure $
        if minl <= Text.length sqid'
          then sqid'
          else let extra = minl - Text.length sqid
                in Text.cons (Text.head sqid') (Text.take extra chars <> Text.tail sqid')

    checkAgainstBlocklist nums sqid = do
      bls <- asks sqidsBlocklist
      if isBlockedId bls sqid then
        case nums of
          n : ns ->
            encodeNumbers (if partitioned then n + 1 : ns else 0 : n : ns) True
          _ ->
            error "encodeNumbers: implementation error"
        else
          pure sqid
