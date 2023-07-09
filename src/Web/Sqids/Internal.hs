{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Web.Sqids.Internal
  ( sqidsVersion
  , SqidsOptions(..)
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
  , filteredBlocklist
  , encodeNumbersWithAlphabet
  , decodeWithAlphabet
  , decodeStep
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
import Control.Monad.State.Strict (StateT, MonadState, evalStateT, put, get, gets)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (ord, toLower, isDigit)
import Data.List (foldl', unfoldr, elemIndex, intersect, nub, null, intercalate)
import Data.Text (Text)
import Debug.Trace (traceShow, traceShowM)
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

data SqidsState = SqidsState 
  { sqidsAlphabet  :: !Text 
  , sqidsMinLength :: !Int
  , sqidsBlocklist :: ![Text]
  } deriving (Show, Eq, Ord)

{-# INLINE emptySqidsState #-}
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
  -- | Encode a list of unsigned integers into an ID
  encode       :: [Int] -> m Text
  -- | Decode an ID back into an array of unsigned integers
  decode       :: Text -> m [Int]
--  getAlphabet  :: m Text
--  getMinLength :: m Int
--  getBlocklist :: m [Text]

-- | Sqids constructor
sqidsOptions
  :: (MonadSqids m, MonadState SqidsState m, MonadError SqidsError m)
  => SqidsOptions
  -> m ()
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

  put $ SqidsState (shuffle alphabet) minLength (filteredBlocklist alphabet blocklist)

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
  encode numbers
    | null numbers =
        -- If no numbers passed, return an empty string
        pure Text.empty
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise = do
        (SqidsState chars minLength wlist) <- get
        pure $ encodeNumbersWithAlphabet wlist minLength chars numbers False

  decode sqid = decodeWithAlphabet <$> gets sqidsAlphabet <*> pure sqid

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
runSqidsT options value =
  runExceptT (evalStateT (unwrapSqidsT withOptions) emptySqidsState)
  where
    withOptions = sqidsOptions options >> value

sqidsT :: (Monad m) => SqidsT m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options . unwrapSqids

sqids :: Sqids a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

instance (MonadSqids m) => MonadSqids (StateT s m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m) => MonadSqids (ExceptT e m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m) => MonadSqids (ReaderT r m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m, Monoid w) => MonadSqids (WriterT w m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m) => MonadSqids (MaybeT m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m) => MonadSqids (ContT r m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

instance (MonadSqids m) => MonadSqids (SelectT r m) where
  encode       = lift . encode
  decode       = lift . decode
--  getAlphabet  = lift getAlphabet
--  getMinLength = lift getMinLength
--  getBlocklist = lift getBlocklist

-- Clean up blocklist:
--
--   1. All words must be lowercase
--   2. No words should be less than three characters
--   3. Remove words that contain characters that are not in the alphabet
--
filteredBlocklist :: Text -> [Text] -> [Text]
filteredBlocklist alph ws = (Text.map toLower) <$> filter isValid ws where
  isValid w = Text.length w >= 3 && Text.all (`Text.elem` alph) w

xxx wlist minlen chars numbers partitioned sqid 
  | minlen <= Text.length sqid || partitioned = 
      sqid
  | otherwise = 
      encodeNumbersWithAlphabet wlist minlen chars (0 : numbers) True

--yyy :: Text -> Text
yyy minlen chars bars numbers partitioned sqid 
  | minlen <= Text.length sqid =
      sqid
  | otherwise = 
        let extra = minlen - Text.length sqid
         in Text.cons (Text.head sqid) (Text.take extra bars <> Text.tail sqid)

zzz wlist minlen alph (n : ns) partitioned sqid 
  | isBlockedId wlist sqid = 
      if partitioned 
          then encodeNumbersWithAlphabet wlist minlen alph (n + 1 : ns) True
          else encodeNumbersWithAlphabet wlist minlen alph (0 : n : ns) True
  | otherwise = 
      sqid

encodeNumbersWithAlphabet wlist minlen alph numbers partitioned =
  case Text.unpack xx of
    [] -> 
      error "encodeNumbersWithAlphabet: implementation error"
    prefix : partition : _ ->
          -- TODO: Reader monad
          let gork = xxx wlist minlen alph numbers partitioned zork 
              (zork, tork) = foobaz prefix partition
           in 
                 zzz wlist minlen alph numbers partitioned (yyy minlen alph tork numbers partitioned gork)
  where
    (xx, newAlphabet) = Text.splitAt 2 (rearrangeAlphabet alph numbers)

    foobaz prefix partition = 
        foldl' run (Text.singleton prefix, newAlphabet) (zip numbers [0..])
      where
        run (r, chars) (n, i) =
          let
            barrier
              | i == length numbers - 1 = 
                  Text.empty
              | otherwise = Text.singleton $
                  if partitioned && i == 0 then partition else Text.last chars
           in
            ( r <> toId n (Text.init chars) <> barrier
            , if i == length numbers - 1 then chars else shuffle chars
            )

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
shuffle alph = foldl' mu alph ixs
  where
    len = Text.length alph
    ixs = [ (i, j) | i <- [ 0 .. len - 2 ], let j = len - i - 1 ]
    --
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
isBlockedId blocklist sqid =
    any disallowed (wordsNoLongerThan (Text.length sqid) blocklist)
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
