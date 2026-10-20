{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Sqids.Internal
  ( sqidsVersion
  , SqidsOptions (..)
  , SqidsError (..)
  , SqidsContext (..)
  , emptySqidsContext
  , defaultSqidsOptions
  , SqidsStack
  , MonadSqids (..)
  , sqidsContext
  , mkSqidsContext
  , SqidsT (..)
  , Sqids
  , runSqidsT
  , runSqidsTWith
  , sqidsT
  , runSqids
  , runSqidsWith
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

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Select (SelectT)
import Control.Monad.Writer (WriterT)
import Data.Char (isDigit, ord, toLower)
import Data.Text (Text)
import Web.Sqids.Blocklist (defaultBlocklist)
import Web.Sqids.Internal.Core
  ( Blocklist (..)
  , Core (..)
  , CoreError (..)
  , asciiByteArrayToText
  , asciiShortToText
  , asciiTextToByteArray
  , coreFromShuffled
  , decodeCore
  , defaultAlphabet
  , encodeCore
  , mkCore
  , shuffleBytes
  , textBytes
  )
import Web.Sqids.Utils.Internal
  ( containsMultibyteChars
  , swapChars
  , unsafeIndex
  , unsafeUncons
  , wordsNoLongerThan
  )

import qualified Data.List as List
import qualified Data.Text as Text

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
  { alphabet  = defaultAlphabet
  , minLength = 0
  , blocklist = defaultBlocklist
  }

-- | The validated, precomputed state that a `SqidsT` computation runs against.
--
--   Building one of these is the expensive part of using this library, and it
--   depends only on the `SqidsOptions`. Build it once — see `mkSqidsContext` —
--   and keep it for the lifetime of your program rather than reconstructing it
--   per ID.
--
--   @sqidsCore@ holds the byte-level representation that encoding and decoding
--   actually run on. The other three fields are derived views of the same
--   thing, kept for compatibility.
data SqidsContext s = SqidsContext
  { sqidsAlphabet  :: !Text
  , sqidsMinLength :: !Int
  , sqidsBlocklist :: [Text]
  -- ^ Lazy: a view of the filtered blocklist, computed only if asked for.
  , sqidsCore      :: Core
  -- ^ Lazy on purpose, so that `emptySqidsContext` is still a usable value.
  }

-- | Equality, ordering and display compare the options a context was built
--   from; @sqidsCore@ is a function of those, so this agrees with structural
--   equality on the whole record.
instance Eq (SqidsContext s) where
  a == b = view a == view b

instance Ord (SqidsContext s) where
  compare a b = compare (view a) (view b)

instance Show (SqidsContext s) where
  showsPrec d SqidsContext{..} =
    showParen (d > 10) $
      showString "SqidsContext "
        . showsPrec 11 sqidsAlphabet . showChar ' '
        . showsPrec 11 sqidsMinLength . showChar ' '
        . showsPrec 11 sqidsBlocklist

{-# INLINE view #-}
view :: SqidsContext s -> (Text, Int, [Text])
view SqidsContext{..} = (sqidsAlphabet, sqidsMinLength, sqidsBlocklist)

{-# INLINE emptySqidsContext #-}
emptySqidsContext :: SqidsContext s
emptySqidsContext = SqidsContext
  { sqidsAlphabet  = Text.empty
  , sqidsMinLength = 0
  , sqidsBlocklist = []
  , sqidsCore      = error "Web.Sqids: emptySqidsContext has no alphabet to encode with"
  }

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

fromCoreError :: CoreError -> SqidsError
fromCoreError CoreMultibyteAlphabet  = SqidsAlphabetContainsMultibyteCharacters
fromCoreError CoreAlphabetTooShort   = SqidsAlphabetTooShort
fromCoreError CoreRepeatedCharacters = SqidsAlphabetRepeatedCharacters
fromCoreError CoreInvalidMinLength   = SqidsInvalidMinLength

type SqidsStack s m = ReaderT (SqidsContext s) (ExceptT SqidsError m)

class (Monad m) => MonadSqids s m | m -> s where
  -- | Encode a list of integers into an ID
  sqidsEncode :: [s]     -- ^ A list of non-negative numbers to encode
              -> m Text  -- ^ Returns the generated ID
  -- | Decode an ID back into a list of integers
  sqidsDecode :: Text    -- ^ The encoded ID
              -> m [s]   -- ^ Returns a list of numbers

-- | Validate `SqidsOptions` and precompute everything derived from them.
--
--   Pure and total: unlike `sqidsContext` this does not need to run inside a
--   Sqids computation.
mkSqidsContext :: SqidsOptions -> Either SqidsError (SqidsContext s)
mkSqidsContext SqidsOptions{..} =
  case mkCore alphabet minLength blocklist of
    Left err -> Left (fromCoreError err)
    Right core -> Right SqidsContext
      { sqidsAlphabet  = asciiByteArrayToText (coreAlphabet core) (coreLen core)
      , sqidsMinLength = minLength
      , sqidsBlocklist = map asciiShortToText (blKept (coreBlocklist core))
      , sqidsCore      = core
      }

-- | Sqids constructor
sqidsContext
  :: (MonadSqids s m, MonadError SqidsError m)
  => SqidsOptions
  -> m (SqidsContext s)
sqidsContext = either throwError pure . mkSqidsContext

-- | Sqids monad transformer
newtype SqidsT s m a = SqidsT { unwrapSqidsT :: SqidsStack s m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (SqidsContext s)
    , MonadError SqidsError
    , MonadIO
    )

instance MonadTrans (SqidsT s) where
  lift = SqidsT . lift . lift

instance (Integral s, Monad m) => MonadSqids s (SqidsT s m) where
  sqidsEncode numbers
    | null numbers =
        -- If no numbers passed, return an empty string
        pure Text.empty
    | any (< 0) numbers =
        -- Don't allow negative integers
        throwError SqidsNegativeNumberInInput
    | otherwise =
        encodeNumbers numbers 0

  sqidsDecode sqid = do
    core <- asks sqidsCore
    let (arr, off, len) = textBytes sqid
    pure (decodeCore core arr off len)

-- | Sqids monad
type Sqids s = SqidsT s Identity

-- | Evaluate a `SqidsT` computation with the given options.
--
--   This validates the options and precomputes a `SqidsContext` on every call.
--   If you are generating more than a handful of IDs, build the context once
--   with `mkSqidsContext` and use `runSqidsTWith` instead — on the default
--   options that is the difference between ~3.5 µs and ~0.3 µs per ID.
runSqidsT :: (Monad m) => SqidsOptions -> SqidsT s m a -> m (Either SqidsError a)
runSqidsT options value =
  case mkSqidsContext options of
    Left err  -> pure (Left err)
    Right ctx -> runSqidsTWith ctx value

-- | Evaluate a `SqidsT` computation against a context you already hold.
--
--   Unlike `runSqidsT` this does no setup, so the cost of validating the
--   alphabet and preparing the blocklist is paid once rather than per call.
--   Build the context at startup and keep it:
--
-- > sqidsCtx :: SqidsContext Int
-- > sqidsCtx = either (error . show) id (mkSqidsContext myOptions)
-- >
-- > makeId :: [Int] -> Either SqidsError Text
-- > makeId = runSqidsWith sqidsCtx . encode
--
--   A `SqidsContext` is immutable and safe to share across threads.
--
--   Note the absence of a `Monad` constraint: there is no setup left that can
--   fail, so unlike `runSqidsT` this never has to inject a pure `Left`.
runSqidsTWith :: SqidsContext s -> SqidsT s m a -> m (Either SqidsError a)
runSqidsTWith ctx value = runExceptT (runReaderT (unwrapSqidsT value) ctx)

-- | Evaluate a `SqidsT` computation with the default options. This is a short
--   form for `runSqidsT defaultSqidsOptions`.
sqidsT :: (Monad m) => SqidsT s m a -> m (Either SqidsError a)
sqidsT = runSqidsT defaultSqidsOptions

-- | Evaluate a `Sqids` computation with the given options.
--
--   As with `runSqidsT`, this rebuilds the context on every call; see
--   `runSqidsWith`.
runSqids :: SqidsOptions -> Sqids s a -> Either SqidsError a
runSqids options = runIdentity . runSqidsT options

-- | Evaluate a `Sqids` computation against a context you already hold. The
--   pure counterpart of `runSqidsTWith`.
runSqidsWith :: SqidsContext s -> Sqids s a -> Either SqidsError a
runSqidsWith ctx = runIdentity . runSqidsTWith ctx

-- | Evaluate a `Sqids` computation with the default options. This is a short
--   form for `runSqids defaultSqidsOptions`.
sqids :: Sqids s a -> Either SqidsError a
sqids = runSqids defaultSqidsOptions

instance (MonadSqids s m) => MonadSqids s (StateT s m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m) => MonadSqids s (ExceptT e m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m) => MonadSqids s (ReaderT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m, Monoid w) => MonadSqids s (WriterT w m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m) => MonadSqids s (MaybeT m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m) => MonadSqids s (ContT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

instance (MonadSqids s m) => MonadSqids s (SelectT r m) where
  sqidsEncode = lift . sqidsEncode
  sqidsDecode = lift . sqidsDecode

-- | Encode a non-empty list of non-negative numbers, starting from the given
--   increment and raising it while the result is blocked.
{-# INLINABLE encodeNumbers #-}
encodeNumbers ::
  ( Integral s
  , MonadSqids s m
  , MonadError SqidsError m
  , MonadReader (SqidsContext s) m
  ) => [s] -> Int -> m Text
encodeNumbers numbers increment = do
  core <- asks sqidsCore
  case encodeCore core numbers increment (coreLen core) of
    Left _ -> throwError SqidsMaxEncodingAttempts
    Right (bytes, len) -> pure (asciiByteArrayToText bytes len)

--------------------------------------------------------------------------------
-- Compatibility shims
--
-- Nothing below is on the hot path any more: `encodeNumbers` and `sqidsDecode`
-- go straight to "Web.Sqids.Internal.Core". These are kept with their original
-- types and semantics because they are exported, and because the test suite
-- drives several of them directly from the spec's fixtures.
--
-- Where a byte path exists and the input is ASCII they dispatch to it;
-- otherwise they fall back to the original `Char`-based definition, so results
-- are unchanged even for inputs the library itself never produces.
--------------------------------------------------------------------------------

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

shuffle :: Text -> Text
shuffle alph
  | containsMultibyteChars alph = shuffleSlow alph
  | otherwise = asciiByteArrayToText (shuffleBytes bytes n) n
  where
    bytes = asciiTextToByteArray alph
    n = let (_, _, l) = textBytes alph in l

shuffleSlow :: Text -> Text
shuffleSlow alph =
  List.foldl' mu alph [(i, j) | i <- [0 .. len - 2], let j = len - i - 1]
  where
    len = Text.length alph
    mu chars (i, j) =
      let r = (i * j + ordAt i + ordAt j) `mod` len
          ordAt = ord . (chars `Text.index`)
       in swapChars i r chars

decodeWithAlphabet :: (Integral a) => Text -> Text -> [a]
decodeWithAlphabet alph sqid =
  case coreFromShuffled alph of
    Just core -> let (arr, off, len) = textBytes sqid in decodeCore core arr off len
    Nothing   -> decodeWithAlphabetSlow alph sqid

decodeWithAlphabetSlow :: (Integral a) => Text -> Text -> [a]
decodeWithAlphabetSlow alph sqid
  | Text.null sqid || not (Text.all (`Text.elem` alph) sqid) = []
  | otherwise = List.unfoldr decodeStep (slicedId, Text.reverse chars)
  where
    offset = unsafeIndex prefix alph
    (prefix, slicedId) = unsafeUncons sqid
    chars = Text.drop offset alph <> Text.take offset alph

decodeStep :: (Integral a) => (Text, Text) -> Maybe (a, (Text, Text))
decodeStep (sqid, alph)
  | Text.null sqid = Nothing
  | otherwise =
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
    offset = (increment + List.foldl' mu (length numbers) (zip numbers [0 ..])) `mod` len
    len = Text.length alph

    mu :: (Integral a, Num b) => b -> (a, b) -> b
    mu a (v, i) =
      let currentChar = Text.index alph (fromIntegral (v `mod` fromIntegral len))
       in fromIntegral (ord currentChar) + i + a
