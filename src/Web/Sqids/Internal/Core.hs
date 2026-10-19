{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The byte-level core of the encoder.
--
-- Sqids alphabets are validated to be ASCII-only, so every ID the library can
-- produce is ASCII too. That makes bytes the natural representation rather than
-- a compromise: the alphabet lives in a 'ByteArray', shuffling happens in place
-- on a 'MutableByteArray', character lookup is a 256-byte table, and IDs are
-- written once into an exactly-sized buffer.
--
-- Because 'Data.Text.Text' is UTF-8 backed by a @ByteArray#@ since @text-2.0@,
-- and ASCII is valid UTF-8, the resulting buffer can be handed out as a 'Text'
-- with no copy and no validation (see 'asciiByteArrayToText').
--
-- This module is internal and offers no stability guarantees.
module Web.Sqids.Internal.Core
  ( -- * Prepared state
    defaultAlphabet
  , Core (..)
  , Blocklist (..)
  , CoreError (..)
  , mkCore
  , coreFromShuffled
  , emptyBlocklist

    -- * Encoding and decoding
  , encodeCore
  , decodeCore

    -- * Primitives
  , shuffleBytes
  , shuffleSlice
  , isBlockedBytes

    -- * Conversions
  , asciiByteArrayToText
  , textBytes
  , asciiTextToByteArray
  , asciiShortToText
  ) where

import Control.Monad.ST (ST, runST)
import Data.Char (isAscii, ord, toLower)
import Data.Primitive.ByteArray
  ( ByteArray (..)
  , MutableByteArray
  , copyByteArray
  , copyMutableByteArray
  , indexByteArray
  , newByteArray
  , readByteArray
  , setByteArray
  , unsafeFreezeByteArray
  , writeByteArray
  )
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Internal (Text (..))
import Data.Word (Word8)

import qualified Data.ByteString.Short as Short
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Primitive.Array as Array
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray

import Web.Sqids.Blocklist (defaultBlocklist)

--------------------------------------------------------------------------------
-- Unboxed accessors
--------------------------------------------------------------------------------

{-# INLINE rd8 #-}
rd8 :: MutableByteArray s -> Int -> ST s Word8
rd8 = readByteArray

{-# INLINE wr8 #-}
wr8 :: MutableByteArray s -> Int -> Word8 -> ST s ()
wr8 = writeByteArray

{-# INLINE ix8 #-}
ix8 :: ByteArray -> Int -> Word8
ix8 = indexByteArray

-- | The alphabet 'Web.Sqids.defaultSqidsOptions' uses. Defined here so that
--   the prepared form of the default blocklist can be recognised and shared.
defaultAlphabet :: Text
defaultAlphabet =
  Text.pack "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

-- | Marks \"this byte is not in the alphabet\". An ASCII alphabet holds at most
--   128 distinct bytes, so no real position can collide with it.
absent :: Word8
absent = 0xff

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | /O(1)./ Reinterpret an ASCII 'ByteArray' as 'Text'. No copy, no validation.
--
--   The caller must guarantee the array holds @len@ bytes, all below @0x80@;
--   ASCII is valid UTF-8, which is what @text@ stores internally.
{-# INLINE asciiByteArrayToText #-}
asciiByteArrayToText :: ByteArray -> Int -> Text
asciiByteArrayToText (ByteArray ba) = Text (TextArray.ByteArray ba) 0

-- | /O(1)./ The UTF-8 bytes behind a 'Text', as @(array, offset, length)@.
{-# INLINE textBytes #-}
textBytes :: Text -> (ByteArray, Int, Int)
textBytes (Text (TextArray.ByteArray ba) off len) = (ByteArray ba, off, len)

-- | Copy a 'Text'\'s UTF-8 bytes into a fresh, unsliced 'ByteArray'. For ASCII
--   input (which callers must have checked for) one byte is one character.
asciiTextToByteArray :: Text -> ByteArray
asciiTextToByteArray t = runST $ do
  dst <- newByteArray len
  copyByteArray dst 0 src off len
  unsafeFreezeByteArray dst
  where
    (src, off, len) = textBytes t

asciiTextToShort :: Text -> Short.ShortByteString
asciiTextToShort = Short.toShort . encodeUtf8

-- | Inverse of 'asciiTextToShort'. Only used by compatibility views.
asciiShortToText :: Short.ShortByteString -> Text
asciiShortToText = decodeUtf8 . Short.fromShort

--------------------------------------------------------------------------------
-- Prepared state
--------------------------------------------------------------------------------

-- | Everything derivable from 'Web.Sqids.Internal.SqidsOptions', computed once.
data Core = Core
  { coreAlphabet :: !ByteArray
    -- ^ The shuffled alphabet, @coreLen@ bytes, all ASCII.
  , coreIndex    :: !ByteArray
    -- ^ 256 bytes mapping a byte to its position in 'coreAlphabet', or
    --   'absent'. Turns character lookup from an /O(n)/ scan into one load.
  , coreLen      :: !Int
  , coreMinLen   :: !Int
  , coreBlocklist :: Blocklist
    -- ^ Deliberately lazy: a context that only ever decodes never pays to
    --   prepare the blocklist.
  }

-- | The blocklist, partitioned by how the spec matches each word and bucketed
--   on the only byte position that can start (or end) a match.
--
--   The partition is exact, not an approximation. The spec compares a word to
--   the whole ID for equality when either is three characters or shorter, so a
--   word of length @<= 3@ can never match an ID longer than that, and words
--   longer than three characters split statically on whether they contain a
--   digit.
data Blocklist = Blocklist
  { blAny    :: !Bool
    -- ^ 'False' short-circuits every check.
  , blKept   :: [Short.ShortByteString]
    -- ^ Every word that survived filtering, in input order. Only here so that
    --   @sqidsBlocklist@ can be presented without redoing the filtering.
  , blShort  :: ![Short.ShortByteString]
    -- ^ Length @<= 3@; consulted only for IDs of length @<= 3@, exact match.
  , blPrefix :: !(Array.Array [Short.ShortByteString])
    -- ^ Digit-bearing words, bucketed on first byte (prefix match).
  , blSuffix :: !(Array.Array [Short.ShortByteString])
    -- ^ Digit-bearing words, bucketed on last byte (suffix match).
  , blInfix  :: !(Array.Array [Short.ShortByteString])
    -- ^ Digit-free words, bucketed on first byte (infix match).
  }

emptyBlocklist :: Blocklist
emptyBlocklist = Blocklist
  { blAny    = False
  , blKept   = []
  , blShort  = []
  , blPrefix = noBuckets
  , blSuffix = noBuckets
  , blInfix  = noBuckets
  }
  where
    noBuckets = Array.arrayFromListN 256 (replicate 256 [])

-- | Why an alphabet or minimum length was rejected. Mapped onto
--   @SqidsError@ by "Web.Sqids.Internal", which owns the public error type.
data CoreError
  = CoreMultibyteAlphabet
  | CoreAlphabetTooShort
  | CoreRepeatedCharacters
  | CoreInvalidMinLength
  deriving (Show, Eq)

-- | Validate options and precompute everything derived from them. Failures are
--   reported in the same order as the original implementation, which the test
--   suite pins.
mkCore
  :: Text    -- ^ alphabet, as supplied (unshuffled)
  -> Int     -- ^ minimum length
  -> [Text]  -- ^ blocklist, as supplied
  -> Either CoreError Core
mkCore alph minLen bls
  | Text.any (> '\DEL') alph = Left CoreMultibyteAlphabet
  | n < 3 = Left CoreAlphabetTooShort
  | otherwise = case buildIndexSlice src aoff n of
      Nothing -> Left CoreRepeatedCharacters
      Just _
        | minLen < 0 || minLen > 255 -> Left CoreInvalidMinLength
        | otherwise -> case buildIndex shuffled n of
            Nothing -> Left CoreRepeatedCharacters  -- unreachable: a permutation
            Just idx -> Right Core
              { coreAlphabet  = shuffled
              , coreIndex     = idx
              , coreLen       = n
              , coreMinLen    = minLen
              , coreBlocklist = mkBlocklist alph bls
              }
  where
    (src, aoff, n) = textBytes alph
    shuffled = shuffleSlice src aoff n

-- | Prepare from an alphabet that has /already/ been shuffled, with no
--   blocklist. Used by the compatibility shims that take a bare alphabet.
coreFromShuffled :: Text -> Maybe Core
coreFromShuffled alph
  | Text.any (> '\DEL') alph = Nothing
  | n < 3 = Nothing
  | otherwise = do
      idx <- buildIndexSlice src aoff n
      pure Core
        { coreAlphabet  = unsliced
        , coreIndex     = idx
        , coreLen       = n
        , coreMinLen    = 0
        , coreBlocklist = emptyBlocklist
        }
  where
    (src, aoff, n) = textBytes alph
    unsliced
      | aoff == 0 = src
      | otherwise = asciiTextToByteArray alph

-- | @Nothing@ when the alphabet contains a repeated byte.
buildIndex :: ByteArray -> Int -> Maybe ByteArray
buildIndex alph = buildIndexSlice alph 0

buildIndexSlice :: ByteArray -> Int -> Int -> Maybe ByteArray
buildIndexSlice alph off n = runST $ do
  tbl <- newByteArray 256
  setByteArray tbl 0 256 absent
  let go !i
        | i >= n = Just <$> unsafeFreezeByteArray tbl
        | otherwise = do
            let b = fromIntegral (ix8 alph (off + i))
            prev <- rd8 tbl b
            if prev /= absent
              then pure Nothing
              else wr8 tbl b (fromIntegral i) >> go (i + 1)
  go 0

--------------------------------------------------------------------------------
-- shuffle
--------------------------------------------------------------------------------

-- | The spec's deterministic shuffle: @n-1@ swaps. One allocation, then every
--   swap is two byte writes on the same buffer.
shuffleBytes :: ByteArray -> Int -> ByteArray
shuffleBytes src = shuffleSlice src 0

-- | 'shuffleBytes' over a slice, so the source can be a 'Text'\'s backing
--   array and the copy-in doubles as the only allocation.
shuffleSlice :: ByteArray -> Int -> Int -> ByteArray
shuffleSlice src off n = runST $ do
  buf <- newByteArray n
  copyByteArray buf 0 src off n
  shuffleMut buf n
  unsafeFreezeByteArray buf

{-# INLINE shuffleMut #-}
shuffleMut :: MutableByteArray s -> Int -> ST s ()
shuffleMut buf n = go 0
  where
    go !i
      | i > n - 2 = pure ()
      | otherwise = do
          let j = n - i - 1
          ci <- rd8 buf i
          cj <- rd8 buf j
          let r = (i * j + fromIntegral ci + fromIntegral cj) `rem` n
          cr <- rd8 buf r
          wr8 buf i cr
          wr8 buf r ci
          go (i + 1)

--------------------------------------------------------------------------------
-- blocklist
--------------------------------------------------------------------------------

-- | Lowercase, drop words shorter than three characters, and drop words using
--   characters outside the alphabet — then partition and bucket.
--
--   Lowercasing goes through 'toLower' on 'Char' rather than a byte-wise
--   @+0x20@, because a handful of non-ASCII code points lowercase /into/ ASCII
--   (U+0130 and U+212A) and the original implementation keeps those words.
mkBlocklist :: Text -> [Text] -> Blocklist
mkBlocklist alph ws
  -- Preparing the 560-word default blocklist is the single most expensive part
  -- of building a context, and it is the same answer every time. Recognise it
  -- and share one prepared copy.
  | alph == defaultAlphabet && ws == defaultBlocklist = preparedDefaultBlocklist
  | otherwise = buildBlocklist alph ws

-- | The prepared default blocklist, under the default alphabet.
preparedDefaultBlocklist :: Blocklist
preparedDefaultBlocklist = buildBlocklist defaultAlphabet defaultBlocklist
{-# NOINLINE preparedDefaultBlocklist #-}

buildBlocklist :: Text -> [Text] -> Blocklist
buildBlocklist alph ws
  | null kept = emptyBlocklist
  | otherwise = Blocklist
      { blAny    = True
      , blKept   = kept
      , blShort  = [w | w <- kept, Short.length w <= 3]
      , blPrefix = bucketOn firstByte digitWords
      , blSuffix = bucketOn lastByte  digitWords
      , blInfix  = bucketOn firstByte plainWords
      }
  where
    allowed = allowedTable (Text.map toLower alph)
    admits c = isAscii c && ix8 allowed (ord c) /= absent

    kept =
      [ asciiTextToShort lw
      | w <- ws
      , let lw = Text.map toLower w
      , Text.length lw >= 3
      , Text.all admits lw
      ]

    longWords  = [w | w <- kept, Short.length w > 3]
    digitWords = [w | w <- longWords, hasDigit w]
    plainWords = [w | w <- longWords, not (hasDigit w)]

    firstByte w = Short.index w 0
    lastByte  w = Short.index w (Short.length w - 1)

    hasDigit w = go 0
      where
        m = Short.length w
        go !i
          | i >= m = False
          | otherwise =
              let c = Short.index w i
               in (c >= 0x30 && c <= 0x39) || go (i + 1)

    bucketOn key xs =
      Array.arrayFromListN 256 [IntMap.findWithDefault [] b byKey | b <- [0 .. 255]]
      where
        byKey = IntMap.fromListWith (++) [(fromIntegral (key w), [w]) | w <- xs]

-- | 256-byte membership table for an ASCII character set.
allowedTable :: Text -> ByteArray
allowedTable t = runST $ do
  tbl <- newByteArray 256
  setByteArray tbl 0 256 absent
  let go [] = pure ()
      go (c : cs)
        | isAscii c = wr8 tbl (ord c) 1 >> go cs
        | otherwise = go cs
  go (Text.unpack t)
  unsafeFreezeByteArray tbl

-- | Is this ID blocked? Reads the ID bytes directly and lowercases during
--   comparison, so nothing is allocated.
isBlockedBytes :: Blocklist -> ByteArray -> Int -> Int -> Bool
isBlockedBytes Blocklist{..} arr off n
  | not blAny = False
  | n == 0 = False
  | n <= 3 = any exact blShort
  | otherwise = hitPrefix || hitSuffix || scan 0
  where
    -- lowercased byte at position p of the ID
    lo !p =
      let c = ix8 arr (off + p)
       in if c >= 0x41 && c <= 0x5a then c + 32 else c

    -- does word w occur at position p?
    matchAt !p w = go 0
      where
        m = Short.length w
        go !q
          | q >= m = True
          | p + q >= n = False
          | Short.index w q /= lo (p + q) = False
          | otherwise = go (q + 1)

    exact w = Short.length w == n && matchAt 0 w

    hitPrefix = any (matchAt 0) (bucket blPrefix (lo 0))
    hitSuffix =
      any (\w -> let m = Short.length w in m <= n && matchAt (n - m) w)
          (bucket blSuffix (lo (n - 1)))

    scan !p
      | p >= n = False
      | otherwise = any (matchAt p) (bucket blInfix (lo p)) || scan (p + 1)

    bucket buckets b = Array.indexArray buckets (fromIntegral b)

--------------------------------------------------------------------------------
-- encode
--------------------------------------------------------------------------------

-- | Encode a non-empty list of non-negative numbers, retrying with a larger
--   increment while the result is blocked.
{-# INLINABLE encodeCore #-}
encodeCore
  :: (Integral a)
  => Core
  -> [a]
  -> Int   -- ^ starting increment
  -> Int   -- ^ highest increment to try
  -> Either Int (ByteArray, Int)
encodeCore core ns start maxAttempts = go start
  where
    k = length ns
    go !increment
      | increment > maxAttempts = Left increment
      | otherwise =
          let (out, len) = buildId core ns k increment
           in if isBlockedBytes (coreBlocklist core) out 0 len
                then go (increment + 1)
                else Right (out, len)

-- | Write one candidate ID. The working alphabet is a single mutable buffer
--   that is rotated, reversed and then shuffled in place; the output length is
--   known exactly up front, so the result is one allocation filled once.
{-# INLINABLE buildId #-}
buildId :: (Integral a) => Core -> [a] -> Int -> Int -> (ByteArray, Int)
buildId Core{..} ns k increment = runST $ do
  buf <- newByteArray coreLen
  loadRotatedReversed buf coreAlphabet coreLen offset
  out <- newByteArray outLen
  wr8 out 0 (ix8 coreAlphabet offset)
  end <- writeNumbers out buf 1 ns
  if coreMinLen > end
    then do
      sep <- rd8 buf 0
      wr8 out end sep
      pad out buf (end + 1)
    else pure ()
  frozen <- unsafeFreezeByteArray out
  pure (frozen, outLen)
  where
    -- 'toId' works against the alphabet minus its leading separator.
    base = fromIntegral (coreLen - 1)

    outLen = max (1 + sum (map (digitCount base) ns) + (k - 1)) coreMinLen

    offset = (increment + k + offsetSum) `rem` coreLen
    offsetSum = acc 0 0 ns
      where
        acc !s _ [] = s
        acc !s !i (v : vs) =
          let c = ix8 coreAlphabet (fromIntegral (v `rem` fromIntegral coreLen))
           in acc (s + fromIntegral c + i) (i + 1) vs

    writeNumbers out buf = loop
      where
        loop !pos [] = pure pos
        loop !pos (v : vs) = do
          let d = digitCount base v
              digits !i !m
                | i < 0 = pure ()
                | otherwise = do
                    let (q, r) = m `quotRem` base
                    c <- rd8 buf (fromIntegral r + 1)
                    wr8 out (pos + i) c
                    digits (i - 1) q
          digits (d - 1) v
          case vs of
            [] -> pure (pos + d)
            _ -> do
              sep <- rd8 buf 0
              wr8 out (pos + d) sep
              shuffleMut buf coreLen
              loop (pos + d + 1) vs

    pad out buf = go
      where
        go !pos
          | coreMinLen - pos <= 0 = pure ()
          | otherwise = do
              shuffleMut buf coreLen
              let m = min (coreMinLen - pos) coreLen
              copyMutableByteArray out pos buf 0 m
              go (pos + m)

-- | @buf := reverse (rotate off alphabet)@
{-# INLINE loadRotatedReversed #-}
loadRotatedReversed :: MutableByteArray s -> ByteArray -> Int -> Int -> ST s ()
loadRotatedReversed buf alph n off = go 0
  where
    go !i
      | i >= n = pure ()
      | otherwise = wr8 buf i (ix8 alph ((off + n - 1 - i) `rem` n)) >> go (i + 1)

{-# INLINE digitCount #-}
digitCount :: (Integral a) => a -> a -> Int
digitCount b = go 1
  where
    go !acc n = let m = n `quot` b in if m == 0 then acc else go (acc + 1) m

--------------------------------------------------------------------------------
-- decode
--------------------------------------------------------------------------------

-- | Decode an ID read straight out of a caller-supplied buffer.
--
--   Finding each separator is a bounded scan and advancing past it is an index
--   bump, so the whole walk is linear in the ID length — the original split the
--   remaining ID apart and glued it back together on every step.
decodeCore :: (Integral a) => Core -> ByteArray -> Int -> Int -> [a]
decodeCore Core{..} src off len
  | len == 0 = []
  | anyUnknown 0 = []
  -- Nothing after the prefix byte: no numbers were encoded.
  | len == 1 = []
  | otherwise =
      -- The first chunk needs no lookup table. At that point the working
      -- alphabet is reverse (rotate offset alphabet), so a byte's position in
      -- it is a closed form over the index we already have -- which makes
      -- decoding a single-number ID completely allocation-free.
      case findSep sep0 1 of
        -1 -> [numberAt 1 len]
        1  -> []
        q  -> numberAt 1 q : runST (do
                buf <- newByteArray coreLen
                tbl <- newByteArray 256
                loadRotatedReversed buf coreAlphabet coreLen offset
                shuffleMut buf coreLen
                setByteArray tbl 0 256 absent
                reindex buf tbl
                loop buf tbl (q + 1) [])
  where
    base = fromIntegral (coreLen - 1)

    {-# INLINE byteAt #-}
    byteAt !i = ix8 src (off + i)

    anyUnknown !i
      | i >= len = False
      | ix8 coreIndex (fromIntegral (byteAt i)) == absent = True
      | otherwise = anyUnknown (i + 1)

    offset = fromIntegral (ix8 coreIndex (fromIntegral (byteAt 0)))

    -- buf[0], without materialising buf
    sep0 = ix8 coreAlphabet ((offset + coreLen - 1) `rem` coreLen)

    -- position of byte c in reverse (rotate offset alphabet)
    {-# INLINE posOf #-}
    posOf c =
      (offset + coreLen - 1 - fromIntegral (ix8 coreIndex (fromIntegral c)))
        `rem` coreLen

    findSep sep !q
      | q >= len = -1
      | byteAt q == sep = q
      | otherwise = findSep sep (q + 1)

    -- first chunk: positions computed, not looked up
    numberAt !from !to = go 0 from
      where
        go !acc !i
          | i >= to = acc
          | otherwise = go (base * acc + fromIntegral (posOf (byteAt i) - 1)) (i + 1)

    reindex buf tbl = go 0
      where
        go !i
          | i >= coreLen = pure ()
          | otherwise = do
              c <- rd8 buf i
              wr8 tbl (fromIntegral c) (fromIntegral i)
              go (i + 1)

    -- subsequent chunks: the alphabet has been shuffled, so look positions up
    toNumber tbl !from !to = go 0 from
      where
        go !acc !i
          | i >= to = pure acc
          | otherwise = do
              p <- rd8 tbl (fromIntegral (byteAt i))
              -- positions in the alphabet *without* its leading separator
              go (base * acc + fromIntegral (fromIntegral p - 1 :: Int)) (i + 1)

    loop buf tbl !p acc
      | p >= len = pure (reverse acc)
      | otherwise = do
          sep <- rd8 buf 0
          case findSep sep p of
            -1 -> do
              v <- toNumber tbl p len
              pure (reverse (v : acc))
            q | q == p -> pure (reverse acc)
              | otherwise -> do
                  v <- toNumber tbl p q
                  shuffleMut buf coreLen
                  reindex buf tbl
                  loop buf tbl (q + 1) (v : acc)

--------------------------------------------------------------------------------
-- Specialisations
--
-- The digit loops are `quotRem`-bound, so the difference between a machine
-- instruction and a dictionary call is most of the remaining cost. These cover
-- the two types the library exposes.
--------------------------------------------------------------------------------

{-# SPECIALIZE decodeCore :: Core -> ByteArray -> Int -> Int -> [Int] #-}
{-# SPECIALIZE decodeCore :: Core -> ByteArray -> Int -> Int -> [Integer] #-}
{-# SPECIALIZE encodeCore :: Core -> [Int] -> Int -> Int -> Either Int (ByteArray, Int) #-}
{-# SPECIALIZE encodeCore :: Core -> [Integer] -> Int -> Int -> Either Int (ByteArray, Int) #-}
{-# SPECIALIZE buildId :: Core -> [Int] -> Int -> Int -> (ByteArray, Int) #-}
{-# SPECIALIZE buildId :: Core -> [Integer] -> Int -> Int -> (ByteArray, Int) #-}
