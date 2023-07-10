-- | Sqids (pronounced "squids") is a small library that lets you generate
--   YouTube-looking IDs from numbers. It's good for link shortening, fast and
--   URL-safe ID generation and decoding back into numbers for quicker database
--   lookups.
--
-- For more info, see the official Sqids home page: <https://sqids.org>
module Web.Sqids
  ( sqidsVersion
  , defaultSqidsOptions
  , SqidsOptions(..)
  , SqidsError(..)
  , MonadSqids(..)
  , sqidsOptions
  , SqidsT
  , runSqidsT
  , sqidsT
  , Sqids
  , runSqids
  , sqids
  ) where

import Web.Sqids.Internal
