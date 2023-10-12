module Web.Sqids.Integer
  ( sqidsVersion
  , defaultSqidsOptions
  , SqidsOptions(..)
  , SqidsError(..)
  , MonadSqids
  , sqidsOptions
  , SqidsT
  , runSqidsT
  , sqidsT
  , Sqids
  , runSqids
  , sqids
  , encode
  , decode
  ) where

import Web.Sqids.Internal
  ( sqidsVersion
  , defaultSqidsOptions
  , SqidsOptions(..)
  , SqidsError(..)
  , MonadSqids
  , sqidsOptions
  , SqidsT
  , runSqidsT
  , sqidsT
  , Sqids
  , runSqids
  , sqids
  )

import Data.Text (Text)
import qualified Web.Sqids.Internal as Sqids

encode :: (MonadSqids m) => [Integer] -> m Text
encode = Sqids.encode

decode :: (MonadSqids m) => Text -> m [Integer]
decode = Sqids.decode
