module Web.Sqids
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
  , encode_
  , decode_
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
  , sqidsEncode
  , sqidsDecode
  )

import Data.Text (Text)

encode :: (MonadSqids m) => [Int] -> m Text
encode = sqidsEncode

decode :: (MonadSqids m) => Text -> m [Int]
decode = sqidsDecode

encode_ :: (MonadSqids m) => [Integer] -> m Text
encode_ = sqidsEncode

decode_ :: (MonadSqids m) => Text -> m [Integer]
decode_ = sqidsDecode
