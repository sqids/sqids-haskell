{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Sqids.Integer
  ( sqidsVersion
  , defaultSqidsOptions
  , SqidsOptions(..)
  , SqidsError(..)
  , MonadSqids
  , sqidsContext
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
  , sqidsContext
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

encode :: (MonadSqids Integer m) => [Integer] -> m Text
encode = sqidsEncode

decode :: (MonadSqids Integer m) => Text -> m [Integer]
decode = sqidsDecode
