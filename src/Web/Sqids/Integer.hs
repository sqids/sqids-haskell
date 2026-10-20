{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Sqids.Integer
  ( sqidsVersion
  , defaultSqidsOptions
  , SqidsOptions(..)
  , SqidsError(..)
  , MonadSqids
  , SqidsContext
  , mkSqidsContext
  , sqidsContext
  , SqidsT
  , runSqidsT
  , runSqidsTWith
  , sqidsT
  , Sqids
  , runSqids
  , runSqidsWith
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
  , SqidsContext
  , mkSqidsContext
  , sqidsContext
  , SqidsT
  , runSqidsT
  , runSqidsTWith
  , sqidsT
  , Sqids
  , runSqids
  , runSqidsWith
  , sqids
  , sqidsEncode
  , sqidsDecode
  )

import Data.Text (Text)

encode :: (MonadSqids Integer m) => [Integer] -> m Text
encode = sqidsEncode

decode :: (MonadSqids Integer m) => Text -> m [Integer]
decode = sqidsDecode
