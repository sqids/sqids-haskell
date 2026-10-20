{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Web.Sqids
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

encode :: (MonadSqids Int m) => [Int] -> m Text
encode = sqidsEncode

decode :: (MonadSqids Int m) => Text -> m [Int]
decode = sqidsDecode
