# [Sqids Haskell](https://sqids.org/haskell)

[![Haskell CI](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml)

Sqids (pronounced "squids") is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

## Getting started

### Installation

Sqids is available on Hackage. Install it with:

```
cabal install sqids
```

or using [Stack](https://docs.haskellstack.org/en/stable/):

```
stack install sqids
```

- Hackage page: https://hackage.haskell.org/package/sqids

### Usage

```haskell
module Main where

import Web.Sqids

main :: IO ()
main =
  case sqids (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

```
"8QRLaD"
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Sqids

main :: IO ()
main =
  case sqids (decode "8QRLaD") of
    Left  {}   -> print "Something went wrong."
    Right nums -> print nums
```

```
[1,2,3]
```

```haskell
main =
  case runSqids defaultSqidsOptions{ minLength = 24 } (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

```
"75JILToVsGerOADWmT1cd0dL"
```

```haskell
main = do
  let options = SqidsOptions
        { alphabet  = "1234567890"
        , minLength = 8
        , blocklist = []
        }
  case runSqids options (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

```
"31764540"
```

#### Monad transformer

```haskell
module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Text (Text)
import Web.Sqids

main :: IO ()
main = do
  w <- execWriterT (sqidsT makeIds)
  print w

makeIds :: SqidsT (WriterT [Text] IO) ()
makeIds = do
  liftIO $ print "Generating IDs"
  forM_ [1 .. 50] $ \n -> do
    sqid <- encode [n, n, n, n]
    lift (tell [sqid])
```

```
["QkA3AmAC","fh9rtRtv","a7totm7V","KF5Z5l4X","ngqSq2b3","pjkCJlJf","yTrOSYSQ","HKVia9J2","0gTF2Zr3","jiw7wbw1","PtNNFWFA","I0vlvGvD","08TV2Sr5","UPLILMlD","ut2A2D20","Inv5vZvK","pDkBJTJJ","P1N8FRFr","R2eqeYeY","Ki5o5Q4U","1k70bzbD","dK4cE6Es","1L7XbJbZ","FyGjG1G0","ZEMReNre","aKtMte79","UtLNL9li","o6lElt2f","1w7ebtbl","nuqNqqbk","HlVSaOJ9","IKvdvave","3cWkDSD9","oQlzlc2C","RrezeDeC","OhJcJoVR","OEJFJzVJ","oplJlm2F","u8292F2H","FZGiGzGI","dN40E9EO","Q0AdAhAR","HJVzaaJC","s08YCUdX","sW8UCadW","ZaMNekrp","X4bsWS4Z","OoJIJEVj","Rqe1eTey","3aWYDXDs"]
```

@todo

## Types

### `SqidsOptions`

@todo

### `SqidsError`

@todo

### `SqidsT`

@todo

### `Sqids`

@todo

## Type classes

### `MonadSqids`

@todo

## Functions

### `encode`

@todo

### `decode`

@todo

### `sqidsVersion`

@todo

### `defaultSqidsOptions`

@todo

### `sqidsOptions`

@todo

### `runSqidsT`

@todo

### `sqidsT`

This is a short form for `runSqidsT defaultSqidsOptions`.

### `runSqids`

@todo

### `sqids`

This is a short form for `runSqids defaultSqidsOptions`.

## Examples

@todo

## License

[MIT](LICENSE)
