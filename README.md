# [Sqids Haskell](https://sqids.org/haskell)

[![Haskell CI](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml)

Sqids (pronounced "squids") is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

## Getting started

### Installation

@todo

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

```haskell
main =
  case runSqids defaultSqidsOptions { minLength = 24 } (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
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
  w <- execWriterT (sqidsT makeids)
  print w

makeids :: SqidsT (WriterT [Text] IO) ()
makeids = do
  liftIO $ print "Generating IDs"
  forM_ [1 .. 50] $ \n -> do
    sqid <- encode [n, n, n, n]
    lift (tell [sqid])
```

@todo

## API

@todo

## Examples

@todo

## License

[MIT](LICENSE)
