# [Sqids Haskell](https://sqids.org/haskell)

[![Haskell CI](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml/badge.svg)](https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml)
[![License: MIT](https://img.shields.io/badge/license-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Language](https://img.shields.io/badge/language-Haskell-orange.svg)](https://www.haskell.org/)
[![Hackage](https://img.shields.io/hackage/v/sqids.svg)](https://hackage.haskell.org/package/sqids)

Sqids (pronounced "squids") is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

## 🏃Getting started

### Installation

Sqids is available on Hackage ([hackage.haskell.org/package/sqids](https://hackage.haskell.org/package/sqids)). To install this package, run:

```
cabal install sqids
```

Or using [Stack](https://docs.haskellstack.org/en/stable/):

```
stack install sqids
```

### Usage

```
encode :: [Int] -> Sqids Text
deocde :: Text -> Sqids [Int]

sqids :: Sqids a -> Either SqidsError a
```

#### Encoding

```haskell
module Main where

import Web.Sqids

main :: IO ()
main =
  case sqids (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

The output of this program is:

```
"8QRLaD"
```

#### Decoding

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

The output of this program is:

```
[1,2,3]
```

> Note that `decode` takes a `Text` value as input. Without the `OverloadedStrings` extension, the `"8QRLaD"` string literal in the above example would need to be explicitly converted, using the `pack` function from `Data.Text`.
>
> ```haskell
> import Data.Text (pack)
> ```
>
> ```haskell
> decode (pack "8QRLaD")
> ```

#### Setting options

```
runSqids :: SqidsOptions -> Sqids a -> Either SqidsError a
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

In a real-world application, you probably want to create your `SqidsOptions` 
once, and then be able to call `encode`/`decode` in various places across your 
code without passing this object around. If you're application relies on a 
custom monad stack, this is achieved by adding the `SqidsT` transformer at a
suitable layer of the stack. Below is an example where Sqids is used in
combination with the `Writer` and `IO` monads.

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

The output of this program is:

```
["QkA3AmAC","fh9rtRtv","a7totm7V","KF5Z5l4X","ngqSq2b3","pjkCJlJf","yTrOSYSQ","HKVia9J2","0gTF2Zr3","jiw7wbw1","PtNNFWFA","I0vlvGvD","08TV2Sr5","UPLILMlD","ut2A2D20","Inv5vZvK","pDkBJTJJ","P1N8FRFr","R2eqeYeY","Ki5o5Q4U","1k70bzbD","dK4cE6Es","1L7XbJbZ","FyGjG1G0","ZEMReNre","aKtMte79","UtLNL9li","o6lElt2f","1w7ebtbl","nuqNqqbk","HlVSaOJ9","IKvdvave","3cWkDSD9","oQlzlc2C","RrezeDeC","OhJcJoVR","OEJFJzVJ","oplJlm2F","u8292F2H","FZGiGzGI","dN40E9EO","Q0AdAhAR","HJVzaaJC","s08YCUdX","sW8UCadW","ZaMNekrp","X4bsWS4Z","OoJIJEVj","Rqe1eTey","3aWYDXDs"]
```

## ⚙️ Options

### `alphabet :: Text`

@todo

Default value:

```
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
```

### `minLength :: Int`

The minimum allowed length of IDs.

Default value: 0

### `blocklist :: [Text]`

A list of words that must never appear in IDs.

Default value: See __

## 💣 Errors

### `SqidsAlphabetTooShort`

The alphabet must be at least 5 characters long.

### `SqidsAlphabetRepeatedCharacters`

The provided alphabet contains duplicate characters. E.g., `"abcdefgg"` is not
a valid alphabet.

### `SqidsInvalidMinLength`

The given `minLength` value is not within the valid range.

### `SqidsNegativeNumberInInput`

One or more numbers in the list passed to `encode` is negative. Only
non-negative integers are permitted.

### `SqidsOutOfRangeError`

@todo

## Notes

- **Do not encode sensitive data.** These IDs can be easily decoded.
- **Default blocklist is auto-enabled.** It's configured for the most common profanity words. Create your own custom list by using the blocklist parameter, or pass an empty list to allow all words.
- Read more at https://sqids.org/haskell

## 📄 API documentation

See https://hackage.haskell.org/package/sqids

## Examples

Using a custom alphabet?

@todo

## License

[MIT](LICENSE)
