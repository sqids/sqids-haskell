<p align="center">
  <img alt="Logo" src="readme/sqids-haskell.svg" width="140" />
</p>

<a href="https://sqids.org/haskell">
  <h1 align="center">Sqids Haskell</h1>
</a>

<p align="center">
  <a href="https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml">
    <img alt="" src="https://github.com/sqids/sqids-haskell/actions/workflows/haskell.yml/badge.svg" />
  </a>
  <a href="https://opensource.org/licenses/MIT">
    <img alt="" src="https://img.shields.io/badge/license-MIT-yellow.svg" />
  </a>
  <a href="https://www.haskell.org">
    <img alt="" src="https://img.shields.io/badge/language-Haskell-orange.svg" />
  </a>
  <a href="https://hackage.haskell.org/package/sqids">
    <img alt="" src="https://img.shields.io/hackage/v/sqids.svg" />
  </a>
</p>

<p align="center">
  Sqids (<em>pronounced "squids"</em>) is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.
</p>

### Table of contents

* [Getting started](#-getting-started)
  * [Installation](#installation)
  * [Usage](#usage)
    * [Encoding](#encoding)
    * [Decoding](#decoding)
  * [Setting options](#setting-options)
  * [Monad transformer](#monad-transformer)
  * [Error handling](#error-handling)
* [Options](#options)
* [Errors](#errors)
* [Notes](#notes)
* [API documentation](#api-documentation)
* [License](#license)

## 🚀 Getting started

### Installation

Sqids is available on Hackage ([hackage.haskell.org/package/sqids](https://hackage.haskell.org/package/sqids)). To install it, run:

```
cabal install sqids
```

Or using [Stack](https://docs.haskellstack.org/en/stable/):

```
stack install sqids
```

### Usage

The library exposes two versions of the API;

- `Web.Sqids` relies on Haskell's `Int` data type, whereas
- `Web.Sqids.Integer` uses `Integer`s, which support arbitrarily large integers.

If you need to work with (i.e. encode and decode to) large numbers, it is
recommended to choose the latter option, in which case you would import the
library as:

```
import Web.Sqids.Integer
```

The Haskell standard (see [here](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#dx13-135009))
guarantees the range supported by `Int` to have an upper bound of at least
2<sup>29</sup> - 1 (= 536,870,911). If this does not present a problem for your
use case, instead use:

```
import Web.Sqids
```

Use `encode` to translate a list of non-negative integers into an ID, and
`decode` to retrieve back the list of numbers encoded by an ID.

```haskell
encode :: (Integral a) => [a] -> Sqids Text
decode:: (Integral a) => Text -> Sqids [a]
```

These functions return (monadic) values of type `Sqids Int a` or `Sqids Integer a`.
Calling `sqids`, which uses the default configuration, or `runSqids` (see below)
is the most straightforward way to extract the `something` from a `Sqids s something`
value.

```haskell
sqids :: Sqids s a -> Either SqidsError a
```

This gives you a value of type `Either SqidsError a`, where `a` is the ID in the
case of `encode`. If encoding fails for some reason, then the `Left` constructor
[contains the error](#error-handling). For some use cases, directly calling
`sqids` or `runSqids` in this way is sufficient. Doing so in multiple locations
in your code, however, doesn't scale very well, especially when IO or other
effects are involved. In this case, the [`SqidsT` monad transformer](#monad-transformer)
is a better choice.

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

> The output of this program is:
>
> ```
> "86Rf07"
> ```

#### Decoding

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Sqids

main :: IO ()
main =
  case sqids (decode "86Rf07") of
    Left  {}   -> print "Something went wrong."
    Right nums -> print nums
```

> The output of this program is:
>
> ```
> [1,2,3]
> ```

##### A note about the `OverloadedStrings` language extension

`decode` takes a `Text` value as input. If you are not compiling with
`OverloadedStrings` enabled, the `"86Rf07"` string literal in the previous
example would need to be explicitly converted, using the `pack` function from
`Data.Text`.

```haskell
import Data.Text (pack)
```

```haskell
decode (pack "86Rf07")
```

### Setting options

To pass custom options to `encode` and `decode`, use `runSqids` which takes
an additional `SqidsOptions` argument.

```haskell
runSqids :: SqidsOptions -> Sqids s a -> Either SqidsError a
```

See [here](#options) for available options. You can override the default values using
`defaultSqidsOptions`, and the following idiom:

```haskell
main =
  case runSqids defaultSqidsOptions{ minLength = 24 } (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

> The output of this program is:
>
> ```
> "86Rf07xd4zBmiJXQG6otHEbe"
> ```

To set a custom alphabet:

```haskell
main =
  case runSqids defaultSqidsOptions{ alphabet = "mTHivO7hx3RAbr1f586SwjNnK2lgpcUVuG09BCtekZdJ4DYFPaWoMLQEsXIqyz" } (encode [1, 2, 3]) of
    Left  {}   -> print "Something went wrong."
    Right sqid -> print sqid
```

> The output of this program is:
>
> ```
> "oz6E9F"
> ```

Or, you can set all options at once:

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

> The output of this program is:
>
> ```
> "38494176"
> ```

### Monad transformer

In a more realistically sized application, calling `runSqids` every time you
need to access the value returned by `encode` or `decode` isn't ideal. Instead,
you probably want to create your `SqidsOptions` once, and then do things with
the IDs across the code without having to pass the options object along every
time. Assuming your application relies on a transformer stack to combine effects
from different monads, then this implies adding the `SqidsT` transformer at
some suitable layer of the stack. Instead of `sqids` and `runSqids`, there are
two corresponding functions to fish out :fishing_pole_and_fish: the value from
inside of `SqidsT`:

```haskell
sqidsT :: Monad m => SqidsT s m a -> m (Either SqidsError a)
runSqidsT :: Monad m => SqidsOptions -> SqidsT s m a -> m (Either SqidsError a)
```

Below is an example where `SqidsT` is used in combination with the `Writer` and
`IO` monads.

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
  w <- sqidsT (execWriterT makeIds)
  case w of
    Left  err -> print ("Error: " <> show err)
    Right ids -> print ids

makeIds :: WriterT [Text] (SqidsT Int IO) ()
makeIds = do
  liftIO $ print "Generating IDs"
  forM_ [1 .. 50] $ \n -> do
    sqid <- encode [n, n, n, n]
    tell [sqid]
```

> The output of this program is:
>
> ```
> "Generating IDs"
> ["Q8Ac4uf3","fU9zWydl","aStUNEra","KR5zQbHB","n7qefHP0","pRkWlenI","ylr03cjE","H0V1tEjl","0rTYteaW","jQw6pcuZ","P9NfMbEk","IYvhBx6l","0vTGthaI","UXLhWExs","u52hY2FK","IjvHBv6e","pqk3lJnQ","PKNDMnEj","RJepNxTd","K15yQcHf","1c72LltW","dY4YwC0z","127FLStT","F0GBXRKm","ZDMTUa09","aFtHNir0","U4LiWBxu","oRltrlxW","1w7ULqtK","nYq5fnPa","HNVMtQjF","IRv4B26F","3wWEpjeF","oXlIrpxD","RNeTNnTN","OQJXLTbo","OAJwLube","onlgrbxt","u42vYoFH","FmGvXwKx","d84vwS0K","QuAl41fQ","H9VRtOjU","sh80jrCd","sB8CjqC3","ZKMzUJ0a","XkbEbTzD","OZJnL3bj","RGevNZTU","36WapueZ"]
> ```

### Error handling

Encoding and decoding can fail for various reasons.

```haskell
  case runSqids options (encode numbers) of
    Left SqidsNegativeNumberInInput ->
      print "Negative numbers are not allowed as input."
    _ ->
      -- etc...
```

See [here](#errors) for possible errors.

The following is an example of how to handle errors with the help of
`MonadError`s exception-handling mechanism:

```haskell
module Main where

import Control.Monad (when)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (fromRight)
import Data.Text (unpack)
import Text.Read (readMaybe)
import Web.Sqids

putStrLn_ :: String -> SqidsT Int IO ()
putStrLn_ = liftIO . putStrLn

repl :: SqidsT Int IO ()
repl = do
  input <- liftIO $ do
    putStrLn "Enter a comma-separated list of non-negative integers, or \"exit\"."
    putStr "> "
    getLine
  when (input /= "exit") $ do
    case readMaybe ("[" <> input <> "]") of
      Nothing ->
        putStrLn_ "Invalid input: Please try again."
      Just numbers ->
        catchError (encode numbers >>= putStrLn_ . unpack) $ \err ->
          case err of
            SqidsNegativeNumberInInput ->
              putStrLn_ "Only non-negative integers are accepted as input."
            _ ->
              putStrLn_ "Unexpected error"
    repl

runRepl :: IO (Either SqidsError ())
runRepl = runSqidsT defaultSqidsOptions repl

main :: IO ()
main = fromRight () <$> runRepl
```

> Program example output:
>
> ![Example](https://raw.githubusercontent.com/sqids/sqids-haskell/main/readme/example.gif)

## Options

### `alphabet :: Text`

The alphabet used by the algorithm for encoding and decoding.

* Default value: `abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789`

### `minLength :: Int`

The minimum allowed length of IDs.

* Default value: `0`

### `blocklist :: [Text]`

A list of words that must never appear in IDs.

* Default value: See [src/Web/Sqids/Blocklist.hs](src/Web/Sqids/Blocklist.hs).
* Also see [the official Sqids blocklist repository](https://github.com/sqids/sqids-blocklist).

## Errors

### `SqidsAlphabetTooShort`

The alphabet must be at least 3 characters long.

### `SqidsAlphabetRepeatedCharacters`

The provided alphabet contains duplicate characters. E.g., `"abcdefgg"` is not
a valid alphabet.

### `SqidsInvalidMinLength`

The given `minLength` value is not within the valid range.

### `SqidsNegativeNumberInInput`

One or more numbers in the list passed to `encode` are negative. Only
non-negative integers can be used as input.

### `SqidsMaxEncodingAttempts`

Encoding failed after too many recursive iterations. This happens if the
blocklist is too restrictive. Consider the following example:

```haskell
  let options = defaultSqidsOptions
        { alphabet  = "abc"
        , blocklist = [ "cab", "abc", "bca" ]
        , minLength = 3
        }
   in
     runSqids options (encode [0])
```

### `SqidsAlphabetContainsMultibyteCharacters`

The alphabet must consist of only characters in the standard single-byte
character set.

## Notes

- **Do not encode sensitive data.** These IDs can be easily decoded.
- **Default blocklist is auto-enabled.** It's configured for the most common profanity words. Create your own custom list by using the blocklist parameter, or pass an empty list to allow all words.
- Read more at https://sqids.org/haskell

## API documentation

See https://hackage.haskell.org/package/sqids.

## License

[MIT](LICENSE)
