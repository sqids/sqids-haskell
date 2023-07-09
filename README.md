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
        Left {} ->
            print "Something went wrong."
        Right sqid ->
            print sqid
```

@todo

## API

@todo

## Examples

@todo

## License

[MIT](LICENSE)
