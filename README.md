# [Sqids Haskell](https://sqids.org/haskell)

[![Github Actions](https://img.shields.io/github/actions/workflow/status/sqids/sqids-haskell/tests.yml)](https://github.com/sqids/sqids-haskell/actions)

Sqids (pronounced "squids") is a small library that lets you generate YouTube-looking IDs from numbers. It's good for link shortening, fast & URL-safe ID generation and decoding back into numbers for quicker database lookups.

## Getting started

Install Sqids via:

```haskell
# @todo
```

## Examples

Simple encode & decode:

```haskell
# @todo
```

Randomize IDs by providing a custom alphabet:

```haskell
# @todo
```

Enforce a *minimum* length for IDs:

```haskell
# @todo
```

Prevent specific words from appearing anywhere in the auto-generated IDs:

```haskell
# @todo
```

## Notes

- **Do not encode sensitive data.** These IDs can be easily decoded.
- **Default blacklist is auto-enabled.** It's configured for the most common profanity words. Create your own custom list by using the `blacklist` parameter, or pass an empty array to allow all words.
- Read more at <https://sqids.org/haskell>

## License

[MIT](LICENSE)