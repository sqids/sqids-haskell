# CHANGELOG

## 0.2.3.0

Internals rewritten around byte arrays and in-place mutation. No change to the
public API, and no change to any ID this library produces or accepts — the new
implementation was checked against 0.2.2.0 on a 31,512-case differential corpus
covering random alphabets, every `minLength` from 0 to 255, retry-forcing
blocklists, and arbitrary decode input, and reproduces it byte for byte.

### Performance

Measured on GHC 9.14.1, `-O2`, aarch64-darwin:

| | 0.2.2.0 | 0.2.3.0 | |
|---|---|---|---|
| `sqids (encode [n])` | 78.9 µs | 3.30 µs | 24× |
| `encode [n]`, context reused | 3.07 µs | 0.26 µs | 12× |
| `encode` 10 numbers | 165.6 µs | 3.27 µs | 51× |
| `encode [n]`, `minLength = 64` | 99.9 µs | 3.45 µs | 29× |
| `decode` a 20-byte ID | 51.7 µs | 3.34 µs | 16× |
| `decode` a 1,140-byte ID | 4,034 µs | 148 µs | 27× |
| `decode` a 21,820-byte ID | 246,903 µs | 2,566 µs | 96× |
| building a context | 75.0 µs | 5.00 µs | 15× |

What changed:

- `shuffle` now copies the alphabet once and swaps bytes in place, instead of
  rebuilding the whole alphabet for each of its `n-1` swaps. It was the hottest
  function in the library, running once per context, once per encoded number,
  once per 62 bytes of `minLength` padding, and once per number during decode.
- Character lookup goes through a 256-byte reverse index carried in the context,
  rather than an `O(alphabet)` scan per character.
- `decode` walks the ID once. Previously each step split the entire remaining ID
  apart and glued it back together, which made decoding **super-linear** in ID
  length: per-byte cost rose from 0.26 µs/byte on a short ID to 11.8 µs/byte on
  a 21.8 KB one. Since IDs usually arrive from untrusted input and `decode`
  accepts any length, that was a CPU-amplification hazard; cost is now linear.
- IDs are written once into an exactly-sized buffer instead of being built by
  repeated concatenation.
- The blocklist is partitioned at context-build time by how the spec matches
  each word, and bucketed on the only byte that can start or end a match. The
  prepared form of the default blocklist is shared, so default options cost
  nothing to set up.
- Encoding 20,000 IDs allocates 11.3 MB rather than 2.67 GB.

### Added

- `Web.Sqids.Internal.mkSqidsContext` — validate `SqidsOptions` and precompute,
  purely and totally, without running a Sqids computation. Prefer building one
  context and keeping it; that is where most of the remaining cost lives.
- `Web.Sqids.Internal.Core`, the byte-level core. Internal, no stability
  guarantees.

### Changed

- `runSqidsT`, `sqidsT`, `runSqids` and `sqids` have dropped their `Integral s`
  constraint, which is no longer needed now that building a context is pure.
  This widens the types they accept and matches the signatures the README has
  always documented; existing call sites are unaffected.
- `SqidsContext` has gained a `sqidsCore` field, and `sqidsBlocklist` is now
  lazy. The three original fields are unchanged, and `Eq`, `Ord` and `Show`
  still compare and render exactly those three.
- `Web.Sqids.Internal.shuffle`, `decodeWithAlphabet` and friends remain exported
  with their original types and results. They are compatibility shims now; the
  hot paths go straight to `Web.Sqids.Internal.Core`.

### Dependencies

- Added `primitive >= 0.8 && < 0.10`.
