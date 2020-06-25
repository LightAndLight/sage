# sage

Efficient parser combinators for modern Haskell.

[https://github.com/LightAndLight/sage/tree/master/bench](Benchmarks) show Sage performing
8-9x faster than Megaparsec, 3-4x faster than Attoparsec. Memory usage is also improved -
Sage uses around 10% of the memory required by Megaparsec, and half that of Attoparsec.

Inspired by Ed Kmett's [https://github.com/ekmett/codex/tree/master/parsnip](parsnip) parser,
which for some reason I thought was called 'parsley'. (I don't care for root vegetable puns)
