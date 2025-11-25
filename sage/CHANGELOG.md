# Revision history for sage

## 0.5

* Fix use-after-free bug in `withByteString#`
* Rename `withByteString#` to `unsafeWithByteString#`

## 0.4

* `Parser` is now monomorphic in stream type
* Removed `Streaming.Chars`, Streaming.Chars.ByteString.Utf8`, and `Streaming.Chars.Text`

## 0.3

* Removed `parsers` instances (now in `sage-parsers-instances` package)

## 0.2

* Use `base >=4.16` (GHC 9.2)
* Use `text >=2.0`

## 0.1

* Initial release
