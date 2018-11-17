# Hex

Hex is a small package for working with `elm/bytes`.
It exposes three functions.

- `encodeString : String -> Bytes`
- `decodeString : Bytes -> Maybe String`
- `hex : Bytes -> Maybe String`

The main function is `hex`:

```
$ elm repl
> import Hex exposing(..)

> encodeString "Hello" |> hex
Just "48656C6C6F" : Maybe String
```

The `decodeString` function may also be useful for testing:

```
> encodeString "$20$" |> decodeString
Just "$20$" : Maybe String
```
