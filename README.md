# Hex

Hex is a small package for working with `elm/bytes`.
It exposes two functions.

- `fromBytes : Bytes -> String`
- `toBytes : String -> Maybe Bytes`

The first function gives a hexadecimal representation of a Bytes, value, e.g.,
something like `"6A45F2"`. The second takes a string like the one
just given and returns a value of type `Maybe Bytes`. Such a function
call can fail, e.g., on an input `"6A45F!"`.

## Examples

`Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes == Just "FF66"`

```
$ elm repl
> import Hex exposing(..)
> import Bytes.Encode as Encode exposing(encode)

> encode (Encode.string "Hello") |> Hex.fromBytes
"48656C6C6F" : String

> Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes
Just "FF66" : Maybe String

> Hex.toBytes "FF66!!" |> Maybe.map Hex.fromBytes
Nothing : Maybe String
```
