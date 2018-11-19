# Hex

Hex is a small package for working with `elm/bytes`.
It exposes three functions.

- `fromBytes : Bytes -> String`
- `toBytes : String -> Maybe Bytes`
- `stringBlocks : Int -> String -> List String`

The first function gives a hexadecimal representation of a Bytes, value, e.g.,
something like `"6A45F2"`. The second takes a string like the one
just given and returns a value of type `Maybe Bytes`. Such a function
call can fail, e.g., on an input `"6A45F!"`. The third is useful
for viewing long strings as blocks of shorter strings.

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

> "abcdefghijklmnopqrstuvwx1234" |> Hex.stringBlocks 4
["abcd","efgh","ijkl","mnop","qrst","uvwx","1234"]
```

## Notes

The function `Hex.toBytes` is case-insensitive

```
 > Hex.toBytes "ff66" |> Maybe.map Hex.fromBytes
Just "FF66"
```

If you prefer lower-case output from `Hex.fromBytes`,
you can do as in this example:

```
> Hex.toBytes "ff66" |> Maybe.map Hex.fromBytes |> Maybe.map String.toLower
Just "ff66" : Maybe String
```
