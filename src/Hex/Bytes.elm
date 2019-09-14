module Hex.Bytes exposing (fromBytes, toBytes, stringBlocks)

{-| The Hex package exposes three functions

  - fromBytes : Bytes -> String
  - toBytes : String -> Maybe Bytes
  - stringBlocks :

The first gives a hexadecimal representation of a Bytes, value, e.g.,
something like `"6A45F2"`. The second takes a string like the one
just given and returns a value of type `Maybe Bytes`. Such a function
call can fail, e.g., on an input `"6A45F!"`. The third is used
to "format" output into blocks.

@docs fromBytes, toBytes, stringBlocks

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), loop, map, succeed)
import Bytes.Encode as Encode exposing (encode)
import List.Extra


{-| Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes == Just "FF66"

> import Bytes.Encode as Encode exposing(encode)
> encode (Encode.string "Hello") |> Hex.fromBytes
> "48656C6C6F" : String

> Hex.toBytes "FF66!!" |> Maybe.map Hex.fromBytes
> Nothing : Maybe String

-}
fromBytes : Bytes -> String
fromBytes bytes_ =
    bytes_
        |> Decode.decode (decodeBytes (Bytes.width bytes_) Decode.unsignedInt8)
        |> Maybe.map (List.reverse >> List.map hexStringOfInt >> String.join "")
        |> Maybe.withDefault "Error"


{-| Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes == Just "FF66"
-}
toBytes : String -> Maybe Bytes
toBytes str =
    Maybe.map encode (toBytesEncoder str)


{-|

> "abcdefhij" |> stringBlocks 3
> ["abc","def","hij"]

-}
stringBlocks : Int -> String -> List String
stringBlocks blockSize str =
    str
        |> String.split ""
        |> List.Extra.groupsOf blockSize
        |> List.map (String.join "")



--
-- NOT EXPOSED
--


decodeBytes : Int -> Decoder a -> Decoder (List a)
decodeBytes len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder



{- String (Hex Representation) <-> Int conversions -}


stringOfHexDigit : Int -> String
stringOfHexDigit n =
    if n < 10 then
        String.fromInt n

    else
        case n of
            10 ->
                "A"

            11 ->
                "B"

            12 ->
                "C"

            13 ->
                "D"

            14 ->
                "E"

            15 ->
                "F"

            _ ->
                "X"


{-| hexStringOfInt 247 == "F7"
-}
hexStringOfInt : Int -> String
hexStringOfInt b =
    let
        hi =
            b // 16

        low =
            modBy 16 b
    in
    stringOfHexDigit hi ++ stringOfHexDigit low


intOfHexDigit : Char -> Maybe Int
intOfHexDigit str =
    case str of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'A' ->
            Just 10

        'a' ->
            Just 10

        'B' ->
            Just 11

        'b' ->
            Just 11

        'C' ->
            Just 12

        'c' ->
            Just 12

        'D' ->
            Just 13

        'd' ->
            Just 13

        'E' ->
            Just 14

        'e' ->
            Just 14

        'F' ->
            Just 15

        'f' ->
            Just 15

        _ ->
            Nothing


{-| intOfHexPair 'F' '7' == Just 247
intOfHexPair 'F' '7' |> Maybe.map hexStringOfInt
-}
intOfHexPair : Char -> Char -> Maybe Int
intOfHexPair highChar lowChar =
    let
        hi_ =
            intOfHexDigit highChar

        hi =
            Maybe.map (\x -> 16 * x) hi_

        lo =
            intOfHexDigit lowChar
    in
    Maybe.map2 (+) hi lo


byteListOfStringEncoder : List Encode.Encoder -> List Char -> Maybe (List Encode.Encoder)
byteListOfStringEncoder list charList =
    case charList of
        high :: low :: rest ->
            case intOfHexPair high low of
                Just value ->
                    byteListOfStringEncoder (Encode.unsignedInt8 value :: list) rest

                Nothing ->
                    Nothing

        [ _ ] ->
            Nothing

        [] ->
            List.reverse list |> Just


toBytesEncoder : String -> Maybe Encode.Encoder
toBytesEncoder str =
    String.toList str |> byteListOfStringEncoder [] |> Maybe.map Encode.sequence
