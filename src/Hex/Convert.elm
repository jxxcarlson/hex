module Hex.Convert exposing (toString, toBytes, blocks)

{-| The `Hex.Convert` package converts `Bytes` values to and from `String`
values.  Three functions are exposed:

  - `toString : Bytes -> String`
  - `toBytes : String -> Maybe Bytes`
  - `blocks : Int -> String -> List String`

The `toString` function converts a `Bytes` value to
a string of hexadecimal characters representing the `Bytes` value.
The `toBytes` function converts a string to a value of type `Maybe Bytes`.
This choice of return type is necessary to account for the fact
that a function call can fail, e.g., on the input `"6A45F!"`.
The `blocks` function is used to "format" the output
of `toString` into blocks of a given number of characters.

@docs toBytes, toString, blocks

-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), loop, map, succeed)
import Bytes.Encode as Encode exposing (encode)


{-|
Do `import Bytes.Encode as Encode exposing(encode)`.  Then

    encode (Encode.string "Hello")
        |> Hex.Convert.toString
    --> "48656C6C6F" : String

    Hex.Convert.toBytes "FF66!!"
        |> Maybe.map Hex.Convert.toString
    --> Nothing : Maybe String

-}
toString : Bytes -> String
toString bytes_ =
    bytes_
        |> Decode.decode (decodeBytes (Bytes.width bytes_) Decode.unsignedInt8)
        |> Maybe.map (List.reverse >> List.map hexStringOfInt >> String.join "")
        |> Maybe.withDefault "Error"


{-|
    Hex.Convert.toBytes "FF66"
        |> Maybe.map Hex.Convert.toString
    --> Just "FF66"
-}
toBytes : String -> Maybe Bytes
toBytes str =
    Maybe.map encode (toBytesEncoder str)


{-|  The `blocks` function is a general-purpose
string utility which divides a string into blocks
of characters, that is, a list of strings:

    "abcdefhij" |> Hex.Convert.blocks 3
    --> ["abc","def","hij"]

-}
blocks : Int -> String -> List String
blocks blockSize str =
    str
        |> String.split ""
        |> groupsOf blockSize
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


{-| Split list into groups of length `size`. If there are not enough elements
to completely fill the last group, it will not be included. This is equivalent
to calling `groupsOfWithStep` with the same `size` and `step`.

    groupsOf 3 (List.range 1 10)
    --> [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]

-}
groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


{-| Split list into groups of length `size` at offsets `step` apart. If there
are not enough elements to completely fill the last group, it will not be
included.

    groupsOfWithStep 4 4 (List.range 1 10)
    --> [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ]

    groupsOfWithStep 3 1 (List.range 1 5)
    --> [ [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4, 5 ] ]

    groupsOfWithStep 3 6 (List.range 1 20)
    --> [ [ 1, 2, 3 ], [ 7, 8, 9 ], [ 13, 14, 15 ] ]

If `step == size`, every element (except for perhaps the last few due to the
non-greedy behavior) will appear in exactly one group. If `step < size`, there
will be an overlap between groups. If `step > size`, some elements will be
skipped and not appear in any groups.

-}
groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    let
                        thisGroup =
                            List.take size xs
                    in
                    if size == List.length thisGroup then
                        let
                            rest =
                                List.drop step xs
                        in
                        go rest (thisGroup :: acc)

                    else
                        List.reverse acc
        in
        go list []
