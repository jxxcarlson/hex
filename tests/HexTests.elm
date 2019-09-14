module HexTests exposing (..)

import Expect
import Fuzz
import Hex
import Test exposing (Test, describe, fuzz, fuzz2, test)


tests : Test
tests =
    describe "A Test Suite"
        [ fuzz hexFuzzer "Round trip test" <|
            \hex ->
                case Hex.toBytes hex of
                    Just bytes ->
                        Hex.fromBytes bytes |> String.toLower |> Expect.equal (String.toLower hex)

                    Nothing ->
                        Expect.fail "Failed to encode hex string."
        , fuzz2 hexFuzzer hexCharFuzzer "Hex.toBytes fails with odd number of chars" <|
            \hex extra ->
                String.cons extra hex |> Hex.toBytes |> Expect.equal Nothing
        , test "Round trip is stack safe" <|
            \_ ->
                let
                    longString =
                        String.repeat 1000000 "00"
                in
                longString |> Hex.toBytes |> Maybe.map Hex.fromBytes |> Expect.equal (Just longString)
        , test "Hex.toBytes fails on nonsense text." <|
            \_ ->
                Hex.toBytes "asdf" |> Expect.equal Nothing
        ]


hexCharFuzzer =
    [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'A', 'B', 'C', 'D', 'E', 'F' ] |> List.map Fuzz.constant |> Fuzz.oneOf


hexCharPairFuzzer =
    Fuzz.map2 (\a b -> String.fromList [ a, b ]) hexCharFuzzer hexCharFuzzer


hexFuzzer =
    Fuzz.list hexCharPairFuzzer |> Fuzz.map String.concat
