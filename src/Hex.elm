module Hex exposing(fromBytes, toBytes)

{-|  The Hex package exposes two functions

- fromBytes : Bytes -> String 
- toBytes : String -> Maybe Bytes

The first gives a hexadecimal representation of a Bytes, value, e.g.,
something like `"6A45F2"`.  The second takes a string like the one
just given and returns a value of type `Maybe Bytes`.  Such a function
call can fail, e.g., on an input `"6A45F!"`. 

@docs fromBytes, toBytes
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), loop,  map, succeed)
import Bytes.Encode as Encode exposing(encode)



{-| Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes == Just "FF66"

> import Bytes.Encode as Encode exposing(encode)
> encode (Encode.string "Hello") |> Hex.fromBytes
"48656C6C6F" : String

> Hex.toBytes "FF66!!" |> Maybe.map Hex.fromBytes
Nothing : Maybe String

-}
fromBytes : Bytes -> String 
fromBytes bytes_ = 
  bytes_  
    |> Decode.decode (decodeBytes (Bytes.width bytes_) Decode.unsignedInt8)
    |> Maybe.map (List.reverse >> (List.map hexStringOfInt) >> String.join "")
    |> Maybe.withDefault "Error"


{-| Hex.toBytes "FF66" |> Maybe.map Hex.fromBytes == Just "FF66"
-}
toBytes : String -> Maybe Bytes
toBytes str = 
  Maybe.map encode (toBytesEncoder str)


--
-- NOT EXPOSED 
--

decodeBytes : Int -> Decoder a -> Decoder (List a)
decodeBytes len decoder = 
  loop (len, []) (listStep decoder)


listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done xs)
  else
    map (\x -> Loop (n - 1, x :: xs)) decoder


{- String (Hex Representation) <-> Int conversions -}

stringOfHexDigit : Int -> String  
stringOfHexDigit n = 
  if n < 10 then 
    String.fromInt n
  else 
  case n of 
    10 ->  "A"
    11 ->  "B"
    12 ->  "C"
    13 ->  "D"
    14 ->  "E"
    15 ->  "F"
    _ -> "X" 


{-|  hexStringOfInt 247 == "F7"
-}
hexStringOfInt : Int -> String
hexStringOfInt b =
  let 
    hi = b // 16
    low = modBy 16 b
 in 
   (stringOfHexDigit hi) ++ (stringOfHexDigit low)
  
intOfHexDigit : String -> Maybe Int
intOfHexDigit str = 
  if List.member str ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"] then 
    String.toInt str  
  else 
    case str of 
       "A" -> Just 10
       "B" -> Just 11
       "C" -> Just 12
       "D" -> Just 13
       "E" -> Just 14
       "F" -> Just 15
       _ -> Nothing

{-| intOfHexPair "F7" == Just 247
intOfHexPair "F7" |> Maybe.map hexStringOfInt
-}
intOfHexPair : String -> Maybe Int  
intOfHexPair str = 
  let 
    hi_ = intOfHexDigit (String.left 1 str)
    hi = Maybe.map (\x -> 16*x) hi_
    lo = intOfHexDigit (String.right 1 str)
  in
  Maybe.map2 (+) hi lo


encodeByteOfHexPair : String -> Maybe Encode.Encoder
encodeByteOfHexPair str = 
  Maybe.map Encode.unsignedInt8 (intOfHexPair str)


stringPairListOfString : String -> List String   
stringPairListOfString str = 
  str 
    |> String.split "" 
    |> groupsOf 2 
    |> List.map (String.join "")

byteListOfStringEncoder : String ->  Maybe (List Encode.Encoder)
byteListOfStringEncoder str = 
  str 
    |> stringPairListOfString
    |> List.map encodeByteOfHexPair
    |> combine
   

toBytesEncoder : String ->  Maybe Encode.Encoder
toBytesEncoder str = 
   Maybe.map Encode.sequence (byteListOfStringEncoder str)


--
-- The following two functions are from List.Extra
--

groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
  groupsOfWithStep size size xs


groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step xs =
    let
        thisGroup =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayLength =
            size == List.length thisGroup
    in
    if okayArgs && okayLength then
        thisGroup :: groupsOfWithStep size step xs_

    else
    []


--
-- The following two functions are from Maybe.Extra
--

traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldr step (Just [])


{-| Take a list of `Maybe`s and return a `Maybe` with a list of values. `combine == traverse identity`.
    combine [] == Just []
    combine [ Just 1, Just 2, Just 3 ] == Just [ 1, 2, 3 ]
    combine [ Just 1, Nothing, Just 3 ] == Nothing
-}
combine : List (Maybe a) -> Maybe (List a)
combine =
  traverse identity