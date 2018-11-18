module Hex exposing(decode, hexStringOfInt, intOfHexDigit, intOfHexPair)

{-|  The function call `hex someBytes` gives 
a hexadecimal view of the contents of 
`someBytes : Bytes`.

@docs decode
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..), loop,  map, succeed)
import Bytes.Encode as Encode exposing(encode)


{-| Encode.encode (Encode.string "Hello") |> Hex.decode == "48656C6C6F" 
-}
decode : Bytes -> String 
decode bytes_ = 
  bytes_  
    |> Decode.decode (decodeBytes (Bytes.width bytes_) Decode.unsignedInt8)
    |> Maybe.map (List.reverse >> (List.map hexStringOfInt) >> String.join "")
    |> Maybe.withDefault "Error"




stringOfHexDigit : Int -> String  
stringOfHexDigit n = 
  if n < 10 then 
    String.fromInt n 
  else 
  case n of 
    10 -> "A"
    11 -> "B"
    12 -> "C"
    13 -> "D"
    14 -> "E"
    15 -> "F"
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
  if List.member str ["0", "1", "2", "3", "4", "5", "6", "7", "9", "9"] then 
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
    

decodeBytes : Int -> Decoder a -> Decoder (List a)
decodeBytes len decoder = 
  loop (len, []) (listStep decoder)


listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done xs)
  else
    map (\x -> Loop (n - 1, x :: xs)) decoder