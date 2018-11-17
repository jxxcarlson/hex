module Hex exposing(hex, encodeString, decodeString)

{-|  The function call `hex someBytes` gives 
a hexadecimal view of the contents of 
`someBytes : Bytes`.

@docs hex, encodeString, decodeString
-}

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (..)
import Bytes.Encode as Encode exposing(encode)


{-| > encodeString "Hello" |> hex
Just "48656C6C6F" : Maybe String
-}
hex : Bytes -> Maybe  String 
hex bytes_ = 
  bytes_  
    |> decode (decodeBytes (Bytes.width bytes_) Decode.unsignedInt8)
    |> Maybe.map (List.reverse >> (List.map hexStringOfInt) >> String.join "")



{-| > encodeString "$20"
<3 bytes> : Bytes.Bytes
-}
encodeString : String -> Bytes 
encodeString  str = 
  encode (Encode.string str)


{-| > encodeString "$20$" |> decodeString
Just "$20$" : Maybe String
-}  
decodeString : Bytes -> Maybe String 
decodeString bytes = 
  let 
    n = Bytes.width bytes
  in
  decode (Decode.string n) bytes


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

hexStringOfInt : Int -> String
hexStringOfInt b =
  let 
    hi = b // 16
    low = modBy 16 b
 in 
   (stringOfHexDigit hi) ++ (stringOfHexDigit low)
  

{-| 
> abc = encodeString "abc"
<3 bytes> : Bytes
> abc 
       |> decode (decodeNBytes 3 D.unsignedInt8) 
       |> Maybe.map (List.reverse >> (List.map hexStringOfInt) >> String.join "")
Just "616263" : Maybe String
-}
decodeBytes : Int -> Decoder a -> Decoder (List a)
decodeBytes len decoder = 
  loop (len, []) (listStep decoder)


listStep : Decoder a -> (Int, List a) -> Decoder (Step (Int, List a) (List a))
listStep decoder (n, xs) =
  if n <= 0 then
    succeed (Done xs)
  else
    map (\x -> Loop (n - 1, x :: xs)) decoder