module Crockford exposing
    ( encode, decode, Error(..)
    , encodeWithChecksum, decodeWithChecksum
    )

{-| Encode integers as [Crockford-style base32 strings](https://www.crockford.com/base32.html).

From the specification:

> Base 32 is a textual 32-symbol notation for expressing numbers in a form that can be conveniently and accurately transmitted between humans and computer systems. It can be used for out of band communication of public keys.
>
> The encoding scheme is required to
>
>   - Be human readable and machine readable.
>   - Be compact. Humans have difficulty in manipulating long strings of arbitrary symbols.
>   - Be error resistant. Entering the symbols must not require keyboarding gymnastics.
>   - Be pronounceable. Humans should be able to accurately transmit the symbols to other humans using a telephone.

This package provides functions for encoding and decoding base32 data.


# Encoding & decoding

@docs encode, decode, Error


# Checksums

You can optionally insert a checksum at the end of the encoded data.

This allows validating the correctness of the string at a later point. For example, you may want to allow people to communicate the string over the phone, which obviously introduces a source of error. Crockford's base32 is optimized for communication, avoiding characters that look the same, but it's still possible for characters to be swapped or omitted during communication. By encoding with a checksum, and later decoding with a checksum again, you can validate that the string has not been modified and therefore corrupted.

@docs encodeWithChecksum, decodeWithChecksum

-}

import Crockford.Advanced as Advanced


{-| Encoding or decoding can fail in a couple of ways:

  - `NegativeNumberError` means you tried to encode a negative integer, which isn't supported.
  - `InvalidChecksum` means you tried to decode a base32 string with a checksum, but the checksum didn't match.
  - `InvalidCharacter` means you tried to decode a base32 string, but an invalid character was encountered.
  - `EmptyString` means you tried to decode an empty string.

-}
type Error
    = NegativeNumberError
    | InvalidChecksum
    | InvalidCharacter Char
    | EmptyString
    | NumberTooLarge Int


{-| Encode an integer as a base32 string.

    Crockford.encode 1337 --> "19S" : Result Crockford.Error String

-}
encode : Int -> Result Error String
encode x =
    Advanced.encode { checksum = False } x
        |> Result.mapError mapError


{-| Decode a base32 string to an integer.

    Crockford.decode "19S" --> 1337 : Result Crockford.Error Int

-}
decode : String -> Result Error Int
decode s =
    Advanced.decode { checksum = False } s
        |> Result.mapError mapError


{-| Like `encode`, but appends a checksum character to the end of the string.

    Crockford.encodeWithChecksum 32 --> Ok "10*" : Result Crockford.Error String

    Crockford.decodeWithChecksum "10*" --> Ok 32 : Result Crockford.Error Int

    Crockford.decodeWithChecksum "10~" --> Err InvalidChecksum : Result Crockford.Error Int

-}
encodeWithChecksum : Int -> Result Error String
encodeWithChecksum x =
    Advanced.encode { checksum = True } x
        |> Result.mapError mapError


{-| Like `decode`, but expects a checksum character at the end of the string.

See `encodeWithChecksum` for more information.

-}
decodeWithChecksum : String -> Result Error Int
decodeWithChecksum s =
    Advanced.decode { checksum = True } s
        |> Result.mapError mapError


mapError : Advanced.Error -> Error
mapError err =
    case err of
        Advanced.NegativeNumberError ->
            NegativeNumberError

        Advanced.InvalidChecksum ->
            InvalidChecksum

        Advanced.InvalidCharacter chr ->
            InvalidCharacter chr

        Advanced.EmptyString ->
            EmptyString

        Advanced.NumberTooLarge n ->
            NumberTooLarge n
