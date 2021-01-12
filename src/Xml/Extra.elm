----------------------------------------------------------------------
--
-- Xml/Extra.elm
-- Simplify creating Decoders for XML input.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Xml.Extra exposing
    ( TagSpec, Required(..)
    , requiredTag, optionalTag, multipleTag
    , decodeXml
    , Error(..), DecodeDetails
    , tagDecoder, stringToJson, xmlToJson
    )

{-| Simplify creating Decoders for XML input.

Most code will only need the two types, the `decodeXml` function, and the `optionalTag` & `multipleTag` decoders.

Example:

    import XML.Extra exposing ( TagSpec, Required(..)
                              , decodeXml, optionalTag
                              )
    import Json.Decode as JD exposing ( Decoder )

    type alias PersonRecord =
        { name : String
        , age : Int
        , spouse : Maybe Person
        , children : List Person
        }

    type Person =
        Person PersonRecord

    personDecoder : Decoder Person
    personDecoder =
        JD.map4 (\name age spouse children ->
                     Person <| PersonRecord name age spouse children
            (JD.field "name" JD.string)
            (JD.field "age" JD.int)
            (optionalTag "spouse"
                 (JD.lazy (\_ -> personDecoder))
                 personTagSpecs
            )
            (multipleTag "child"
                 (JD.lazy (\_ -> personDecoder))
                 personTagSpecs)

    personTagSpecs : List TagSpec
    personTagSpecs =
        [ ("name", Required)
        , ("age", Required)
        , ("spouse", Optional)
        , ("child", Multiple)
        ]

    decodePersonXml : String -> Result String Person
    decodePersonXml xml =
        decodeXml xml "person" personDecoder personTagSpecs

    Xml =
        """
    <person>
      <name>Irving</name>
      <age max="100">30</age>
      <sex>yes</sex>
      <favoriteColor>blue</favoriteColor>
      <spouse>
          <name>Joan</name>
          <age>28</age>
      </spouse>
      <child>
          <name>Bob</name>
          <age>1</age>
      </child>
      <child>
          <name>Sally</name>
          <age>3</age>
      </child>
    </person>
        """


# Types

@docs TagSpec, Required


# Decoders

@docs requiredTag, optionalTag, multipleTag


# Functions

@docs decodeXml


# Errors

@docs Error, DecodeDetails


# Low-level decoder and functions

@docs tagDecoder, stringToJson, xmlToJson

-}

import Debug exposing (log)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import Xml
import Xml.Decode as XD


{-| Details about a decode error.

`value` is the input to the decoder.
`msg` is the error from Json.Decode.decodeValue with the current value appended.

-}
type alias DecodeDetails =
    { value : JD.Value
    , msg : String
    }


{-| The error return from decodeXml
-}
type Error
    = XmlError String
    | DecodeError DecodeDetails


{-| Decode an XML string containing a single tag into an Elm value.
-}
decodeXml : String -> String -> Decoder value -> List TagSpec -> Result Error value
decodeXml xml tag valueDecoder tagSpecs =
    case stringToJson xml of
        Err msg ->
            Err <| XmlError msg

        Ok value ->
            case
                JD.decodeValue (JD.list JD.value) value
            of
                Ok list ->
                    case list of
                        [ a ] ->
                            case
                                JD.decodeValue
                                    (JD.field tag <|
                                        tagDecoder valueDecoder tagSpecs
                                    )
                                    a
                            of
                                Ok val ->
                                    Ok val

                                Err msg ->
                                    Err <|
                                        DecodeError
                                            { value = value
                                            , msg = JD.errorToString msg
                                            }

                        _ ->
                            Err <|
                                DecodeError
                                    { value = value
                                    , msg = "Xml did not contain a single top-level tag."
                                    }

                Err msg ->
                    Err <|
                        DecodeError
                            { value = value
                            , msg = JD.errorToString msg
                            }


optionalTagCallback : Decoder value -> List TagSpec -> Maybe Value -> Decoder (Maybe value)
optionalTagCallback valueDecoder tagSpecs value =
    case value of
        Nothing ->
            JD.succeed Nothing

        Just v ->
            let
                decoder =
                    if tagSpecs == [] then
                        valueDecoder

                    else
                        tagDecoder valueDecoder tagSpecs
            in
            case
                JD.decodeValue decoder v
            of
                Ok val ->
                    JD.succeed <| Just val

                Err msg ->
                    JD.fail <| JD.errorToString msg


{-| A decoder for `Optional` XML tags

If the `TagSpec` list is empty, then the `Decoder` is for a simple value. Otherwise, it's for a nested tag.

-}
optionalTag : String -> Decoder value -> List TagSpec -> Decoder (Maybe value)
optionalTag tag valueDecoder tagSpecs =
    JD.oneOf
        [ JD.field tag (JD.nullable JD.value)
        , JD.succeed Nothing
        ]
        |> JD.andThen (optionalTagCallback valueDecoder tagSpecs)


{-| A decoder for `Required` XML tags

If the `TagSpec` list is empty, then the `Decoder` is for a simple value. Otherwise, it's for a nested tag.

-}
requiredTag : String -> Decoder value -> List TagSpec -> Decoder value
requiredTag tag valueDecoder tagSpecs =
    let
        decoder =
            if tagSpecs == [] then
                valueDecoder

            else
                tagDecoder valueDecoder tagSpecs
    in
    JD.field tag decoder


{-| A decoder for `Multiple` XML tags

If the `TagSpec` list is empty, then the `Decoder` is for a simple value. Otherwise, it's for a nested tag.

-}
multipleTag : String -> Decoder value -> List TagSpec -> Decoder (List value)
multipleTag tag valueDecoder tagSpecs =
    let
        decoder =
            if tagSpecs == [] then
                valueDecoder

            else
                tagDecoder valueDecoder tagSpecs
    in
    JD.field tag <| JD.list decoder


{-| Decode an XML string into a simplified `Json.Encode.Value`.
-}
stringToJson : String -> Result String Value
stringToJson string =
    case XD.decode string of
        Ok val ->
            Ok <| xmlToJson val

        Err msg ->
            Err msg


{-| Convert the `Xml.Value` returned by `Xml.Decode.decode` to a `Json.Encode.Value`,
removing all the attributes.
-}
xmlToJson : Xml.Value -> Value
xmlToJson xml =
    let
        value =
            Xml.xmlToJson xml
    in
    decodeXmlValue value


removeLeadingNullValue : List Value -> Decoder Value
removeLeadingNullValue list =
    case list of
        a :: rest ->
            if a == JE.null then
                JD.succeed (JE.list identity rest)

            else
                JD.succeed (JE.list identity list)

        _ ->
            JD.succeed <| JE.list identity list


removeLeadingNullDecoder : Decoder Value
removeLeadingNullDecoder =
    JD.list JD.value
        |> JD.andThen removeLeadingNullValue


removeLeadingNull : Value -> Value
removeLeadingNull value =
    case JD.decodeValue removeLeadingNullDecoder value of
        Ok val ->
            val

        Err _ ->
            value


decodeXmlValue : Value -> Value
decodeXmlValue value =
    case JD.decodeValue xmlValueDecoder value of
        Ok val ->
            removeLeadingNull val

        Err s ->
            value


listToNull : List Value -> Decoder Value
listToNull list =
    case list of
        [] ->
            JD.succeed JE.null

        _ ->
            JD.fail "Not an empty list"



-- Simplify the parsed XML by replacing the objects containing a "value"


xmlValueDecoder : Decoder Value
xmlValueDecoder =
    JD.oneOf
        [ JD.map decodeXmlValue <| JD.field "value" JD.value
        , JD.list JD.value |> JD.andThen listToNull
        , JD.map (JE.list identity) (JD.list <| JD.lazy (\_ -> xmlValueDecoder))
        , JD.map JE.object <|
            JD.keyValuePairs (JD.lazy (\_ -> xmlValueDecoder))
        , JD.value
        ]


{-| How to handle a tag in a `TagSpec`.

`Required` tags error if not there.
`RequiredIgnore` tags must be in the XML, but are not returned.
`Optional` tags will become null if not in the XML.
`Multiple` tags become a list.

-}
type Required
    = Required
    | RequiredIgnore
    | Optional
    | Multiple


{-| A description of one tag to decode: (<tag name>, Required, <tag decoder>)
-}
type alias TagSpec =
    ( String, Required )


tagDecoderCallback : Decoder value -> Value -> Decoder value
tagDecoderCallback valueDecoder value =
    case JD.decodeValue valueDecoder value of
        Ok val ->
            JD.succeed val

        Err msg ->
            JD.fail <| JD.errorToString msg


{-| Decode the contents of an XML tag with subtags.

Each TagSpec pulls one or more matching tags from the list.

Unspecified tags in the parsed `Value` are skipped.

You end up with a single JSON object, with the tags as keys,
to which you can apply a standard JSON decoder.

-}
tagDecoder : Decoder value -> List TagSpec -> Decoder value
tagDecoder valueDecoder tagSpecs =
    tagValueDecoder tagSpecs
        |> JD.andThen (tagDecoderCallback valueDecoder)


tagValueDecoder : List TagSpec -> Decoder Value
tagValueDecoder tagSpecs =
    JD.list JD.value
        |> JD.andThen (doTagDecode tagSpecs)


oneTagDecoder : String -> Decoder Value
oneTagDecoder tag =
    JD.field tag JD.value


notateError : String -> List TagSpec -> List Value -> Decoder Value
notateError msg tagSpecs values =
    JD.fail <|
        msg
            ++ "\ntagSpecs: "
            ++ Debug.toString tagSpecs
            ++ "\nvalues: "
            ++ Debug.toString values


doTagDecode : List TagSpec -> List Value -> Decoder Value
doTagDecode tagSpecs values =
    let
        loop : List TagSpec -> List Value -> List ( String, Value ) -> Decoder Value
        loop specs vals res =
            case specs of
                [] ->
                    JD.succeed <| JE.object res

                ( tag, req ) :: specsTail ->
                    case req of
                        Multiple ->
                            case decodeMultiple tag vals of
                                Ok ( valsTail, value ) ->
                                    loop specsTail valsTail <|
                                        ( tag, value )
                                            :: res

                                Err msg ->
                                    notateError msg specs vals

                        _ ->
                            case vals of
                                [] ->
                                    hangingVals specs res

                                val :: valsTail ->
                                    case decodeOne tag req val valsTail of
                                        Ok ( vtail, oneRes, found ) ->
                                            loop specsTail
                                                (if found then
                                                    vtail

                                                 else
                                                    vals
                                                )
                                            <|
                                                case req of
                                                    RequiredIgnore ->
                                                        res

                                                    _ ->
                                                        ( tag, oneRes ) :: res

                                        Err msg ->
                                            notateError msg specs vals
    in
    loop tagSpecs values []



-- Here when we've successfully parsed all the elements of the list.
-- If there are any tag specs left, make sure they're all
-- Optional or Multiple, and append nulls or empty lists.
-- If any Required specs are left, error.


hangingVals : List TagSpec -> List ( String, Value ) -> Decoder Value
hangingVals specs res =
    let
        loop specsTail r =
            case specsTail of
                [] ->
                    JD.succeed <| JE.object r

                ( tag, req ) :: specsTailTail ->
                    case req of
                        Optional ->
                            loop specsTailTail <| ( tag, JE.null ) :: r

                        Multiple ->
                            loop specsTailTail <| ( tag, JE.list identity [] ) :: r

                        _ ->
                            JD.fail <| "Tag not found: " ++ tag
    in
    loop specs res



-- Decode a single tag from the list.
-- Skip elements with other tags until you find one with the passed `tag`.
-- The `Required` arg is guaranteed NOT to be `Multiple`.


decodeOne : String -> Required -> Value -> List Value -> Result String ( List Value, Value, Bool )
decodeOne tag req val valsTail =
    let
        oneDecoder =
            oneTagDecoder tag

        loop vl vt =
            case JD.decodeValue oneDecoder vl of
                Ok v ->
                    Ok ( vt, v, True )

                Err msg ->
                    case vt of
                        [] ->
                            case req of
                                Optional ->
                                    Ok ( valsTail, JE.null, False )

                                _ ->
                                    Err <| "Required tag not found: " ++ tag

                        vv :: vvt ->
                            loop vv vvt
    in
    loop val valsTail


decodeMultiple : String -> List Value -> Result String ( List Value, Value )
decodeMultiple tag vals =
    let
        oneDecoder =
            oneTagDecoder tag

        loop valsTail res =
            case valsTail of
                [] ->
                    if res == [] then
                        Ok ( vals, JE.list identity [] )

                    else
                        Ok
                            ( []
                            , JE.list identity <| List.reverse res
                            )

                val :: tail ->
                    case JD.decodeValue oneDecoder val of
                        Ok v ->
                            loop tail <| v :: res

                        Err msg ->
                            if res == [] then
                                loop tail []

                            else
                                Ok
                                    ( valsTail
                                    , JE.list identity <| List.reverse res
                                    )
    in
    loop vals []
