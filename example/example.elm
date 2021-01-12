----------------------------------------------------------------------
--
-- example.elm
-- Example of using billstclair/elm-xml-extra
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module Main exposing (..)

import Html
    exposing
        ( Attribute
        , Html
        , div
        , input
        , p
        , pre
        , text
        )
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onCheck)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import String.Extra as SE
import Xml
import Xml.Decode as XD
import Xml.Extra
    exposing
        ( Required(..)
        , TagSpec
        , decodeXml
        , multipleTag
        , optionalTag
        , tagDecoder
        , xmlToJson
        )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { xml : String
    , isComplexXml : Bool
    , isComplexDecoder : Bool
    , tagSpecs : List TagSpec
    , decoder : Decoder Person
    }


type Msg
    = SetXml String
    | SetIsComplexXml Bool
    | SetIsComplexDecoder Bool


simpleXml =
    """
<?xml version="1.0" encoding="UTF-8"?>
<person>
  <name>noah</name>
  <age max="100">50</age>
</person>
    """



-- Will get more complicated


complexXml =
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


init : ( Model, Cmd Msg )
init =
    ( { xml = simpleXml
      , isComplexXml = False
      , isComplexDecoder = False
      , tagSpecs = simplePersonTagSpecs
      , decoder = simplePersonDecoder
      }
    , Cmd.none
    )


type alias PersonRecord =
    { name : String
    , age : Int
    , spouse : Maybe Person
    , children : List Person
    }


type Person
    = Person PersonRecord


simplePersonDecoder : Decoder Person
simplePersonDecoder =
    JD.map2
        (\name age ->
            Person <| PersonRecord name age Nothing []
        )
        (JD.field "name" JD.string)
        (JD.field "age" JD.int)


simplePersonTagSpecs : List TagSpec
simplePersonTagSpecs =
    [ ( "name", Required )
    , ( "age", Required )
    ]



-- Avoid the temptation to move the JD.lazy calls below into
-- a top-level function.
-- It tickles an Elm compiler bug.


personDecoder : Decoder Person
personDecoder =
    JD.map4
        (\name age spouse children ->
            Person <| PersonRecord name age spouse children
        )
        (JD.field "name" JD.string)
        (JD.field "age" JD.int)
        (optionalTag "spouse"
            (JD.lazy (\_ -> personDecoder))
            personTagSpecs
        )
        (multipleTag "child"
            (JD.lazy (\_ -> personDecoder))
            personTagSpecs
        )


personTagSpecs : List TagSpec
personTagSpecs =
    [ ( "name", Required )
    , ( "age", Required )
    , ( "spouse", Optional )
    , ( "child", Multiple )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetXml xml ->
            ( { model | xml = xml }
            , Cmd.none
            )

        SetIsComplexXml isComplex ->
            ( { model
                | isComplexXml = isComplex
                , xml =
                    if isComplex then
                        complexXml

                    else
                        simpleXml
              }
            , Cmd.none
            )

        SetIsComplexDecoder isComplex ->
            ( { model
                | isComplexDecoder = isComplex
                , tagSpecs =
                    if isComplex then
                        personTagSpecs

                    else
                        simplePersonTagSpecs
                , decoder =
                    if isComplex then
                        personDecoder

                    else
                        simplePersonDecoder
              }
            , Cmd.none
            )


b : List (Html msg) -> Html msg
b body =
    Html.b [] body


br : Html msg
br =
    Html.br [] []


view : Model -> Html Msg
view model =
    let
        xml =
            model.xml

        xval =
            XD.decode xml

        xs =
            case xval of
                Ok v ->
                    toString v

                Err msg ->
                    msg

        val =
            case xval of
                Ok v ->
                    JE.encode 1 <| Xml.xmlToJson v

                Err msg ->
                    msg

        simpleVal =
            case xval of
                Ok v ->
                    JE.encode 1 <| xmlToJson v

                Err msg ->
                    msg

        -- This simple call will suffice for most of your XML parsing.
        decodedSimpleValue =
            decodeXml xml "person" model.decoder model.tagSpecs

        decodedString =
            case decodedSimpleValue of
                Err err ->
                    "Error:" ++ toString err

                Ok s ->
                    toString s
    in
    div [ style "margin-left" "2em" ]
        [ p []
            [ input
                [ type_ "checkbox"
                , checked model.isComplexXml
                , onCheck SetIsComplexXml
                ]
                []
            , text "  complex XML"
            , br
            , input
                [ type_ "checkbox"
                , checked model.isComplexDecoder
                , onCheck SetIsComplexDecoder
                ]
                []
            , text " complex Decoder"
            ]
        , b [ text "Decoded:" ]
        , pre []
            [ text <| SE.softWrapWith 80 "\n  " decodedString ]
        , b [ text "XML:" ]
        , pre []
            [ text xml ]
        , b [ text "Xml.Extra.xmlToJson:" ]
        , pre []
            [ text simpleVal ]
        , b [ text "Xml.xmlToJson:" ]
        , pre []
            [ text val ]
        , b [ text "Parsed XML:" ]
        , pre []
            [ text <| SE.softWrapWith 80 "\n  " xs ]
        ]
