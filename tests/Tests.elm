module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import Xml.Extra exposing ( TagSpec, Required(..), Error(..), DecodeDetails
                          , xmlToJson, decodeXml
                          , tagDecoder, optionalTag, multipleTag
                          )

import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE

log = Debug.log

enableLogging : Bool
enableLogging =
  False --change to True to log JSON input & output results

maybeLog : String -> a -> a
maybeLog label value =
  if enableLogging then
    log label value
  else
    value

testMap : (x -> String -> Test) -> List x -> List Test
testMap test data =
    let numbers = List.map toString <| List.range 1 (List.length data)
    in
        List.map2 test data numbers

all : Test
all =
    Test.concat <|
        List.concat
            [ (testMap personTest personData)
            ]

expectResult : Result err a -> Result err a -> Expectation
expectResult sb was =
    case (maybeLog "  result" was) of
        Err err ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True
                Ok _ ->
                    Expect.false (toString err) True
        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True
                Ok sbv ->
                    Expect.equal sbv wasv

type alias PersonRecord =
    { name : String
    , age : Int
    , spouse : Maybe Person
    , children : List Person
    , favoriteColor : Maybe String
    , friends : List String
    }

type Person =
    Person PersonRecord

-- Avoid the temptation to move the JD.lazy calls below into
-- a top-level function.
-- It tickles an Elm compiler bug.
personDecoder : Decoder Person
personDecoder =
    JD.map6 (\name age spouse children color friends ->
                 Person
                 <| PersonRecord name age spouse children color friends
            )
        (JD.field "name" JD.string)
        (JD.field "age" JD.int)
        (optionalTag "spouse"
             (JD.lazy (\_ -> personDecoder)) personTagSpecs)
        (multipleTag "child"
             (JD.lazy (\_ -> personDecoder)) personTagSpecs)
        (optionalTag "favoriteColor" JD.string [])
        (multipleTag "friend" JD.string [])

personTagSpecs : List TagSpec
personTagSpecs =
    [ ("name", Required)
    , ("age", Required)
    , ("spouse", Optional)
    , ("child", Multiple)
    , ("favoriteColor", Optional)
    , ("friend", Multiple)
    ]

personTest : (String, Result Error Person) -> String -> Test
personTest (xml, result) name =
    test ("personTest \"" ++ name ++ "\"")
        (\_ ->
             expectResult result
                 <| decodeXml xml "person" personDecoder personTagSpecs
        )

xmlError : Error
xmlError =
    XmlError "message"

decodeError : Error
decodeError =
    DecodeError { value = JE.null
                , msg = "message"
                }

simplePerson : String -> Int -> Person
simplePerson name age =
    Person <| PersonRecord name age Nothing [] Nothing []

person : String -> Int -> Maybe Person -> List Person -> Person
person name age spouse children =
    Person <| PersonRecord name age spouse children Nothing []

fullPerson : String -> Int -> Maybe Person -> List Person -> Maybe String -> List String -> Person
fullPerson name age spouse children color friends =
    Person <| PersonRecord name age spouse children color friends

personData : List (String, Result Error Person)
personData =
    [ ( "<person><name>Irving</name><age>30</age></person>"
      , Ok <| simplePerson "Irving" 30
      )
    , ( "<person><name>Irving</name><ignore>foo</ignore><age>30</age></person>"
      , Ok <| simplePerson "Irving" 30
      )
    , ( "<person><name>John</name></person>"
      , Err decodeError
      )
    , ( """
         <person>
           <name>John</name>
           <age>30</age>
           <spouse>
             <name>Joan</name>
             <age>28</age>
           </spouse>
         </person>
        """
       , Ok
           <| person "John"
               30
               (Just <| simplePerson "Joan" 28)
               []
      )
    , ( """
         <person>
           <name>John</name>
           <age>30</age>
           <spouse>
             <name>Joan</name>
             <age>28</age>
           </spouse>
           <favoriteColor>blue</favoriteColor>
         </person>
        """
       , Ok
           <| fullPerson "John"
               30
               (Just <| simplePerson "Joan" 28)
               []
               (Just "blue")
               []
      )
    , ( """
         <person>
           <name>John</name>
           <age>30</age>
           <favoriteColor>green</favoriteColor>
         </person>
        """
      , Ok
          <| fullPerson "John"
              30
              Nothing
              []
              (Just "green")
              []
      )
    , ( """
         <person>
           <name>John</name>
           <age>30</age>
           <child>
             <name>Joe</name>
             <age>3</age>
           </child>
           <friend>Mel</friend>
           <friend>Bud</friend>
           <friend>Sam</friend>
         </person>
        """
      , Ok
          <| fullPerson "John"
              30
              Nothing
              [ simplePerson "Joe" 3 ]
              Nothing
              [ "Mel", "Bud", "Sam" ]
      )
    ]
