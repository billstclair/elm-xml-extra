module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import Xml.Extra exposing ( TagSpec, Required(..), Error(..), DecodeDetails
                          , xmlToJson, decodeXml
                          , tagDecoder, requiredTag, optionalTag, multipleTag
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

type alias Friend =
    { name : String
    , nickname : String
    }

type alias PersonRecord =
    { name : String
    , age : Int
    , spouse : Maybe Person
    , children : List Person
    , favoriteColor : Maybe String
    , bestFriend : Friend
    }

type Person =
    Person PersonRecord

-- Avoid the temptation to move the JD.lazy calls below into
-- a top-level function.
-- It tickles an Elm compiler bug.
personDecoder : Decoder Person
personDecoder =
    JD.map6 (\name age spouse children color friend ->
                 Person
                 <| PersonRecord name age spouse children color friend
            )
        (JD.field "name" JD.string)
        (JD.field "age" JD.int)
        (optionalTag "spouse"
             (JD.lazy (\_ -> personDecoder)) personTagSpecs)
        (multipleTag "child"
             (JD.lazy (\_ -> personDecoder)) personTagSpecs)
        (optionalTag "favoriteColor" JD.string [])
        (requiredTag "bestFriend" friendDecoder friendTagSpecs)

personTagSpecs : List TagSpec
personTagSpecs =
    [ ("name", Required)
    , ("age", Required)
    , ("spouse", Optional)
    , ("child", Multiple)
    , ("favoriteColor", Optional)
    , ("bestFriend", Required)
    ]

friendDecoder : Decoder Friend
friendDecoder =
    JD.map2 Friend
        (JD.field "name" JD.string)
        (JD.field "nickname" JD.string)

friendTagSpecs : List TagSpec
friendTagSpecs =
    [ ("name", Required)
    , ("nickname", Required)
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

noFriend : Friend
noFriend =
    Friend "nobody" "nothing"

simplePerson : String -> Int -> Person
simplePerson name age =
    Person <| PersonRecord name age Nothing [] Nothing noFriend

person : String -> Int -> Maybe Person -> List Person -> Person
person name age spouse children =
    Person <| PersonRecord name age spouse children Nothing noFriend

fullPerson : String -> Int -> Maybe Person -> List Person -> Maybe String -> Friend -> Person
fullPerson name age spouse children color friend =
    Person <| PersonRecord name age spouse children color friend

noFriendXml : String
noFriendXml =
    """
     <bestFriend>
       <name>nobody</name>
       <nickname>nothing</nickname>
     </bestFriend>
    """

simpleXml : String -> String
simpleXml body =
    "<person>" ++ body ++ noFriendXml ++ "</person>"

personData : List (String, Result Error Person)
personData =
    [ ( simpleXml "<name>Irving</name><age>30</age>"
      , Ok <| simplePerson "Irving" 30
      )
    , ( simpleXml "<name>Irving</name><ignore>foo</ignore><age>30</age>"
      , Ok <| simplePerson "Irving" 30
      )
    , ( simpleXml "<name>John</name>"
      , Err decodeError
      )
    , ( simpleXml <|
        """
         <name>John</name>
         <age>30</age>
         <spouse>
           <name>Joan</name>
           <age>28</age>
        """ ++ noFriendXml ++
        """
         </spouse>
        """
       , Ok
           <| person "John"
               30
               (Just <| simplePerson "Joan" 28)
               []
      )
    , ( simpleXml <|
        """
         <name>John</name>
         <age>30</age>
         <spouse>
           <name>Joan</name>
           <age>28</age>
        """ ++ noFriendXml ++
        """
         </spouse>
         <favoriteColor>blue</favoriteColor>
        """
       , Ok
           <| fullPerson "John"
               30
               (Just <| simplePerson "Joan" 28)
               []
               (Just "blue")
               noFriend
      )
    , ( simpleXml
        """
           <name>John</name>
           <age>30</age>
           <favoriteColor>green</favoriteColor>
        """
      , Ok
          <| fullPerson "John"
              30
              Nothing
              []
              (Just "green")
              noFriend
      )
    , ( """
         <person>
           <name>John</name>
           <age>30</age>
           <child>
             <name>Joe</name>
             <age>3</age>
        """ ++ noFriendXml ++
        """
           </child>
           <bestFriend>
             <name>Mel</name>
             <nickname>Johnny</nickname>
           </bestFriend>
         </person>
        """
      , Ok
          <| fullPerson "John"
              30
              Nothing
              [ simplePerson "Joe" 3 ]
              Nothing
              (Friend "Mel" "Johnny")
      )
    ]
