module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Bloom exposing (empty, add)
import Regex exposing (Regex)
import Array


createTest : a -> ( b, b ) -> Test
createTest i (expected, result) = 
  test ("Test " ++ (Debug.toString i)) <|
    \() -> 
        Expect.equal expected result


createSuite : ( String, List ( a, a ) ) -> Test
createSuite (suiteName, cases) = 
  describe suiteName (List.indexedMap createTest cases)

      
regex : String -> Regex
regex str =
  Maybe.withDefault Regex.never <|
    Regex.fromString str 


trim : String -> String
trim word =
  word
  |> Regex.replace (regex "^\\W+") (\_ -> "")
  |> Regex.replace (regex "\\W+$") (\_ -> "")


tokenize : String -> List String
tokenize =
  String.toLower 
    >> String.words 
    >> List.map trim 
    >> List.filter ((/=) "") 


longText : List String
longText = """
A Bloom filter is a space-efficient probabilistic data structure, 
conceived by Burton Howard Bloom in 1970, that is used to test whether an
element is a member of a set. False positive matches are possible, but false 
negatives are not, thus a Bloom filter has a 100% recall rate. In other 
words, a query returns either "possibly in set" or "definitely not in set". 
Elements can be added to the set, but not removed (though this can be 
addressed with a "counting" filter). The more elements that are added to 
the set, the larger the probability of false positives.
""" |> tokenize


testCreate : ( String, List ( List number, List Int ) )
testCreate =
    Tuple.pair "Create Bloom Filter"
    [ ([], (empty 0 0 |> .set |> Array.toList))
    , ([0,0,0,0,0,0,0,0,0,0], (empty 10 3 |> .set |> Array.toList))
    ] 


testFilter : ( String, List ( Bool, Bool ) )
testFilter = 
    let
        s = List.foldr add (empty 20 3) ["foo","bar","baz"]
        t = List.foldr add (empty 1000 4) longText
        checkT = List.map (\w -> Bloom.test w t) longText
    in
        Tuple.pair "Use Bloom Filter"
        [ (True, (Bloom.test "foo" s))
        , (True, (Bloom.test "bar" s))
        , (True, (Bloom.test "baz" s))
        , (False, (Bloom.test "fuo" s))
        , (False, (Bloom.test "bas" s))
        , (False, (Bloom.test "bak" s))
        , (True, (List.all identity checkT))
        , (False, (Bloom.test "jorge" t))
        , (False, (Bloom.test "luis" t))
        , (False, (Bloom.test "borges" t))
        ] 


all : Test
all =
    describe "Bloom Filter"
        [createSuite testCreate, createSuite testFilter]
