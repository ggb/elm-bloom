import Bloom exposing (empty, add)
import Html exposing(..)
import ElmTest exposing (..)
import String
import Array
import Regex exposing (HowMany(All), regex)


trim : String -> String
trim word =
  word
  |> Regex.replace All (regex "^\\W+") (\_ -> "")
  |> Regex.replace All (regex "\\W+$") (\_ -> "")


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


main : Html String
main =
  let
    lines = String.lines tests
  in
    div [ ] (List.map (\s -> line s) lines)


line : String -> Html String
line s =
  div [ ] [
    text s,
    br [ ] [ ]
  ]


tests : String
tests =
  let
    s = List.foldr add (empty 20 3) ["foo","bar","baz"]
    t = List.foldr add (empty 1000 4) longText |> Debug.log "t"
    checkT = List.map (\w -> Bloom.test w t) longText
  in
    suite "Bloom Filter"
      [ defaultTest (assertEqual [] (empty 0 0 |> .set |> Array.toList))
      , defaultTest (assertEqual [0,0,0,0,0,0,0,0,0,0] (empty 10 3 |> .set |> Array.toList))
      , defaultTest (assertEqual True (Bloom.test "foo" s))
      , defaultTest (assertEqual True (Bloom.test "bar" s))
      , defaultTest (assertEqual True (Bloom.test "baz" s))
      , defaultTest (assertEqual False (Bloom.test "fuo" s))
      , defaultTest (assertEqual False (Bloom.test "bas" s))
      , defaultTest (assertEqual False (Bloom.test "bak" s))
      , defaultTest (assertEqual True (List.all identity checkT))
      , defaultTest (assertEqual False (Bloom.test "jorge" t))
      , defaultTest (assertEqual False (Bloom.test "luis" t))
      , defaultTest (assertEqual False (Bloom.test "borges" t))
      ]
    |> stringRunner