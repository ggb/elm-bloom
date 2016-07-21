module Bloom exposing 
  ( Filter
  , empty
  , add
  , test
  )

{-| Elm module for 

@docs Filter

# API

@docs empty, add, test

-}

import Array exposing (Array)
import Murmur3 exposing (hashString)


{-| Filter

    type alias Filter = 
      Array Int
-}
type alias Filter = 
  Array Int


{-| Empty

    import Bloom

    Bloom.empty 10 |> Array.toList

    -- [0,0,0,0,0,0,0,0,0,0]
-}
empty : Int -> Filter
empty m =
  Array.initialize m (always 0)


hashes : Int -> Int -> String -> List Int
hashes k m word =
  let
    hash1 = hashString 0 word
    hash2 = hashString hash1 word
  in
    List.map 
      (\i -> (hash1 + i * hash2) % m |> abs) 
      [1..k]


{-| Add

    import Bloom exposing (add, empty)

    add' = add 3

    t = List.foldr add' (empty 20) ["foo", "bar", "baz"]
-}
add : Int -> String -> Filter -> Filter
add k word set =
  let
    m = Array.length set
  in
    List.foldr 
      (\pos acc -> Array.set pos 1 acc) 
      set 
      (hashes k m word)


{-| Test

    import Bloom exposing (add, empty)

    -- create filter t with k = 3
    test' = test 3

    test' "foo" t == True
    test' "fou" t == False
-}
test : Int -> String -> Filter -> Bool
test k word set =
  let 
    m = Array.length set
  in
    hashes k m word
    |> List.map (\pos -> Array.get pos set)
    |> List.all (\entry -> entry == Just 1)