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
      { set: Array Int
      , m: Int
      , k: Int
      }
-}
type alias Filter = 
  { set: Array Int
  , m: Int
  , k: Int
  }


{-| Empty

    import Bloom

    Bloom.empty 10 3 |> Array.toList

    -- {m=10, k=3, set=[0,0,0,0,0,0,0,0,0,0]}
-}
empty : Int -> Int -> Filter
empty m k =
  { set = Array.initialize m (always 0)
  , m = m
  , k = k
  }


hashes : Int -> Int -> String -> List Int
hashes k m word =
  let
    hash1 = hashString 0 word
    hash2 = hashString hash1 word
  in
    List.map 
      (\i -> (hash1 + i * hash2) % m |> abs) 
      [1..k]


updateFilter : Int -> Int -> Array Int -> Filter
updateFilter m k newSet =
  { set = newSet
  , m = m
  , k = k
  }


{-| Add

    import Bloom exposing (add, empty)

    t = List.foldr add (empty 20 3) ["foo", "bar", "baz"]
-}
add : String -> Filter -> Filter
add word {m, k, set} = 
  List.foldr 
    (\pos acc -> Array.set pos 1 acc) 
    set 
    (hashes k m word)
  |> updateFilter m k


{-| Test

    import Bloom exposing (add, empty)

    -- create filter t
    
    test "foo" t == True
    test "fou" t == False
-}
test : String -> Filter -> Bool
test word {m, k, set} =
  hashes k m word
  |> List.map (\pos -> Array.get pos set)
  |> List.all (\entry -> entry == Just 1)