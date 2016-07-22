module Bloom exposing 
  ( Filter
  , empty
  , add
  , test
  )

{-| Elm [Bloom filter](https://en.wikipedia.org/wiki/Bloom_filter) implementation using [Murmur3](https://en.wikipedia.org/wiki/MurmurHash). It may not be the fastest implementation, but it is simple and easy to use. This [blog post](https://corte.si/posts/code/bloom-filter-rules-of-thumb/index.html) with rules of thumb for choosing m and k might be helpful. 

Use it as follows:

    import Bloom exposing (empty, add, test)

    -- create an empty filter with m elements and k hashes
    emptyFilter = empty 1000 4

    -- add elements to the filter
    filter = 
      List.foldr 
        add
        emptyFilter 
        ["foo", "bar", "baz", ... ]

    -- check if elements are recognized by the filter
    test "bar" filter == True
    test "barr" filter == False

# Data

@docs Filter

# Create, manipulate and test

@docs empty, add, test

-}

import Array exposing (Array)
import Murmur3 exposing (hashString)


{-| The Filter struct holds an array containing the actual filter, but also the values for m and k (for simplicity).
-}
type alias Filter = 
  { set: Array Int
  , m: Int
  , k: Int
  }


{-| Creates an empty Filter, containing m elements and using k hashes.

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


{-| Adds elements to an existing Filter.

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


{-| Tests if a filter contains an element. By its probalistic nature this function may yield false positive results.

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