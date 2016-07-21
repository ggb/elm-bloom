module Bloom exposing 
  ( empty
  , add
  , test
  )

import Array exposing (Array)
import Murmur3 exposing (hashString)


type alias Filter = Array Int


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
      [0..k]


add : Int -> String -> Filter -> Filter
add k word set =
  let
    m = Array.length set
  in
    List.foldr 
      (\pos acc -> Array.set pos 1 acc) 
      set 
      (hashes k m word)


test : Int -> String -> Filter -> Bool
test k word set =
  let 
    m = Array.length set
  in
    hashes k m word
    |> List.map (\pos -> Array.get pos set)
    |> List.all (\entry -> entry == Just 1)