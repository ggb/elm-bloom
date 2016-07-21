# elm-bloom: Bloom Filter for Elm

## Installation

```bash
elm package install ggb/elm-bloom
```

## Usage

Usage is straightforward: 

```elm
import Bloom exposing (empty, add, test)

filter = 
  List.foldr 
    (add 4) 
    (empty 1000) 
    ["foo", "bar", "baz", ... ]

-- test 4 "bar" filter == True
-- test 4 "barr" filter == False
```