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
    add
    (empty 1000 4) 
    ["foo", "bar", "baz", ... ]

-- test "bar" filter == True
-- test "barr" filter == False
```