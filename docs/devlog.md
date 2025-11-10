# Dev Log

This devlog is **very incomplete** so please look the git commit history as complement.

## 26.03.2025

- Eta-trick for implementing delay (which is always free)
- "Halt" constructor for creating initial computation
- Recursive environments
- omega, fix, add, inc, dec, infinity
    * infinity is defined as `inf = comu[k -> < S(inf) |> k >]`, given any continuation we can replace inf with `S(inf)` freely in it.

## 29.05.2025
  
- Move all test files into ./test folder.
- try to suppress all warnings.
- Fix parser issue: allow constructor with no arguments to be parsed without parentheses.
- Add temporary solution for "~Halt", the evaluator stop when "~Halt" is matched.
- *First version of working interpreter*

## 09.07.2025

- Add syntactic sugars, translate some examples in sugared style.
- Our language needs explicit label on dependencies, which might or mightnot be an advantage.
- Issues on parsing let bindings.
- A comparison with Haskell using current sugars:
```(Haskell)
handlePacket packet = do
  parsed <- parseHeaders packet
  validated <- validatePacket parsed
  case validated of
    Valid data -> processData data >>= sendResponse
    Invalid err -> sendError err
```
```(mmu)
fn handle_packet packet k := do
  parsed <- parse_headers packet,
  validated <- validate parsed
  then validated . 
    [ Valid data -> process data >>= send_response then k ]
    | Invalid err -> err k @ send_error;
```

## 15.08

some visualization
cli (done by ky)
exploring and ideas written in docs/ideas.md
