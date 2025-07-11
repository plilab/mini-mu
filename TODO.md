# TODO

## Near Term

- parser: `Ap Z ~Halt` is currently parsed as `Ap (Z ~Halt)` but should be as `(Ap Z) ~Halt` [DONE]
  - need to explore left-associative (in parsing) application
    - will probably need "left-factoring"

- better syntax
  - movement away from angle brackets - confusing [DONE]
  - proposal with examples first, don't implement yet [DONE]
- try visualization of steps
  - some way to hide the store/env
  - command line (non-interactive)
  - command line (interactive)
  - graphViz
  - HTML
  - GUI (GTK)
- better parsing error messages (e.g., with "label") (also lookup how to improve error messages in Parsec) [DONE]
  - Might want to support error messages for `comu [ Ap ... ]` when it should be `comu [ ~Ap ... ]`
- better error messages [DONE]
  - E.g. when Map.! fails
- better pretty printer (wait for better syntax) [?DONE]
- move tests from Main to tests/ (easy, so should just do it) [DONE]
- how to do halt/return a value from a program [Michael]

- pretty-printer to latex [?]

- underscore/wildcard patterns: [ Z -> False . k | S _ -> ... | _ -> True . k ] 
- nested patterns: [ (Pair Z (Pair j k)) -> ...]

## Long Term

- Syntactic Sugar
- Develop Programs in mini-mu
