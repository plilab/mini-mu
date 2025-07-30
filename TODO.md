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

- underscore/wildcard patterns: [ Z -> False . k | S _ -> ... | _ -> True . k ]  [done]
- nested patterns: [ (Pair Z (Pair j k)) -> ...] [done]

## Long Term

- Syntactic Sugar
- Develop Programs in mini-mu

- Explore the connection with Zipper[ding]
  - context with/wo pointers reversal
  - The Derivative of a Regular Type is its Type of One-Hole Contexts[http://strictlypositive.org/diff.pdf]
  - https://simon.peytonjones.org/codata-in-action/ section 1 & 2 & 3
  - https://www.cs.uoregon.edu/Reports/DRP-201905-Sullivan.pdf
- Explore how to use macros...[ky]
- user defined infix operator[ky]
- Parsec with 4 kinds of continuations
- backtracking search[ding]
- Regular expression, derivatives
- variance on list_map/list_fold...
- Implementation of a exception system, monad/comonad system[ding]
  - exception monad, reader comonad
- Object-Orientation system[ky]
- positive negative pair/sum system
- excluded-middle law / devil's bargan
  - Either devil gives $1M or (you give devil $1M and devil gives one wish)
    - one wish = impossible value (bottom / infinite loop)
    - $1M = object of type "A"
  - call/cc: cont (cont a) ~= ((a -> bot) -> bot) ~=? { k -> k . a }
- codata[interesting]
- sequent logic[ky]
  - fallback to nd
- async (e.g., JavaScript)[d]
- type[HARD]
- applicative functors
