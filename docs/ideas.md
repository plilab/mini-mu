# Ideas

This is a documentation file that contains the ideas for the projects, including
those have been implemented and those still haven't been implemented.
Some of the ideas are still at very early stage and might be proven wrong.

We will split the goals into catagories:

## General Goals

- Implement a nice language
- Discover how to use the language well

- Implement a compiler
- Large example (e.g., a parser, protocol)
- Want to discover what idioms to use, in particular find ways that the duality is actually an advantage

## Sytax
General Goals: Fewer brackets, Fewer nestings

### Current proposed sugars:
- Cut-as-let (cut-let), thanks you Kyriel for generalizing this.
    ```ocaml
    let VAR = EXPR in CMD
    => EXPR . { VAR -> CMD }

    letc COVAR = COEXPR in CMD
    => { COVAR -> CMD } . COEXPR

    match EXPR with
    | PAT1 -> CMD1
    | PAT2 -> CMD2
    ...
    | PATn -> CMDn
    => EXPR . { PAT1 -> CMD1 | PAT2 -> CMD2 ... | PATn -> CMDn }

    patch COEXPR with
    | PAT1 -> CMD1
    | PAT2 -> CMD2
    ...
    | PATn -> CMDn
    => { PAT1 -> CMD1 | PAT2 -> CMD2 ... | PATn -> CMDn } . COEXPR
    ```
- Function application:
    ```ocaml
    FUN(E1, E2, E3) (* when user dont need explicit conts *)
    => { _k -> FUN . { _f -> (E1, E2, E3, _k) } }

    FUN{K1, K2}(E1, K1, E2, K2) (* For user who want custom amount, position for conts, and they need to indicate the exact amount and position of cont params *)
    => { (K1, K2) -> FUN . {_f -> (E1, K1, E2, K2) } }

    (* cofun? *)
    COFUN(E1, E2, E3)
    => { _k -> { _f -> (E1, E2, E3, _k) } . COFUN }

    COFUN{K1, K2}(E1, K1, E2, K2)
    => { (K1, K2) -> {_f -> (E1, K1, E2, K2) } . COFUN }
    ```
- Do/then ver2.0
    ```ocaml
      do
        PAT1 = FUN1(ARGS...),
        PAT2 = FUN2(ARGS...),
        ...
        PATn-1 = FUNn-1(ARGS...),
        PATn = FUNn(ARGS...)
      then CMDf
      =>
      FUN1(ARGS...) . {
        PAT1 -> FUN2(ARGS...) .
        {
          PAT2 -> ...
          {
            PATn-1 -> FUNn(ARGS...) .
            {
              PATn -> CMDf
            } 
          }
        }
      }
    ```
    ```ocaml
    def quick_sort xs :=
      match xs with
      | Nil -> Nil . k
      | Cons x xs' ->
        do
          (s, g) = split(xs),
          sorted_s = sort(s),
          sorted_g = sort(g)
        then let arg = (sorted_s, pvt::sorted_g) in
          append(arg) . k
    ```

## Delimited Continuations

## Other  
- Letrec
- Debugger
- Side Effects (but that might be a bad idea)
