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
    (* TODO: decide whether ocaml-style or rust-style is better. *)
    let VAR = EXPR in CMD
    => EXPR . { VAR -> CMD }

    letcc COVAR = COEXPR in CMD
    => { COVAR -> CMD } . COEXPR

    match EXPR with
    | PAT1 -> CMD1
    | PAT2 -> CMD2
    ...
    | PATn -> CMDn
    => EXPR . { PAT1 -> CMD1 | PAT2 -> CMD2 ... | PATn -> CMDn }

    dispatch COEXPR with
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
        PAT1 <- FUN1(ARGS...),
        PAT2 <- FUN2(ARGS...),
        ...
        PATn-1 <- FUNn-1(ARGS...),
        PATn <- FUNn(ARGS...)
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
    fn quick_sort xs :=
      match xs with
      | Nil -> Nil . k
      | Cons x xs' ->
        do
          (s, g) <- split(xs),
          sorted_s <- sort(s),
          sorted_g <- sort(g)
        then let arg = (sorted_s, pvt::sorted_g) in
          append(arg) . k
    ```

## Delimited Continuations

We use a special kind of command as delimiter, and I am quite sure this can give us delimited control. Interestingly, this kind command can be implemented as a dynamic time generated value, or a mu-abstraction.
Here is the thing:
```ocaml
add @ 1 <add @ 1 2 here> halt
=> { _callback -> add @ 1 2 _callback } . { _delim -> add @ 1 _delim halt }
```
conceptually, in the delimited scope, all "here" are changed to "there", and we eta-expand the command with this introduced variable, namely:
```ocaml
{ _callback -> add @ 1 2 _callback }
```
Out of the delimited scope, we introduce a one-hole-context, namely:
```ocaml
{ _delim -> add @ 1 _delim halt }
```
So Basically, this sugar is in this form:
```ocaml
CONTEXT <CMD>
=> {_callback -> CMD[_callback/here]} . {_delim -> CONTEXT[delim] }
"/" means substitutes
```

## OOP
```ocaml
module car :=
  field brand = "bmw"
  field year = 2020
  
  Get_brand() -> return this.brand
  Year_till(year) -> return sub(year, this.year)
  Check_year{t, f}(year, t, f) -> eq @ year 10 . { True -> year . t | False -> year . f }
end
=>
car = {
    Get_brand k -> 
    ("bmw", 2020) . { (_brand, _year) ->  _brand . k }
  | Year_till_now year k -> 
    ("bmw", 2020) . { (_brand, _year) -> sub(year, _year) . k }
  | Check_year year t f ->
    ("bmw", 2020) . { (_brand, _year) -> eq @ year 10 . { True -> year . t | False -> year . f } }
}

usage:

car::Get_brand() . halt
=> 
{ _k -> car . Get_brand _k } . halt
```

So generally the sugar is:

```ocaml
module OBJId :=
  field VarId1 = Expr1
  ...
  field VarIdn = Exprn

  ConsId1(ARGS*) -> CMD1
  ConsId2{CONTS}(ARGS*) -> CMD2
```

```ocaml
OBJId := {
  ConsId1 [ARGS* ++ _k] ->
    (Expr1,...,Exprn) . { (_VarId1,...,_VarIdn) -> CMD1 }
  
  ConsId2 ARGS* ->
    (Expr1,...,Exprn) . { (_VarId1,...,_VarIdn) -> CMD2 }
}
return Expr => Expr . _k
```

## Other  
- Letrec
- Debugger
- Side Effects (but that might be a bad idea)
