## Finalized Sugar

1. Unify Expr and CoExpr [Done]

2. Use >> to simplify notation of command, use @ to simplify command involving
  Ap constructor

< Cons |> cont > === Cons >> cont
< Ap x k |> fun > === x k @ fun
< fun |> Ap x y k > === fun @ x y k

3. def/run grammar:

def NAME ARGS* := COMMAND === NAME = mu[ Ap ARGS... -> COMMAND ]

run COMMAND === main = mu[ Halt -> COMMAND ]

E.g.

def list_map f xs k := xs >> [ Nil -> ... | Cons -> ...]

run list_map >> Ap3 inc (List:: (S (S Z)) (List:: (S Z) (List:: Z Nil))) Halt

3. let grammar:

let (NAME = EXPRESSION/COMMAND)(. NAME = EXPRESSION/COMMAND)* in
  EXPRESSION/COMMAND

4. where grammar:

where (NAME = EXPRESSION/COMMAND)(. NAME = EXPRESSION/COMMAND)*

E.g.

def list_map f xs k :=
  xs >> [ Nil -> nil_case | List:: x xs' -> cons_case ]
    where
      nil_case = Nil, k.
      cons_case = f @ x [ y -> list_map @ xs' [ ys' -> List:: y ys', k ] ]

5. do...then grammar (let's temperarily use do instead of seq since it is clearer,
but whether or not it is a monad is pending on further discussion)

do
  NAME <- EXPRESSION EXPRESSION*,
  (NAME <- EXPRESSION EXPRESSION*,)*
then COMMAND

===

EXPRESSION @ EXPRESSION* [ NAME -> EXPRESIION @ EXPRESSION* [ NAME -> COMMAND]]

E.g.
quick_sort = mu[ Ap xs k ->
  < xs |> mu[ Nil -> < Nil |> k >
            | List:: pivot xs' ->
                < list_filter |> Ap4 lt xs' pivot mu[ smaller ->
                  < list_filter |> Ap4 geq xs' pivot mu[ greater ->
                    < quick_sort |> Ap smaller mu[ sorted_smaller ->
                      < quick_sort |> Ap greater mu[ sorted_greater ->
                        < list_append |> Ap3 sorted_smaller (List:: pivot sorted_greater) k > ] > ] > ] > ] > ] > ];

def quick_sort xs k :=
  xs >> [ Nil -> Nil >> k
        | List:: pivot xs' ->
            do
              smaller <- list_filter lt xs' pivot,
              greater <- list_filter geq xs' pivot,
              sorted_s <- quick_sort smaller,
              sorted_g <- quick_sort greater,
            then list_append sorted_s (List:: pivot sorted_g) >> k
        ]
  
6. Simplify mu[] as []
 easy
7. Unify Ap [Done]

8. Expand numerical to S...Z
 easy

Later: idiom brackets