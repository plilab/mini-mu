# 26.03.2025

- Eta-trick for implementing delay (which is always free)
- "Halt" constructor for creating initial computation
- Recursive environments
- omega, fix, add, inc, dec, infinity
    * infinity is defined as `inf = comu[k -> < S(inf) |> k >]`, given any continuation we can replace inf with `S(inf)` freely in it.
    * addition if defined as:
    ``` 
    add = mu[ (Ap3 x y k) -> 
        < x |> mu[ Z -> < y |> k > 
                 | S x' -> < add |> (Ap3 x' (S y) k) > ] > ] 
    ```
    * alternatively, addition can be defined as:
    ```
    add = comu[ (Ap3 x y k) ->
        < x |> mu[ Z -> < y |> k >
                 | S x' -> 
                    < add |> (Ap3 x' y mu[ v -> < S(v) |> k >]) > ]]
    ```
    * evaluate additions:
    ```
    => < add |> (Ap3 2 1 k) >
    => < 2 |> mu[ Z -> < 1 |> k >
                | S x' -> < add |> (Ap3 x' 1 mu[ v -> < S(v) |> k >]) > ]
    => < add |> (Ap3 1 1 mu[ v -> < S(v) |> k >]) >

    => < 1 |> mu[ Z -> < 1 |> mu[ v -> < S(v) |> k >] >
                | S x' -> < add |> (Ap3 x' 1 mu[ v -> < S(v) |> mu[ v -> < S(v)  |> k >] >]) > ]
    => < add |> (Ap3 Z 1 mu[ v -> < S(v) |> mu[ v -> < S(v) |> k >] >]) > 

    => < Z |> mu[ Z -> < 1 |> mu[ v -> < S(v) |> mu[ v -> < S(v) |> k >] >] > 
                | ... ]>
    => < 1 |> mu[ v -> < S(v) |> mu[ v -> < S(v) |> k >] >] >
    => < 2 |> mu[ v -> < S(v) |> k >] >
    => < 3 |> k >
    ```
    * omega combinator: 
    ```
    omega = < comu[ Ap x k -> < x |> Ap x k > ] |> (Ap comu[ Ap x k -> < x |> Ap x k > ] k) >
    ```
    ```
    => < comu[ Ap x0 k0 -> < x0 |> Ap x0 k0 > ] |> (Ap comu[ Ap x0 k0 -> < x0 |> Ap x0 k0 > ] k0) >
    => < comu[ Ap x0 k0 -> < x0 |> Ap x0 k0 > ] |> (Ap comu[ Ap x0 k0 -> < x0 |> Ap x0 k0 > ] k0) >
    => omega (Ap x0 k0)
    ```
    * **A simple match**
    < Cons a b c |> mu[ Cons x y z -> < Some x y |> z > ] >
    => < Some a b |> c >

    * **Or co-match**
    < comu[ Cons x y z -> < Some x y |> z > ] |> Cons a b c > 
    => < Some a b |> c >

    * **increment and decrement:**

    * **increment**
    inc = comu[ Ap x k -> < S(x) |> k > ]

    < inc |> (Ap 3 k0) >
    => < 4 |> k0 >

    * **decrement**
    dec = comu [ Ap x k -> 
                < x |> mu[ Z -> < x |> k > | S(x') -> < x' |> k > ] > ]

    < dec |> (Ap 3 k0) >
    => < 3 |> mu[ Z -> < x |> k0 > | S(x') -> < x' |> k0 > ] >
    => < 2 |> k >

    * **length of a list**
    list_length = comu[ Ap xs k0 -> 
                        < xs |> 
                          mu[ nil -> < Z |> k0 > 
                            | cons x xs' -> 
                                < list_length |> Ap xs' mu[ k' -> < S k' |> k0 > ] > ] > ]

    **EVAL**
    < list_length |> Ap (cons 1 (cons 2 nil)) some_k >

    => < comu[ ... ] |> Ap (cons 1 (cons 2 nil)) some_k >
    => < (cons 1 (cons 2 nil)) |> mu[ nil -> ...
                                    | cons x xs' -> 
                                      < list_length |> Ap xs' mu[ k' -> < S k' |> some_k > ] > ] >

    => < list_length |> Ap (cons 2 nil) mu[ k' -> < S k' |> some_k > ] >
    => < (cons 2 nil) |> mu[ nil -> ...
                            | cons x xs' -> 
                              < list_length |> 
                                Ap xs' mu[ k'' -> < S k'' |> mu[ k' -> < S k' |> some_k > ] > ] > ] >
    
    => < list_length |> Ap nil mu[ k'' -> < S k'' |> mu[ k' -> < S k' |> some_k > ] > ] >
    => < nil |> mu[ nil -> < Z |> mu[ k'' -> < S k'' |> mu[ k' -> < S k' |> some_k > ] > ] > 
                  | cons x xs' -> ... ] >

    => < Z |> mu[ k'' -> < S k'' |> mu[ k' -> < S k' |> some_k > ] > ] >
    => < S(Z) |> mu[ k' -> < S k' |> some_k > ] >
    => < S(S(Z)) |> some_k >

list_length_alt = mu[ Ap xs k -> 
                    < xs |> mu[ nil -> < Z |> k > 
                            | cons x xs' -> 
                            < Ap xs' mu[ k' -> < S k' |> k > ] |> list_length_alt > ] > ]

< Ap (cons 1 (cons 2 nil)) k |> list_length_alt >
=> < Ap (cons 2 nil) mu[ k' -> < S k' |> k > ] |> list_length_alt >
=> < Ap nil mu[ k' -> < S k' |> mu[k' -> < S k' |> k > ] > ] |> list_length_alt >
=> < Z |> mu[ k' -> < S k' |> mu[k' -> < S k' |> k > ] > ] >
=> < S(S(Z)) |> k >

list_copy = comu[ Ap xs k -> 
                < xs |> mu[ nil -> < nil |> k > 
                            | cons x xs' -> 
                                < list_copy |> Ap xs' mu[ k' -> < cons x k' |> k > ] > ] > ]
    
< list_copy |> Ap (cons 1 (cons 2 nil)) k >
=> < list_copy |> Ap (cons 2 nil) mu[ k' -> < cons 1 k' |> k > ] >
=> < list_copy |> Ap nil mu[ k' -> < (cons 2 k') |> mu[ k' -> < cons 1 k' |> k > ] > ] >
=> < nil |> mu[ k' -> < (cons 2 k') |> mu[ k' -> < cons 1 k' |> k > ] > ] >
=> < (cons 2 nil) |> mu[ k' -> < cons 1 k' |> k > ] >
=> < (cons 1 (cons 2 nil)) |> k >

list_reverse = comu[ Ap xs acc k -> 
                    < xs |> mu[ nil -> < acc |> k > 
                                | cons x xs' -> 
                                < list_reverse |> Ap xs' (cons x acc) k > ] > ]

< list_reverse |> Ap (cons 1 (cons 2 nil)) nil k >
=> < list_reverse |> Ap (cons 2 nil) (cons 1 nil) k >
=> < list_reverse |> Ap nil (cons 2 (cons 1 nil)) k >
=> < acc |> k >

list_reverse_alt = mu[ Ap xs acc k -> 
                    < xs |> mu[ nil -> < acc |> k > 
                                | cons x xs' -> 
                                < Ap xs' (cons x acc) k |> list_reverse_alt > ] > ]

< Ap (cons 1 (cons 2 nil)) nil k |> list_reverse_alt >
=> < Ap (cons 2 nil) (cons 1 nil) k |> list_reverse_alt >
=> < Ap nil (cons 2 (cons 1 nil)) k |> list_reverse_alt >
=> < (cons 2 (cons 1 nil)) |> k >

list_map = comu[ Ap3 f xs k -> 
                < xs |> mu[ nil -> < nil |> k > 
                          | cons x xs' -> 
                                < f |> Ap x mu[ 
                                    y -> < list_map |> Ap3 f xs' mu[ 
                                        ys' -> < cons y ys' |> k > ] > ] > ] > ]

< list_map |> Ap3 inc (cons 1 (cons 2 nil)) k >
=> < inc |> Ap 1 mu[ y -> < list_map |> Ap3 inc (cons 2 nil) mu[ ys' -> < cons y ys' |> k > ] > ] >
=> < 2 |> mu[ y -> < list_map |> Ap3 inc (cons 2 nil) mu[ ys' -> < cons y ys' |> k > ] > ] >
=> < list_map |> Ap3 inc (cons 2 nil) mu[ ys' -> < cons 2 ys' |> k > ] >
=> < inc |> Ap 2 mu[ y -> < list_map |> Ap3 inc nil mu[ ys' -> cons y ys' |> mu[ ys' -> < cons 2 ys' |> k > ] > ] > ] >
=> < 3 |> mu[ y -> < list_map |> Ap3 inc nil mu[ ys' -> cons y ys' |> mu[ ys' -> < cons 2 ys' |> k > ] > ] > ] > 
=> < list_map |> Ap3 inc nil mu[ ys' -> < cons 3 ys' |> mu[ ys' -> < cons 2 ys' |> k > ] > ] >
=> < nil |> mu[ ys' -> < cons 3 ys' |> mu[ ys' -> < cons 2 ys' |> k > ] > ] >
=> < (cons 3 nil) |> mu[ ys' -> < cons 2 ys' |> k > ] >
< (cons 2 (cons 3 nil)) |> k >

list_map = mu[ Ap3 f xs k -> 
                < xs |> mu[ nil -> < nil |> k > 
                        | cons x xs' -> 
                            < f |> Ap x mu[ y -> 
                                    < Ap3 f xs' mu[ ys' -> < cons y ys' |> k > ] |> list_map > ] > ] > ]

< Ap3 inc (cons 1 (cons 2 nil)) k |> list_map >
=> < inc |> Ap 1 mu[ y -> < Ap3 f (cons 2 nil) mu[ ys' -> < cons y ys' |> k > ] |> list_map > ] >
=> < 2 |> mu[ y -> < Ap3 f (cons 2 nil) mu[ ys' -> < cons y ys' |> k > ] |> list_map > ] >
=> < Ap3 inc (cons 2 nil) mu[ ys' -> < cons 2 ys' |> k > ] |> list_map >
=> < inc |> Ap 2 mu[ y -> < Ap3 f xs' mu[ ys' -> < cons y ys' |> mu[ ys' -> < cons 2 ys' |> k > ] > ] > ] >


list_map f xs :=
    match xs with
      nil
    | cons x xs' -> cons (f x) list_map f xs'

-- CPS?

list_map& f xs k :=
    match xs with
      nil -> k nil
    | cons x xs' -> list_map& f xs' (fun ys' => (k (cons (f x) ys')))

-- Not really, f is not treated as CPS form

list_map& f xs k :=
    match xs with
      nil -> k nil
    | cons x xs' -> f x (fun y => list_map& f xs' (fun ys' => (k (cons y ys'))))

-- Complete CPS, compare with it in mini-mu?

list_map = comu[ Ap3 f xs k -> 
                < xs |> 
                  mu[ nil -> < nil |> k > 
                    | cons x xs' -> 
                      < f |> Ap x mu[ y -> 
                          < list_map |> Ap3 f xs' mu[ ys' -> 
                            < cons y ys' |> k > ] > ] > ] > ]

list_fold = comu[ Ap4 cons_case nil_case xs k ]

list_filter = comu[ Ap3 xs p k ->
                  < xs |> mu[ nil -> < nil |> k >
                            | cons x xs' ->
                            < p |>
                              Ap x mu[ True ->
                                        < list_filter |> Ap3 xs' p mu[ k' -> < cons x k' |> k > ] >
                                     | False -> < list_filter |> Ap3 xs' p mu[ k' -> < k' |> k > ] > ] > ] > ]

quicksort = comu[ Ap xs k ->
  < xs |> mu[ nil -> < nil |> k >
  | cons pivot xs' ->
    < list_filter |> Ap3 xs' mu[ x -> < x |> Ap2 < |> pivot ] mu[ smaller ->
      < list_filter |> Ap3 xs' mu[ x -> < x |> Ap2 >= pivot ] mu[ greater ->
        < quicksort |> Ap2 smaller mu[ sorted_smaller ->
          < quicksort |> Ap2 greater mu[ sorted_greater ->
            < sorted_smaller |> Ap2 append mu[ pivot |> Ap2 cons sorted_greater ] k > ] > ] > ] > ] > ] > ]