# Possible Syntactic Sugar

- ```def VAR_ID [PARAMS] as / codef COVAR_ID [PARAMS] as``` keyword for Ap related co/expressions.

Examples:

```()
  codef add x y k = <=> add = mu[ Ap3 x y k -> ...]
    ...
```

```()
  def inc n k = <=> inc = comu[ Ap n k -> ...]
    ...
```

Ap [PARAMS], CO_DEFINITION

- ```use [PARAMS] call CO_DEFINITION```, used for generating Ap related commands.

```()
  use (S (S Z)) (S Z) ~Halt call add
  <=> 
  < Ap3 (S (S Z)) (S Z) ~Halt |> ~add >
```

(S (S Z)) (S Z) `Ap3` ~add


f @ x y z k1 k2 k3 => f, Ap x y z k1 k2 k3

Ap3 x y z k1 k2 k3
Ap3 x y k1 k2 k3 k4

mu [ Ap x y z k1 k2 k3

Z `S` inc


Ap3 Z Z ~Halt, ~add

def foo = bar;
def baz = quux;
main
  
- ```call DEFINITION with [PARAMS]```, same as above but for comus

```()
  call add with (S (S Z)) (S Z) ~Halt
  <=> 
  < add |> ~Ap3 (S (S Z)) (S Z) ~Halt >
```

- ```pipe VAR into COVAR```, used for generating commands that is not involved with Ap.

```()
  pipe y into k <=> < y |> k >
  pipe x into mu[...] <=> < x |> mu[...] >

  pipe (Ap3 2 1 Halt) into add <=> use Ap3 2 1 Halt call add
```

- ```use VAR pump COVAR```, same as above, but for comu and cocons.

```()
  use k pump y <=> < k |> ~y >
  use comu[...] pump x <=> < comu[...] |> ~x >
```

- ```branches [...] (where) / origins [...] (where)``` for creating mu / comu expressions, when pattern matching is involved.

Examples:

```()
  branches
  [
    branch1 -> k1
    ...
    branchN -> kN
  ]
  where
    k1 = ...< .. >
    ...
    kN = ...
  <=>
  mu[ branch1 -> k1 | ... | branchN -> kN ]
```

  case [ branch1 -> k1 ... branchN -> kN ]
  mu [ branch1 -> k1 | ... | branchN -> kN ]


case x of Foo -> ...

mu [ foo -> ... | ... | ...]
mu [
  foo -> ...;
  ...;
  ...]

x, [ Foo -> ...]


mu [ Ap x k ->
  ....
  x, k
  ....
  k, x
  ....
]


def x y z k = f, Ap { k1 -> g, Ap2 { k2 -> h, Ap1 x k2} k1} k
def x y z k = f, Ap [ k1 -> g, Ap2 [ k2 -> h, Ap1 x k2] k1] k
def x y z k = f, Ap case [ k1 -> g, Ap2 case [ k2 -> h, Ap1 x k2] k1] k
def x y z k = f, Ap [ mu k1 -> g, Ap2 [ mu k2 -> h, Ap1 x k2] k1] k

def x y z k = f, Ap (mu [ k1 -> g, Ap2 (mu [ k2 -> h, Ap1 x k2]) k1]) k
f (g (h x))

do h' <- h @ x
   g' <- g @ h'
   f' <- f @ h'
[f [g [h x]]]

f @ [k1 -> (g (h x))

f @ x y z k1 k2 k3 => f, Ap [k0 -> x] y z k1 k2 k3

Ap <=> Tp

```()
  origins
  [
    branch1 -> k1
    ...
    branchN -> kN
  ]
  where
    k1 = ...
    ...
    kN = ...
  <=>
  comu[ branch1 -> k1 | ... | branchN -> kN ]
```

```()
codef add x y k as
  catch x with branches
    [
      Z -> Z_next
      S x' -> S_next
    ]
  where
    Z_next = catch y with k
    S_next = pipe x' (S y) k into add

def add x y k as
  catch x with branches
    [
      Z -> Z_next
      S x' -> S_next
    ]
  where
    Z_next = catch y with k
    S_next = use add to pump x' (S y) k 
```

(This would require the Env to contain definition of commands: DON'T forget to capture the variables introduced in pattern matching)

- Automatic main expansion if not specified.

```()
  pipe (S (S Z)) (S Z) ~Halt into add 
  =>
  main = comu[ ~Halt -> < Ap3 (S (S Z)) (S Z) ~Halt |> ~add > ]
```

let x = ...; y = ...; in ...

type ...
def x a b = ...
def y d = ... exit ...
run x, y


(Probably not a good idea)

- Sugared programs:

```()
codef add x y k as
  catch x with branches
    [
      Z -> Z_next
      S x' -> S_next
    ]
  where
    Z_next = catch y with k
    S_next = pipe x' (S y) k into add 

def main k as
  catch k with branches
    [
      ~Halt -> add_something
    ]
  where
    add_something = pipe (S (S Z)) (S Z) ~Halt into add
```

```()
def list_map f xs k
  use origins
  [
    Nil -> nil_case
    List:: x xs' -> cons_case
  ] to catch xs
  where
    nil_case = catch Nil with k
    cons_case = pipe x
      (mu
        [
          y -> pipe f xs'
            (mu
              [
                ys' -> catch (List:: y ys') with k
              ]) into list_map
        ]) into f
```

In above case there are a continuous sequence of pipe and branches, and further the branches have only one variable case. In this case maybe we can simplify as this.

```()
do
  y <- f x
  ys' <- list_map f xs'
  List:: y ys'
then k ?
```

mu [it -> ...] <=> {...}

[y: ...] (probably bad idea)

```()
def list_map f xs k
  catch xs with branches
  [
    Nil -> nil_case
    List:: x xs' -> cons_case
  ]
  where
    nil_case = catch Nil with k
    cons_case = 
      do
        y <- f x
        ys' <- list_map f xs'
        List:: y ys'
      then k 
```

- ```do [BINDINGS] RETURN_VAL then CONTINUATION```, this is for a continuous sequence of ```call```, and direct variable cases in branches.

```()
  do
    a <- cont1 var11 var12
    b <- cont2 var21 ...
    c <- cont3 var31 ...
    val
  then k

  <=>

  pipe var11 var12 
    branches
    [
      a -> pipe var21 
        branches
        [
          b -> pipe var31
            catch val with k
          into cont3
        ]
      into cont2
    ]
  into cont1

  do
    a <- cont1 var11 var12
  then ...

  Ap var11 var12 (mu [_ a b -> ...]), cont1

  f (g x) (h y)

  <
    y' z <- h y;
    x' <- g x;
    z' <- f x' y';
    z'
  >
  begin
    y' z <- h y;
    x' <- g x;
    z' <- f x' y';
    z'
  end

  seq
    y' z <- h y
    x' <- g x
    z' <- f x' y'
    z'
  then k

  h, Ap y [y' ->
    g, Ap x [x' ->
      f @ x' y' k
    ]
  ] <=>
  do
    y' <- h y
    x' <- g x
    f ...x'...
  <=>
    h @ y (y' ->
    g @ x (x' ->
    f ...x'...))

  mu [ x -> ...]
  x -< ...
  x -< ... | Ap y z -< ...
  (mu Ap x -> ... | Ap y z -> ...)

  Ap var11 var12 (mu [
      a -> Ap var21 (mu [
          b -> Ap var31 mu[ val -> (val, k) ] , cont3
        ])
      , cont2
    ])
  , cont1
```

```()
  do
    a <- cont1 var11 var12
    b <- cont2 var21 ...
    c <- cont3 var31 ...
    val
  then k

  <=>

  pipe var11 var12 
    branches
    [
      a -> pipe var21 
        branches
        [
          b -> pipe var31
            catch val with k
          into cont3
        ]
      into cont2
    ]
  into cont1
```

**This is somehow inspired by some imperative nature of the process, imperative programs have some current computation steps and also A SINGLE possibility of continuation, given no control operators**

?? list_filter with cpr

list_filter  = comu[ ~Ap3 pred xs ~k ->
  < xs |> mu[ Nil -> < Nil |> ~k >
            | List:: x xs' -> 
              < pred |> ~Ap x mu[ True -> 
                                  < list_filter |> ~Ap3 pred xs' mu[ ys'-> < List:: x ys' |> ~k > ] > 
                                  < comu[ ~Ap4 pred xs cpr ~k -> < list_filter |> ~Ap4 pred xs' cpr mu[ ys'-> < List:: x ys' |> ~k > ] > ] |> ~Ap4 ... >
                                     | False ->
                                  < list_filter |> ~Ap3 pred xs' mu[ ys' -> < ys' |> ~k > ] > ] > ] > ];

list_filter_with_cpr = comu[ ~Ap4 pred xs cpr ~k -> < list_filter |> ~Ap3 pred xs ~k >]

list_filter "with" cpr

-----------------------

Unify Expr and CoExpr

comma
let (expr and commands)
begin/end
def/run
@
Ap

Later: idiom brackets
