# Possible Syntactic Sugar

- ```def VAR_ID [PARAMS] as / codef COVAR_ID [PARAMS] as``` keyword for Ap related co/expressions.

Examples:

```()
  codef add x y k as <=> add = mu[ Ap3 x y k -> ...]
    ...
```

```()
  def inc n k as <=> inc = comu[ Ap n k -> ...]
    ...
```

- ```pipe [PARAMS] into CO_DEFINITION```, used for generating Ap related commands.

```()
  pipe (S (S Z)) (S Z) ~Halt into add
  <=> 
  < Ap3 (S (S Z)) (S Z) ~Halt |> ~add >
```
  
- ```use DEFINITION to pump [PARAMS]```, same as above but for comus

```()
  use add to pump (S (S Z)) (S Z) ~Halt
  <=> 
  < add |> ~Ap3 (S (S Z)) (S Z) ~Halt >
```

- ```catch VAR with COVAR```, used for generating commands that is not involved with Ap.

```()
  catch y with k <=> < y |> k >
  catch x with mu[...] <=> < x |> mu[...] >
```

- ```use VAR to catch COVAR```, same as above, but for comu and cocons.

```()
  use k to catch y <=> < k |> ~y >
  use comu[...] to catch x <=> < comu[...] |> ~x >
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
    k1 = ...
    ...
    kN = ...
  <=>
  mu[ branch1 -> k1 | ... | branchN -> kN ]
```

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
      (branches
        [
          y -> pipe f xs'
            (branches
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

- ```do [BINDINGS] RETURN_VAL then CONTINUATION```, this is for a continuous squence of ```pipe```, and direct variable cases in branches.

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