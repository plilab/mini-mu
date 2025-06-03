# Possible Syntactic Sugar

- ```def NAME [PARAMS] as / codef NAME [PARAMS] as``` keyword for Ap related co/expressions.

Examples:

```()
  codef add x y k as <=> add = mu[ Ap3 x y k -> ...]
```

```()
  def inc n k as
    ...
```

- ```pipe [PARAMS] into DEFINITION```, used for generating Ap related commands.

```()
  pipe (S (S Z)) (S Z) ~Halt into add
  <=> 
  < Ap3 (S (S Z)) (S Z) ~Halt |> ~add >
```

- ```catch VAR with COVAR```, used for generating commands that is not involved with Ap.

```()
  catch y with k <=> < y |> k >
  catch x with mu[...] <=> < x |> mu[...] >
```

- ```branches [...] (where)``` for creating mu expressions, when pattern matching is involved.

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
codef add x y k as
  catch x with branches
    [
      Z -> Z_next
      S x' -> S_next
    ]
  where
    Z_next = catch y with k
    S_next = pipe x' (S y) k into add 
```

(This would require the Env to contain definition of commands: DON'T forget to capture the variables introduced in pattern matching)

- Automatic main expansion if not specified.

```()
  put (S (S Z)) (S Z) ~Halt into add 
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
  catch xs with branches
  [
    Nil -> nil_case
    List:: x xs' -> cons_case
  ]
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