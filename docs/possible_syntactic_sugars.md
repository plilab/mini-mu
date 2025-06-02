# Possible Syntactic Sugar

- ```def NAME [PARAMS] as / codef NAME [PARAMS] as``` keyword for Ap related co/expressions.

Examples:

```()
  codef add x y k <=> add = mu[ Ap3 x y k -> ...]
```

```()
  def inc n k as
    ...
```

- ```put [PARAMS] into DEFINITION```, used for Ap related commands.

```()
  put (S (S Z)) (S Z) ~Halt into add
  <=> 
  < Ap3 (S (S Z)) (S Z) ~Halt |> ~add >
```

- ```catch VAR with COVAR```, used for non-Ap-related, variable commands.

```()
  catch y with k <=> < y |> k >
```

- ```continue when VAR_NAME is [...] (where)``` for communication between expression and co-expression

```()
codef add x y k as
  continue when x is
    [
      Z -> Z_next
      S x' -> S_next 
    ]
  where
    Z_next = catch y with k
    S_next = put x' (S y) k into add 
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
  continue when x is
    [
      Z -> Z_next
      S x' -> S_next 
    ]
  where
    Z_next = catch y with k
    S_next = put x' (S y) k into add 

def main k as
  continue when k is
    [
      ~Halt -> add_something
    ]
  where
    add_something = put (S (S Z)) (S Z) ~Halt into add
```

```()
def list_map f xs k
  continue when xs is
  [
    Nil -> nil_case
    List:: x xs' -> cons_case
  ]
  where
    nil_case = catch Nil with k
    cons_case = put x y into f,
      where y =
```