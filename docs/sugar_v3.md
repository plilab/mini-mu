label k in
def list_mult xs := 
    xs . { Nil -> 1 ...
         | List:: 0 xs' -> 0 ...
         | List:: x xs' ->
            do rest <- list_mult xs',
                acc <- mul x rest,
            then acc ... }; 

label might resolve the necessity on the continuation variable for function decl
use ... to bind to the nearest label

An idea: Let's keep using Ap as a special constructor, in which case we can enforce it to have a continuation, to distinguish it from normal tuples.

allow constructor to register a contract? where a contract defines what's inside.

introduce command in Expr might introduces hygienic issue.
e.g.

def list_mult xs k := 
    xs . { Nil -> 1 . k
         | List:: 0 xs' -> 0 . k
         | List:: x xs' -> (mul @ x (list_mult @ xs')) . k }; mul @ x {list_mult @ xs' ~1}
            expands to list_mult @ xs' { prod -> mul @ x prod k } , mul @ x { k -> list_mult @ xs' k }

do can be further simplifed using >> or >>=
mul a b >>= inc >>= inc >>= k
this gives a*b+2.
add . 2 3 >>= { a -> add 3 4 >>= {b -> add a b} }
{ k -> add 2 3 k } >>= { a -> { k -> add 3 4 k } >>= { b -> { k -> add a b k } } }
if there is a bind op as a function not a sugar?

a <- add . 2 3
_ <- add . 3 4 
_ <- add 

{ add @ 2 3 { a -> add @ 3 4 { b -> add @ a b k } } }

This might be more useful when we have Partial application[ding]
cause now we can only use "functions" that require 1 arg
but if we have partial application we can have:
mul a b >>= mul c >>= pow _ 2 >>= k

This gives ((a * b) * c) ^ 2

It seems partial application is not a very easy job to do in mini-mu
As the constructor has atomicity: you cannot divide Ap a b c to Ap a -> Ap b -> Ap c, without introducing new continuations: like
{ Ap a b k -> C } will become { Ap a -> { Ap b -> { Ap k -> C } . k^1 } . k^0 }
we have to bind k^0, k^1 to some continuations automatically, which leds the partial application more a "macro-like" behavior.

random question: what will be co-currying, i.e. currying a co-data?

```()
    *[add @ 3] @ *[add @ 1 2] Halt     --- this0 @ this1 Halt
```

this0 @ *[ add @ 1 2 ] halt

this0, this1 are auto-generated variables where

this0 = { this0 -> this0 @ *[ add @ 1 2 ] Halt }

this1 = { res1 -> [ add @ 3 ] @ res1 Halt }
[ add @ 3 ]
== { Ap b k -> add @ 3 b k } . this0

We also need the ability to apply the second argument solely. Like this:
[ add @ _ 3]
== { Ap a k -> add @ a 3 k } . this0

[ add @ 1 2]
== { Ap k -> add @ 1 2 k } . this1

now an interesting thing is that we need to supply a implicit continuation to [ add 1 2 ],
but which? maybe call it "here", and here refer to "this" of that place. in this case this1

now lets make the flexibility for the notation for adding implicit continuation, to add *[  ].

now, *[ add @ 1 2 ] === [ add @ 1 2 here ]
=== { Ap here -> 1 . { Z -> 2 . here | S x' -> add x' 3 here } } . this1
which reduce to 3 . here
which is 3 . { res1 -> [ add @ 3 ] @ res1 Halt }
which is [ add @ 3 ] @ 3 Halt

now the whole thing will be (if deal from left to right)
=> { Ap b k -> 3 . { Z -> b . k | S x' -> add x' S(b) k } } . self0
=> { Ap b k -> 3 . { Z -> b . k | S x' -> add x' S(b) k } } @ &[ add 1 2] Halt

notice that now "here" for add 1 2 is not same as above but:
{ res -> { Ap b k -> 3 . { Z -> b . k | S x' -> add x' S(b) k } } @ res Halt }

now when we do *[ add @ 1 2 ]:
we have 3 . { res -> { Ap b k -> 3 . { Z -> b . k | S x' -> add x' S(b) k } } @ res Halt }
=> { Ap b k -> 3 . { Z -> b . k | S x' -> add x' S(b) k } } @ 3 Halt
=> 6 . Halt
=> 6
ok perfectly done!

small-step semantics for idiom:

```
evalExprWithCtx ctx (Idiom (e, ce)) =
    CommandConfigWithCtx e ce ctx    

step CommandConfigWithCtx p c ctx = 
    ValueConfigWithCtx (evalExprWithCtx c p) (evalWithCtx p c) ctx
step ValueConfigWithCtx pv cv ctx = 
    matchWithCtx p c ctx

matchWithCtx p c ctx = match p and c somehow and = CommandConfig matched_result ctx
```

[add @ 3 _ _] @ 1 halt

e = [add @ 3 _ _]
ce = Ap 1 halt

=> CommandConfigWithCtx e.fst e.snd ce
=> ValueConfigWithCtx add 3 (Ap 1 halt)
=> match result of add and 3 is { Ap b k -> add @ 3 b k }
=> CommandConfig {Ap b k -> add @ 3 b k } . (Ap 1 halt)

v = { Ap b k -> 3 b k @ add }
cv = 

Guess: in this way we can do all syntatic sugars
