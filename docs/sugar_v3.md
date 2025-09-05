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

introduce command in Expr might introduces hygienic issue.
e.g.

def list_mult xs k := 
    xs . { Nil -> 1 . k
         | List:: 0 xs' -> 0 . k
         | List:: x xs' -> (mul @ x (list_mult @ xs')) . k };
            expands to list_mult @ xs' { prod -> mul @ x prod k }

do can be further simplifed using >> or >>=
mul a b >>= inc >>= inc >>= k
this gives a*b+2.

This might be more useful when we have Partial application[ding]
cause now we can only use "functions" that require 1 arg
but if we have partial application we can have:
mul a b >>= mul c >>= pow _ 2 >>= k

This gives ((a * b) * c) ^ 2
