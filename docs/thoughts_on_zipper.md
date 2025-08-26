Hazel environment:

- Hazel is bidirectionally typed: for the hole, it uses check, check with expectation.
for the context it synthesis the type.

- question: is bidirectional type checking implies duality? or is downward check is dual to upward synth?

- The edit state in Hazelnut is a Z-expression, Every Z-expression has a single H-expression, or H-type, τ, under the cursor. Z-expression has its name from Zipper. 
So natrually we got correspondance from our Command to Z-expression.
Edit state in Hazel = Command
The cursor in Hazel = Expression
The rest of the cursor = Co-expression

The rest of the cursor comes from the derivatives of ??? the context might not only have one hole

Figure 1 in Command

Empty . Empty (contruct lambda)
Empty . Lambda (construct num)
num . Lambda (move to parent) 


represented using specialization
Lambda1 = \x -> [] : [] -> []

Lambda = mu{ H-expr -> Lambda1 [] H-expr [] }
