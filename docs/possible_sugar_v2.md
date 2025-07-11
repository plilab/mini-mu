-- TODO: sugar idea: { ... } or [ ... ] ==> [ it -> ...]
-- { x -> x + 1} === { it + 1 }
-- xs.map { it + 1 }
-- xs.map { x -> x + 1 }

-- TODO: alternate notation idea:
--   [ x: ...]
--   [ Z: ... | S x: ... ]

-- TODO: design pattern for: x y z @ [ a b c -> ...]
-- TODO: infix constructors


-- f x y >>= proc1 >>= proc2 >>= cont then k
=== do t1 <- f x y, t2 <- process t1 then t2 k @ cont