```
CommandConfig env store Commands(Cons "Bar" [a b c], ...)
=> ConstructorConfig env store "Bar" [a, b, c] []
=> ConstructorConfig env store' "Bar" [b, c] [1]
=> ConstructorConfig env store'' "Bar" [c] [1, 2]
=> ConstructorConfig env store''' "Bar" [] [1, 2, 3]
=> ValueConfig store''' (ConsValue "Bar" [1, 2, 3])
```

(1, 2, 3) : { it -> it . original }
CommandConfig, Ap a (Ap b c) d . MU
=> EvalConfig val_of_a { this -> Ap this (Ap b c) d . MU }
=> val_of_b {this1 -> val_of_a . { this -> Ap this (Ap this1 c) d . MU } }
