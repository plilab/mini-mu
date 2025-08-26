reify Person {
    name,
    age,
}

(Person) Person

```
Person = { P.Object name age k -> 
    { 
        getName -> name . k 
      | getAge -> age . k 
      | setName new_name -> P.Object new_name age . k
      | grow -> P.Object new_name (age + 1) . k
    } . k }
```

k is the call site of the usage of the object instance.

A object as
mu[ GetVar k1 -> do_smt . k1 | SetVar k2 -> do_smt . k2] == Object {func get_var; func set_var;}

Cons { GetVar k1 -> do_smt . k1 } { k2 ->  do_smt . k2 } ?

- What is co-Object??, mini-mu enables "buy one get one free"

