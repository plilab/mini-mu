class Person {
    name,
    age,
}

```
Person = { P.Object name age k -> 
    { 
        getName -> name . k 
      | getAge -> age . k 
      | setName new_name -> P.Object new_name age . k
      | grow -> P.Object new_name { old_age -> S (old_age) } . k
    } . k }
```

k is the call site of the usage of the object instance.