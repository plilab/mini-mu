# Ideas

- Background Markdown
- Create folder of example .mmu
- Move files into docs folder
- Allow functions to return commands?

## Goals

- Implement a nice language
- Discover how to use the language well

- Interpreter
- Improve Syntax
  - Fewer brackets
  - Be more pretty
  - Look at Haskell or OCaml
  - No have to go through command within each mu. E.g., by designating a current continuation or current expression
- Syntactic Sugar
- Develop Examples
  - Large example (e.g., a parser)
  - Want to discover what idioms to use
    - In particular find ways that the duality is actually an advantage
      - Example, map and fold can be written in accumulator style or not
- Letrec
- Debugger
- GraphViz to see evaluation
- Side Effects (but that might be a bad idea)
- Look at Duo for syntax examples
- Protocol implementations
- Multi-thread webserver (combine concurrency with protocols)
- Static Type checker
- Declare data types?
  - Does this matter if we don't have a type checker?
- Automated testing system

* mu's type is the derivative type of the evaluation tree? cos The Derivative of a Regular Type is its Type of One-Hole Contexts. the evaluation tree is a regular type and mu is a sum of one-hole context of that type
Zipper = (things under focus, context(continuation))
command = < data, codata >
how to handle zippers when cycle exists

List = Nil | Cons Nat List
List = 1 + Nat * List
d List / d List = Nat
d List / d Nat = List

define json as 

key::string : value::List
key::string : value::List
key::string : value::List
...

Json = Null | Field String List Json
Json = 1 + String * List * Json

d Json / d String = List * Json
d Json / d Nat = 
  (d String / d Nat) * List * Json +
  (d List / d Nat) * String * Json +
  (d Json / d Nat) * String * List
= List * String * Json

explanation: the context where Nat can occur  

* mu can model oop [https://www.cs.uoregon.edu/Reports/DRP-201905-Sullivan.pdf]: class as Constructor and Method as Mu

struct Foo {
  ...
  var
}

Foo = Foo1 var1 var2

func (Foo) var_rep {
  return var
}

func (Foo) some {
  ...
}

mu[Foo a b -> ..] // missing for polymophism. the method should depends on instance

mu[ GetVar k1 -> do_smt . k1 | SetVar k2 -> do_smt . k2] == Object {func get_var; func set_var;}

Cons { k1 -> do_smt . k1 } { k2 ->  do_smt . k2 } ?

* Visualization: the command is represented using whole screen, the data is represented using a central circle and codata is the context with a whole surrounding. Use html or something interactive 

* is DFS dual to BFS (stack dual to queue) ?