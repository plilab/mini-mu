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


* Visualization: the command is represented using whole screen, the data is represented using a central circle and codata is the context with a whole surrounding. Use html or something interactive [halfdone]

* is DFS dual to BFS (stack dual to queue) ?
- by Danvy's paper the DFS is the counterpart of shift/reset, BFS is of prompt/control

* Guarded Command Language (dijkstra's), weakest-precondition, Dijkstra Monad For Free [https://arxiv.org/pdf/1608.06499#:~:text=Dijkstra%20monads%20enable%20a%20dependent,)%2C%20and%20Ynot%20are%20built.]

[https://why-lambda.blogspot.com/2014/09/dijkstra-monads.html]

FWIW, the Dijkstra monad is nothing more than the continuized state monad. It's well know that there is an isomorphism

a -> b    ~    forall r. (b -> r) -> a -> r
and anyone paying close enough attention will notice that this is just flipped function composition:

fwd :: (a -> b) -> forall r. (b -> r) -> a -> r
fwd f g x = g (f x)

bwd :: (forall r. (b -> r) -> a -> r) -> a -> b
bwd h = h id
(Other ken observers will then naturally yell "Yoneda!")

The forward direction is also know as continuization, wherein you turn a function into a continuation, where instead of transforming an a into a b, it transforms a b continuation into an a continuation. This is the basis of van Laarhoeven lenses, incidentally.

The Dijkstra monad, defined as

data Dijkstra s a = Dijkstra { runDijkstra :: forall r. ((a,s) -> r) -> s -> r }
is obviously just a special case of this more general continuized form of functions, when applied to state transformers. The monadicity of Dijkstra s is then completely trivial, since Dijkstra a is equivalent to State s, a well-established monad. We could readily define the Dijkstra s monad instance in terms of the state monad instance because of the isomorphism.


pair := Pair a b
pair_lazy := { Fst k -> a . k | Snd k -> b . k }

swap = { (p, k) -> p . { (y, z) -> (z, y) . k } }
swap_lazy = { (p_l, k0) -> { Fst k -> Snd k . p_l | Snd k -> Fst k . p_l } . k0 }

much clearer than: def swap_lazy(𝑥; 𝛼)≔ ⟨cocase {fst(𝛽)⇒⟨𝑥 |snd(𝛽)⟩,snd(𝛽)⇒⟨𝑥 |fst(𝛽)⟩}|𝛼⟩

swap @ (2, 3) k => (3, 2) . k
{ Fst k -> 2 . k | Snd k -> 3 . k } k0 @ swap_lazy => 
{ Fst k -> Snd k . { Fst k -> 2 . k | Snd k -> 3 . k } 
| Snd k -> Fst k . { Fst k -> 2 . k | Snd k -> 3 . k } } . k0

much cleaner and unified in minimu than in lambda mu calculus
