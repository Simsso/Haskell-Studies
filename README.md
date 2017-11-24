# Haskell Programming

## 1. Introduction

A **function** maps from its _domain_ to its _image_ (which is a subset of the _codomain_). Each input is invariably mapped to exactly one output.

In **lambda calculus** an _abstraction_ is an anonymous function. It consists of _head_ and _body_, for example _λx.x_. The head binds the parameter(s) to the body of the function. 

The lambdas _λx.x_ and _λy.y_ are **alpha equivalent**.

**Beta reduction** is the process of replacing all occurences of a parameter with a value or a function; for example _(λx.x+x)1_ becomes _1_ or _(λx.x)(λa.2a)_ turns into _(λa.2a)_. 

If a variable occurs in a function's body but not in the head it is refered to as a **free variable**. Lambdas with multiple arguments such as _λxy.xy_ are a shorthand for multiple nested lambdas _λx.(λy.xy)_.

**Combinators** are lambda terms with no free variables. 

Lambda terms can **diverge** if _evaluation_ does not terminate. For example _λx.xx_ diverges if applied to itself. Evaluation happens in _normal order_, i.e. outer-most and left-most terms get evaluated first.

Notes on syntax: _λab.a(b)_ means that _b_ will be applied to _a_ on evaluation (if possible). However, _(λa.λb.a)b_ evaluates to _λb.b'_.

# 2. Getting Started
**Prelude** is a library of standard types, classes, and functions, such as `pi`, `Bool`, `Monad`, `map`. Haskell files can be loaded to GHCi REPL using `:load file.hs`. 

An _expression_ is in **normal form**, or **irreducible**, when there are no more evaluations steps that can be taken.

Every **Haskell function** is an expression that takes one argument. They always return a result. A **definition** may look like that: `piTimesSquare x = pi * (x ^ 2)`. A function **parameter** stands for a value, while an **argument** is an actual value. Functions are in _prefix_ style by default.

_Infix_ operators are functions that can be used in prefix fashion by wrapping them in parantheses: `(+) 1 2`. The `$` operator has the lowest possible precedence. The following example explains its usage: `(5 *) $ 1 + 1` equals `5 * (1 + 1)`. The GHCi command `:info` provides signature and precedence information about functions.

An **expression** is a combination of symbols that conforms to syntactic rules and can be evaluated to some result. 

A **value** is an expression that can not be evaluated any further. Haskell uses **lazy evaluation**, i.e. it only evaluates an expression when it is forced to by other terms which refer to the expession.

# 3. Strings
The GHCi command `:type` prints the type of a variable / expression. `a :: b` means that `a` has the type `b`.

`String` is a type alias for `[Char]`, i.e. a list of characters.

For outputting variables `print` can be used. `putStr` and `putStrLn` are also printing, however, they are restricted to the type `String`.

The `do` syntax allows sequencing of actions, as shown below.
```haskell
a :: String  -- declaration with type
a = "a"  -- value assignment

main = do
  putStr a
  putStrLn "b"
````

Strings can be **concatenated** with the infix operator `++` or the `concat` function (e.g. `concat ["a", "b"]`).

Functions and types can be defined globally (**top level definitions**) or locally (**local definition**); the _scope_ is different. The `were` and `let` clauses are key to defining local functions or variables, as can be seen in the example below.
```haskell
area d = pi * (r * r)  -- top level
  where r = d / 2  -- local
```

The `:` operator builds a list: `'a' : "bc"`. The functions `head` and `tail` can be applied to strings in order to retrieve the first character (**head**) or everything but the first character (**tail**). A **substring** starting at index 0 can be retrieved using `take`: `take n string`. It will return a list containing the first `n` elements of the list (which can be a `String`). Contrary, `drop` removes the first `n` elements from a list.

```haskell
-- sub list with length l of list x starting at index s
sublst :: Int -> Int -> [a] -> [a]
sublst s l x = take l (drop s x)
```

The **`n` th element** of a list can be accessed with the `!!` operator (`n` is zero based): ``"abc" !! n``.