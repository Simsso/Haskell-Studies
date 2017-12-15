# Haskell Programming Book Notes

## 1 Introduction

A **function** maps from its _domain_ to its _image_ (which is a subset of the _codomain_). Each input is invariably mapped to exactly one output.

In **lambda calculus** an _abstraction_ is an anonymous function. It consists of _head_ and _body_, for example _λx.x_. The head binds the parameter(s) to the body of the function. 

The lambdas _λx.x_ and _λy.y_ are **alpha equivalent**.

**Beta reduction** is the process of replacing all occurences of a parameter with a value or a function; for example _(λx.x+x)1_ becomes _1_ or _(λx.x)(λa.2a)_ turns into _(λa.2a)_. 

If a variable occurs in a function's body but not in the head it is refered to as a **free variable**. Lambdas with multiple arguments such as _λxy.xy_ are a shorthand for multiple nested lambdas _λx.(λy.xy)_.

**Combinators** are lambda terms with no free variables. 

Lambda terms can **diverge** if _evaluation_ does not terminate. For example _λx.xx_ diverges if applied to itself. Evaluation happens in _normal order_, i.e. outer-most and left-most terms get evaluated first.

Notes on syntax: _λab.a(b)_ means that _b_ will be applied to _a_ on evaluation (if possible). However, _(λa.λb.a)b_ evaluates to _λb.b'_.

## 2 Getting Started
**Prelude** is a library of standard types, classes, and functions, such as `pi`, `Bool`, `Monad`, `map`. Haskell files can be loaded to GHCi REPL using `:load file.hs`. 

An _expression_ is in **normal form**, or **irreducible**, when there are no more evaluations steps that can be taken.

Every **Haskell function** is an expression that takes one argument. They always return a result. A **definition** may look like that: `piTimesSquare x = pi * (x ^ 2)`. A function **parameter** stands for a value, while an **argument** is an actual value. Functions are in _prefix_ style by default.

_Infix_ operators are functions that can be used in prefix fashion by wrapping them in parantheses: `(+) 1 2`. The `$` operator has the lowest possible precedence (0). The following example explains its usage: `(5 *) $ 1 + 1` equals `5 * (1 + 1)`. The GHCi command `:info` provides signature and precedence information about functions.

An **expression** is a combination of symbols that conforms to syntactic rules and can be evaluated to some result. 

A **value** is an expression that can not be evaluated any further. Haskell uses **lazy evaluation**, i.e. it only evaluates an expression when it is forced to by other terms which refer to the expession.

## 3 Strings
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

## 4 Basic Data Types
A **data type** is a set of _values_ with an abstract commonality. A **data declaration** defines a new data type. For example, the data type `Bool` is defined with the following _data declaration_.
```haskell
data Bool = False | True
```

**Pattern matching** is a feature of Haskell that allows multiple implementations of the same function. When calling the function, the implementation will be chosen depending on the argument. `_` is called catch-all and will match any argument value.

**Typeclasses** is a polymorphic type that adds functionality (i.e. faculties or interfaces) to types that is reusable across all inheriting types. A **type alias** is a way of making a type available through a differnt name: `type Name = Integer`.

Haskell's inequality symbol is `/=`. The **ordering typeclass** `Ord` enforces implementation of the following operators.
```haskell
compare :: a -> a -> Ordering
(<) :: a -> a -> Bool
(<=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(>=) :: a -> a -> Bool
max :: a -> a -> a
min :: a -> a -> a
```

The **equality typeclass** `Eq` requires the following.
```haskell
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

A **typeclass constraint** can be made for parameters with the following syntax (here for the function equality operator which requires both operands to implement `Eq`): `(==) :: Eq a => a -> a -> Bool`.

Variables in type signatures are commonly named according to the following rules: (1) Type variables are called `a`, `b`, ...; (2) function variables are called `f`, `g`, ... (3) Arguments to functions are often called `x`, `y`, and `z`. (4) Lists of `x` values are called `xs`. (5) All names can also occur with numbers or the prime symbol appended to them, e.e. `x1` or `f'`.

### 4.1 Numbers
Numbers are inheriting from the _typeclass_ `Num`. 
* **`Int`**. An integral number (aka. integer) with a fixed precision, that is it has upper and lower bound (size: 8 byte). `GHC.Int` adds the integer types `Int8`, `Int16`, `Int32`, and `Int64`, with the number indicating the number of bits. The value range of `Int` is _[-9223372036854775808, 9223372036854775807]_.
* **`Integer`**. An integral number that supports arbitrarily large or small numbers.
* **`Float`**. Single-precision floating point number (size: 4 byte).
* **`Double`**. Double-precision floating point number (size: 8 byte).
* **`Rational`**. Represents a fraction of two integer numbers. The data type wraps two `Integer`s and is hence arbitrarilty precise.
* **`Scientific`**. Floating point number with an `Integer` base and `Int` exponent. Therefore, the numbers can be arbitrarily large and precise. This data type is not part of GHC and must be installed separately (`stack install scientific`).

The `Integer` type should be preferred over `Int`, and `Scientific` and `Rational` (typeclass `Fractional`) should be preferred over `Float` and `Double`, unless computational efficiency is a factor.

### 4.2 Boolean
The boolean data type can either be `True` or `False` and is defined as `data Bool = False | True`. Operators for booleans are `&&` for **and**, `||` for **or**, and the function `not` for **inversion**.

Haskell features **if expressions** with the following syntax: `if <condition> then <a> else <b>`. The entire if expression evaluates to either `<a>` or `<b>`, depending on the condition.

### 4.3 Tuples
**Tuples** are types that store a fixed number _n_ of constituents which may have different types themselfes. _n_ is refered to as _arity_ (numer of parameters that a function takes). A tuple can be created with its constructor, `(,,) x1 x2 x3`, here with _n=3_. Tuples with _n=1_ must not exist, however, _n=0_ is possible and called _unit_ `()`.

For convenience, the first element in a tuple can be accessed using `fst :: (a, b) -> a`; `snd` serves equally for the second value. `Data.Tuple` contains the tuple manipulation functions `curry`, `uncurry`, and `swap`. 

A tuple can be unpacked when passed to a function with the following syntax: `tupleSum (a, b) = a + b`

### 4.4 Lists
The **list** data type stores _n_ values of equal type, where _n_ can be changed dynamically.

The **`n` th element** of a list can be accessed with the `!!` operator (`n` is zero based): ``"abc" !! n``.

## 5 Types
Type systems have been defined to enforce correctness. In Haskell, typing is _static_ and typechecking occurs at _compile time_. A **data type declaration** defines a _type constructor_ and _data constructors_. Haskell functions are created from the function type constructor `->` and the function is a _value_.

A function signature may have **multiple typeclass constraints** `(Num a, Num b) => a -> b -> b`. In the example, `a` could be an `Integer` and both `b` could be `Double`s. However, different types for the second argument and the return type would not be possible. 

The `=>` is called **typeclass arrow**. The right associative **type constructor for functions** `->` realizes currying: `f :: a -> a -> a` is read as `f :: a -> (a -> a)`. Due to currying, functions can be partially applied. Infix operators can be partially applied to a first or second parameter, e.g. `(2^)` or `(^2)`.

**Polymorphism** is the provision of a single interface to entities of different types. In Haskell it is either _parametric_ or _constrained_ (aka. _bounded_, _ad-hoc_). The former is polymorphism that accepts any type, whereas the latter accepts only some types. Multiple class constrains must be wrapped in parentheses: `f :: (Eq a, Num b) => a -> b`. The opposite of polymorphism is _monomorphism_, in Haskell called _concrete_. Applied to variables, polymorphism is a property of variables which may refer to more than one concrete type.

**Type inference** is the process of determining a variables _principle type_ by looking at the way it is being used. The **principle type** is the most generic type that can be assigned to a variable.


## 6 Typeclasses
**Typeclasses** generalize over a set of types in terms of comsumption or usage in computation. After declaring a data type with the `data Typename` keyword, typeclasses can be assigned with e.g. `instance Typeclass Typename`. Typeclasses can **inherit** from a _superclass_ (e.g. `class Num a => Fractional a`).

A typeclass can be defined with the `class` keyword. The `Num` typeclass for example is defined as follows.
```haskell
class Num a where
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
```
Types which implement the `Integral` type are required to also implement `Real` and `Enum`.
```haskell
class (Real a, Enum a) => Integral a
```

A typeclass is implemented in a type with `instance`. The implementation is called **instance** and might look as follows.
```haskell
data Suit = Spade | Diamond | Club | Heart
instance Eq Suit where
  (==) Spade Spade = True
  (==) Diamond Diamond = True
  (==) Club Club = True
  (==) Heart Heart = True
  (==) _ _ = False
```

Typeclasses **default** to certain types. `Num` defaults to `Integer` for example `default Num Integer`.

**Typeclass instances** are unique pairings of a typeclass and a type.

**Effects** are observable actions programs may take, such as writing to a file or printing to the console. `IO` is the type for values whose evaluation bears the possibility of causing side effects.

### 6.1 Derivable Typeclasses
The following typeclasses can be automatically derived. That means they can be automatically instantiated for a given type, based on how it is defined.

 * **`Bounded`**. Types that have an upper and lower bound.
 * **`Enum`**. The type's values can be enumerated. Provides methods such as `succ` (successor), `pred` (predecessor), `enumFromTo`, and `enumFromThenTo` (which uses a step size based on the second argument). 
 * **`Eq`**. The type's values can be tested for equality.
 * **`Ord`**. The type's values can be put into sequential order. Implies `Eq` and can be implemented by defining the `compare` method which returns `EQ`, `LT`, or `GT`.
 * **`Read`**. Values can be parsed from strings. It is often a _partial_ function as it does not return a proper value for all possible inputs.
 * **`Show`**. Values can be converted to strings (e.g. for output). Enforces implementation of `showsPrec`, `show`, and `showList`. Printing things is possible in Haskell, even though it is purely functional, because the `print` method invokes `IO` which has the _side effect_ of outputting text. It returns the unit `()` because it has no relevant return value.

 ### 6.2 Typeclass Inheritance
 Inheritance structure of common typeclasses. `Ord` inherits from `Eq`. `Real` inherits from `Ord` and `Num`. `Fractional` inherits from `Num`. `Integral` inherits from `Real`, `Fractional`, and `Enum`.