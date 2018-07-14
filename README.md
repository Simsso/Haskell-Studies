# Haskell Programming Book Notes

# 1 Introduction
A **function** maps from its _domain_ to its _image_ (which is a subset of the _co-domain_). Each input is invariably mapped to exactly one output.

In **lambda calculus** an _abstraction_ is an anonymous function. It consists of _head_ and _body_, for example _λx.x_. The head binds the parameter(s) to the body of the function. 

The lambdas _λx.x_ and _λy.y_ are **alpha equivalent**.

**Beta reduction** is the process of replacing all occurrences of a parameter with a value or a function; for example _(λx.x+x)1_ becomes _1_ or _(λx.x)(λa.2a)_ turns into _(λa.2a)_. 

If a variable occurs in a function's body, but not in the head, it is referred to as a **free variable**. Lambdas with multiple arguments such as _λxy.xy_ are a shorthand for multiple nested lambdas _λx.(λy.xy)_.

**Combinators** are lambda terms with no free variables. 

Lambda terms can **diverge** if _evaluation_ does not terminate. For example _λx.xx_ diverges if applied to itself. Evaluation happens in _normal order_, i.e. outer-most and left-most terms get evaluated first.

Notes on syntax: _λab.a(b)_ means that _b_ will be applied to _a_ on evaluation (if possible). However, _(λa.λb.a)b_ evaluates to _λb.b'_.

# 2 Getting Started
**Prelude** is a library of standard types, classes, and functions, such as `pi`, `Bool`, `Monad`, `map`. Haskell files can be loaded to GHCi REPL using `:load file.hs`. All compiler warnings can be enabled with `-Wall` (or equivalently `{-# OPTIONS_GHC -Wall #-}`). 

An _expression_ is in **normal form**, or **irreducible**, when there are no more evaluations steps that can be taken.

Every **Haskell function** is an expression that takes one argument. They always return a result. A **definition** may look like that: `piTimesSquare x = pi * (x ^ 2)`. A function **parameter** stands for a value, while an **argument** is an actual value that is being passed on to the function. Functions are in _prefix_ style by default.

_Infix_ operators are functions that can be used in prefix fashion by wrapping them in parentheses: `(+) 1 2`. The `$` operator has the lowest possible precedence (0). The following example explains its usage: `(5 *) $ 1 + 1` equals `5 * (1 + 1)`. The GHCi command `info` provides signature and precedence information about functions.

An **expression** is a combination of symbols that conforms to syntactic rules and can be evaluated to some result. 

A **value** is an expression that can not be evaluated any further. Haskell uses **lazy evaluation**, i.e. it only evaluates an expression when it is forced to by other terms which refer to the expression.

# 3 Strings
The GHCi command `:type` prints the type of a variable / expression. `a :: b` means that `a` has the type `b`.

`String` is a type alias for `[Char]`, i.e. a list of characters.

For outputting variables, `print` can be used. `putStr` and `putStrLn` are also printing, however, they are restricted to the type `String`.

The `do` syntax allows for sequencing of actions, as shown below.
```haskell
a :: String  -- declaration with type
a = "a"  -- value assignment

main :: IO ()
main = do
  putStr a
  putStrLn "b"
```

Strings can be **concatenated** with the infix operator `++` or the `concat` function (e.g. `concat ["a", "b"]`).

Functions and types can be defined globally (**top level definitions**) or locally (**local definition**); the _scope_ is different. The `were` and `let` clauses are key to defining local functions or variables, as can be seen in the example below.
```haskell
area d = pi * (r * r)  -- top level
  where r = d / 2  -- local
```

The `:` operator builds a list: `'a' : "bc"`. The functions `head` and `tail` can be applied to strings in order to retrieve the first character (**head**) or everything but the first character (**tail**). A **substring** starting at index 0 can be retrieved using `take`: `take n string`. It will return a list containing the first `n` elements of the list (which can be a `String`). Contrary, `drop` removes the first `n` elements from a list.

```haskell
-- sub list with length l of list x starting at index s
-- pointfree version: https://timodenk.com/blog/making-slice-pointfree/
slice :: Int -> Int -> [a] -> [a]
slice s l x = take l (drop s x)
```

# 4 Basic Data Types
A **data type** is a set of _values_ with an abstract commonality. A **data declaration** defines a new data type. For example, the data type `Bool` is defined with the following _data declaration_.
```haskell
data Bool = False | True
```

**Pattern matching** is a feature of Haskell that allows multiple implementations of the same function. When calling the function, the implementation will be chosen depending on the argument. `_` is called _catch-all_ and will match any argument value.

**Typeclasses** is a polymorphic type that adds functionality (i.e. faculties or interfaces) to types that is reusable across all inheriting types. A **type alias** is a way of making a type available through a different name: `type Name = Integer`.

The **ordering typeclass** `Ord` enforces implementation of the following operators.
```haskell
compare :: a -> a -> Ordering
(<) :: a -> a -> Bool
(<=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(>=) :: a -> a -> Bool
max :: a -> a -> a
min :: a -> a -> a
```

Haskell's inequality symbol is `/=`. The **equality typeclass** `Eq` requires the following.
```haskell
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool
```

A **typeclass constraint** can be made for parameters with the following syntax (here for the function equality operator which requires both operands to implement `Eq`): `(==) :: Eq a => a -> a -> Bool`.

Variables in type signatures are commonly named according to the following rules: (1) Type variables are called `a`, `b`, ...; (2) function variables are called `f`, `g`, ... (3) Arguments to functions are often called `x`, `y`, and `z`. (4) Lists of `x` values are called `xs`. (5) All names can also occur with numbers or the prime symbol appended to them, e.g. `x1` or `f'`.

## 4.1 Numbers
Numbers are inheriting from the _typeclass_ `Num`. 
* **`Int`**. An integral number (aka. integer) with a fixed precision, that is it has upper and lower bound (size: 8 byte). `GHC.Int` adds the integer types `Int8`, `Int16`, `Int32`, and `Int64`, with the number indicating the number of bits used to store the value. The value range of `Int` is _[-9223372036854775808, 9223372036854775807]_.
* **`Integer`**. An integral number that supports arbitrarily large or small numbers.
* **`Float`**. Single-precision floating point number (size: 4 byte).
* **`Double`**. Double-precision floating point number (size: 8 byte).
* **`Rational`**. Represents a fraction of two integer numbers. The data type wraps two `Integer`s and is hence arbitrarily precise.
* **`Scientific`**. Floating point number with an `Integer` base and `Int` exponent. Therefore, the numbers can be arbitrarily large and precise. This data type is not part of GHC and must be installed separately (`stack install scientific`).

The `Integer` type should be preferred over `Int`, and `Scientific` and `Rational` (typeclass `Fractional`) should be preferred over `Float` and `Double`, unless computational efficiency matters.

## 4.2 Boolean
The boolean data type can either be `True` or `False` and is defined as `data Bool = False | True`. Operators for booleans are `&&` for **and**, `||` for **or**, and the function `not` for **inversion**.

Haskell features **if expressions** with the following syntax: `if <condition> then <a> else <b>`. The entire if expression evaluates to either `<a>` or `<b>`, depending on the condition.

## 4.3 Tuples
**Tuples** are types that store a fixed number _n_ of constituents which may have different types themselves. _n_ is referred to as _arity_ (number of parameters that a function takes). A tuple can be created with its constructor, `(,,) x1 x2 x3`, here with _n=3_. Tuples with _n=1_ must not exist, however, _n=0_ is possible and called _unit_ `()`.

For convenience, the first element in a tuple can be accessed using `fst :: (a, b) -> a`; `snd` serves equally for the second value. `Data.Tuple` contains the tuple manipulation functions `curry`, `uncurry`, and `swap`. 

A tuple can be unpacked when passed to a function with the following syntax: `tupleSum (a, b) = a + b`

## 4.4 Lists
The **list** data type stores _n_ values of equal type, where _n_ can be changed dynamically.

The **`n`-th element** of a list can be accessed with the `!!` operator (`n` is zero based): ``"abc" !! n``.

# 5 Types
Type systems have been defined to enforce correctness. In Haskell, typing is _static_ and typechecking occurs at _compile time_. A **data type declaration** defines a _type constructor_ and _data constructors_. Haskell functions are created from the function type constructor `->` and the function is a _value_.

A function signature may have multiple **typeclass constraints** `(Num a, Num b) => a -> b -> b`. In the example, `a` could be an `Integer` and both `b`s could be `Double`s. However, different types for the second argument and the return type would not be possible with this definition. 

The `=>` is called **typeclass arrow**. The right associative **type constructor for functions** `->` realizes currying: `f :: a -> a -> a` is read as `f :: a -> (a -> a)`. Due to currying, functions can be partially applied. Infix operators can be partially applied to a first or second parameter, e.g. `(2^)` or `(^2)`.

**Polymorphism** is the provision of a single interface to entities of different types. In Haskell it is either _parametric_ or _constrained_ (aka. _bounded_, _ad-hoc_). The former is polymorphism that accepts any type, whereas the latter accepts only some types. Multiple class constrains must be wrapped in parentheses: `f :: (Eq a, Num b) => a -> b`. The opposite of polymorphism is _monomorphism_, in Haskell called _concrete_. Applied to variables, polymorphism is a property of variables which may refer to more than one concrete type.

**Type inference** is the process of determining a variables _principle type_ by looking at the way it is being used. The **principle type** is the most generic type that can be assigned to a variable.


# 6 Typeclasses
**Typeclasses** generalize over a set of types in terms of consumption or usage in computation. After declaring a data type with the `data Typename` keyword, typeclasses can be assigned with e.g. `instance Typeclass Typename`. Typeclasses can **inherit** from a _superclass_ (e.g. `class Num a => Fractional a`).

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
Types which implement the `Integral` type are also required to implement `Real` and `Enum`.
```haskell
class (Real a, Enum a) => Integral a
```

A typeclass is implemented for a type with `instance`. The implementation is called **instance** and might look as follows.
```haskell
data Suit = Spade | Diamond | Club | Heart
instance Eq Suit where
  (==) Spade Spade = True
  (==) Diamond Diamond = True
  (==) Club Club = True
  (==) Heart Heart = True
  (==) _ _ = False
```

Typeclasses **default** to certain types. `Num` defaults to `Integer` for example `default Num Integer`. This can be better explained given an example: When entering a `5` into GHCi, a show method must be called. `:t 5` however gives `Num` so it is left to Haskell to choose a `show` method from the inheriting types. In this case `Integer` is chosen by default. 

**Typeclass instances** are unique pairings of a typeclass and a type.

**Effects** are observable actions programs may take, such as writing to a file or printing to the console. `IO` is the type for values whose evaluation bears the possibility of causing side effects.

## 6.1 Derivable Typeclasses
The following typeclasses can be automatically derived. That means they can be automatically instantiated for a given type, based on how it is defined.

 * **`Bounded`**. Types that have an upper and lower bound.
 * **`Enum`**. The type's values can be enumerated. Provides methods such as `succ` (successor; comparable to incrementing), `pred` (predecessor), `enumFromTo`, and `enumFromThenTo` (which uses a step size based on the second argument). 
 * **`Eq`**. The type's values can be tested for equality.
 * **`Ord`**. The type's values can be put into sequential order. Implies `Eq` and can be implemented by defining the `compare` method which returns `EQ`, `LT`, or `GT`.
 * **`Read`**. Values can be parsed from strings. It is often a _partial_ function as it does not return a proper value for all possible inputs.
 * **`Show`**. Values can be converted to strings (e.g. for output). Enforces implementation of `showsPrec`, `show`, and `showList`. Printing things is possible in Haskell, even though it is purely functional, because the `print` method invokes `IO` which has the _side effect_ of outputting text. It returns the unit `()` because it has no relevant return value.

## 6.2 Typeclass Inheritance
Inheritance structure of common typeclasses. `Ord` inherits from `Eq`. `Real` inherits from `Ord` and `Num`. `Fractional` inherits from `Num`. `Integral` inherits from `Real`, `Fractional`, and `Enum`.


# 7 Functional Patterns
Inner variables can _shadow_ outer variables, as can bee seen in the following function which always returns `5`: `func x = let x = 5 in x`.

**Anonymous functions** (aka. lambdas) are functions which are not bound to an identifier and can be declared with this syntax: `(\x -> x * 4) :: Num a => a -> a`. They are often used if a function is passed to another function with the former being needed only once. The signature can be omitted. 

The signature of **higher order functions** contains functions itself. For example `distributor :: (a -> b -> c) -> (a -> b) -> (a -> c)` takes two functions and returns a new one.

The **guard syntax** of Haskell allows to write compact functions with multiple outcomes depending on boolean conditions. Each line behind a pipe is called _guard case_. `otherwise` is a constant that equals `True`, i.e. the catch-all case.
```haskell
clip :: (Num a, Ord a) => a -> a -> a -> a
clip min max x
  | x < min   = min
  | x > max   = max
  | otherwise = x
```

**Pointfree** versions of functions drop arguments for the sake of readability and performance. For example, `print a = (putStrLn . show) a` becomes `print = putStrLn . show`.

**Binding** is the assignment of an argument to a parameter.

# 8 Recursion
A **recursive function** is defined in terms of itself. The **base case** ends the recursion, e.g. factorial of 0 is 1.

In Haskell, **bottom** is a _non-value_ that is used to indicate that a function can not return a value. Possible reasons are errors, partial functions, or infinite recursion / loops.

An example for an elegantly formulated recursive function, performing an integral division:
```haskell
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
```

# 9 Lists

In Haskell, lists are (1) a **collection of elements** of the same type, or (2) an **infinite series** of values (i.e. stream). 

The list **definition** is `data [] a = [] | a : [a]`.

The **initialization** `[1, 2, 3] ++ [4]` is _syntactic sugar_ for `(1 : 2 : 3 : []) ++ 4 : []`. The **range** syntax allows for the definition of sequences from _n_ to _m_ with `[n..m]` and a step size of _1_. It uses `enumFromTo` behind the scenes; and `enumFromThenTo` works for variable step sizes.

**List comprehension**s are a means of generating a new list from an existing list (or multiple lists). For instance `[sqrt x | x <- [0..10], sqrt x < 3]` generates a list of square roots of the numbers from _0_ to _10_, for the cases where the square root is smaller than _3_. `x <- [0..10]` is called _generator_. Multiple generators can be used to create a new list, e.g. `[x*y | x <- [0..10], y <- [10..12]]`. In such a case, each element of the first list will be processed with every element of the second, and so forth.

In the case of a list, the **spine** is a linear succession of one _cons cell_ wrapping another cons cell (`1 : 2 : 3 : []`). Spines are evaluated independently of values. Here, the spine is the structure of a collection, i.e. not the values contained therein. Calling the `length` function with a list does not necessarily lead to an evaluation of all values. The `sprint` command (which is a GHCi feature, not part of the Haskell language) allows you to see how much of a value has been evaluated at this point (https://stackoverflow.com/a/35200329/3607984).

Values in Haskell get reduced to **weak head normal form** by default. **Normal form** means that an expression is fully evaluated. Weak head normal form means the expression is only evaluated as far as is necessary to reach a data constructor. `"a" ++ "b"` is neither of both because the outermost component of the expression is a function.

## 9.1 List Utility Functions
 * **`!!`** returns the **_n_th element**
 * **`take`** returns the **first _n_ elements** of a list. `take :: Int -> [a] -> [a]`
 * **`drop`** returns **all but the first _n_ elements** of a list. `drop :: Int -> [a] -> [a]`
 * **`takeWhile`** iterates over the list and returns **all elements until the condition mismatches**. `takeWhile :: (a -> Bool) -> [a] -> [a]`
 * **`dropWhile`** iterates over the list and **discards all elements until the condition mismatches**. `dropWhile :: (a -> Bool) -> [a] -> [a]`
 * **`splitAt`** returns a **tuple containing the first _n_ and the remaining elements** of the list. `splitAt :: Int -> [a] -> ([a], [a])`
 * **`head`** returns the **first element** of a list. If the list is empty, and exception is thrown.
 * **`last`** returns the **last element** of a list. Throws an exception if the list is empty.
 * **`tail`** returns **all elements but the first** (head). If the list is empty, an exception is thrown.
 * **`init`** returns **all elements but the last**. Throws an exception if the list is empty.
 * **`elem`** checks whether an **element is in a list** or not. `elem :: (Eq a, Foldable t) => a -> t a -> Bool`
 * **`map`** **applies a function to all elements**. `map :: (a -> b) -> [a] -> [b]`
 * **`zip`** creates a **list of tuples** out of two lists. It stops as soon as one list runs out of values. `zip :: [a] -> [b] -> [(a, b)]`
 * **`unzip`** creates a tuple of **two lists out of a list of tuples**. `unzip :: [(a, b)] -> ([a], [b])`
 * **`zipWith`** combines **two lists into one** by subsequently applying a function to two elements. `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`

# 10 Folding Lists
Folding is the reduction of a structure. It happens at two stages, namely (1) traversal and (2) reduction. _Folding_, as a concept, is also refered to as **catamorphism**, that is the unique homomorphism (structure preserving map) from an initial algebra into some other algebra.

The right associative function **fold right**, `foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b`, applies a base value and the last value of a foldable type to a function, takes the result and recursively applies the function to a sequence of values, yielding one value as its final result. The function folds a foldable type _with_ the function `f :: a -> b -> b`.

When computing the product of all values of a foldable, the base value (identity) is _1_; for sums it would be _0_. The identity is also returned, if the foldable data structure contains no value, e.g. an empty list `[]`.

The **left fold** is traversing the data structure in the same order as the right fold, however it is left associative. It is inappropriate to use in combinations with very long lists or impossible with infinite lists. `foldl'` is the strict version of `foldl`. The relationship between `foldl` and `foldr` is (for finite lists `xs`) `foldr f z xs = foldl (flip f) z (reverse xs)`.

**Scans** return a list of all intermediate values of a fold. `scanr :: (a -> b -> b) -> b -> [a] -> [b]` and `scanl` are the Haskell function for right fold and left fold respectively. `scanl` can for example be used to create an infinite list of Fibonacci numbers: `fibs = 1 : scanl (+) 1 fibs`. 

# 11 Algebraic Data Types
**Type constructors** are used at the type level, in type signatures, typeclass declarations, and instances. They are static and resolved at compile time. **Data constructors** construct values and can be interacted with at runtime. Type and data constructors with no arguments are **constants**, for instance `Bool`.

The **arity** of a constructor is the number of parameters it takes. A type or data constructor with no arguments are called _nullary_ and are _type constant_. Data constructors that take exactly one argument are called _unary_, with more than one they are referred to as _products_.

A type constructor argument that does not occur alongside with any value constructor is called **phantom**. For example `a` is a phantom in the declaration `data Type a = Value`.

The **record syntax** allows for the definition of types, where the contained values have names. For example `data Person = Person { name :: String, age :: Int }`. The values can then be accessed by e.g. `name person`.

## 11.1 Kinds
**Kinds** are the types of types. They can be queried in GHCi with `:kind`. For example the kind of `[]` is `* -> *` because it needs to be applied to one type (in order to yield `*`, which is _fully applied_).

**Type constructing** is referring to the application of a type to a type constructor. 

**As-patterns** are a way of unpacking an argument, still keeping a reference to the entire argument. The `@`-sign is used for that:
```haskell
f t@(a, _) = do
  print a
  return t
```

## 11.2 Newtype
**`type`** creates an alias (e.g. `type TwoBool = (Bool, Bool)`), while **`data`** creates arbitrary data structures. **`newtype`** creates types with a single unary data constructor. Resulting from this, the cardinality of the new type equals the cardinality of the type it contains. A `newtype` cannot be a product type, sum type, or contain a nullary value constructor. It has no runtime overhead, because it is reduced to the type it contains.

An example of usage for `newtype`. The `Int` in `B` is wrapped and can therefore be processed differently by `tooMany`. 
```haskell
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype B = B Int deriving (Eq, Show)
instance TooMany B where
  tooMany (B n) = n > 100
```

If `B` shall fall back on the default `tooMany` implementation, the `deriving` keyword can be used in combination with a compiler pragma:
```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool
instance TooMany Int where
  tooMany n = n > 42

newtype B = B Int deriving (Eq, Show, TooMany)
```

## 11.3 Cardinality
The **cardinality** of a type is the number of values it can possibly have. The cardinality _|A|_ of a type `A = A1 a11 ... a1n | ... | An an1 ... ann` is given by _|A1|+...+|An|_, where _|Ai|=|ai1|×...×|ain|_. For instance, the cardinality of `Bool = False | True` is _1+1=2_.

**Sum types** are _or connections_ of multiple types; e.g. `A = B | C`. **Product types** are _and connections_ and have e.g. the following shape: `A = B c d`. Here `B` contains `c` and `d`.

The **number of possible input-output-mappings** of a function from `a` to `b` is computed by _|b|^|a|_. `a -> b -> c` gives _|c|^(|b|×|a|)_.

# 12 Signaling Adversity
In Haskell it is common to use so called _smart constructors_ [https://wiki.haskell.org/Smart_constructors](wiki.haskell.org). These constructors validate their arguments and return `Maybe`, i.e. either the desired object or `Nothing` (or throw an error). For more detailed information about the error, the return type may also be `Either`, which holds a `Left` and a `Right` value. The former is commonly the error object.

**Lifted** and **unlifted types** have different kinds, namely `*` and `#` respectively. Lifted types are much more common and differ from unlifted types by their property of being able to be inhabited by _bottom_.

The type construction `[Maybe]` is invalid, because `[] :: * -> *` and `Maybe` is not `*` but `* -> *` itself.

Opposed to folds, **unfolds** build up data structures from a single starting value (_anamorphism_). `iterate :: (a -> a) -> a -> [a]` does that infinitely, `unfoldr :: (b -> Maybe (a, b)) -> b -> [a]` (in `Data.List`) is the generalization which may terminate.

# 13 Building Projects
Haskell **[Cabal](https://www.haskell.org/cabal/users-guide/)** (Common Architecture for Building Applications and Libraries) is a package manager. A package is a program that may have dependencies.

**Stack** is a program for developing Haskell projects. It is built on top of Cabal. The command `stack build` builds a project and `stack setup` [...]. `stack ghci` starts GHCi in the context of a program, where functions can be executed. `stack new <project-name> simple` creates a new project using the [template](https://github.com/commercialhaskell/stack-templates) "simple".

The **.cabal** file (located in a project's root folder) contains information about the project. For example whether it is a library or an executable.
```cabal
library | executable program-name
  hs-source-dirs:      src
 [exposed-modules:     Module1, Module2]  -- for libraries
 [main-is:             Main.hs]  -- for executables
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
```

However, with Stack projects, there is also a `stack.yaml` file, which contains dependencies. It should be preferred over the Cabal file.

A program can be **start**ed with `stack exec <program-name>` from every directory. However, the program executable is only present if the `.cabal` file that was built before contains the line `executable <program-name>` and if the program was built.

By default a module **export**s all its content. This can be changed by adding a list of exported items:
```haskell
module ModuleName
  (function1, constant1)
  where

-- implementation of function1, constant 1, and possibly more
```
The importing module can also choose what to **import**. This is dome similarly, through a list of items, e.g. `import Data.Bool (bool)`. The other way around, certain things can be excluded from an import: `import Database.SQLite.Simple hiding (close)`.  
**Qualified imports** persist the fully qualified name of the imported items. That means with `import qualified Data.Bool` the function `bool` is only accessible through `Data.Bool.bool`.  
With an **alias**, e.g. `import qualified Data.Bool as B`, `bool` is accessible through  `B.bool`.

In GHCi the **`:browse <Module>`** command lists all items exported by a module. `Prelude` can be disabled with the command `stack ghci --ghci-options -XNoImplicitPrelude`.

## 13.1 Read CSV File
Example snippet that reads the file "data.csv":
```haskell
module CSVReader (readCsv) where

import Data.List.Split (splitOn)

readCsv :: IO [[String]]
readCsv = do
  raw <- readFile "data.csv"
  return $ parseCsv raw
  where
    parseCsv :: String -> [[String]]
    parseCsv s = map (splitOn ",") (lines s)
```

# 14 Testing
There are generally four recognized levels of tests:
1. **Unit testing** tests small units of code, generally on function level, or in object-oriented programming environments on class level.
2. **Integration testing** verifies the interfaces between components against design specifications. It ensures that the units (tested in 1.) are _wired up_ properly.
3. **Component interface testing** controls the data that is passed between units. The data is commonly logged. Unusual data values in an interface can help explain unexpected performance in the next unit.
4. **System testing** (aka. end-to-end testing) tests a completely integrated system to verify that the system meets its requirements.

A **property-based testing** framework runs the same test over and over with generated input.
## 14.1 Hspec
Hspec ([website](https://hspec.github.io/)) is a Haskell testing framework. In order to work with it, the dependency `hspec` must be added or it can be installed manually.
```bash
cabal install hspec
```

Sample snippet
```haskell
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
```

## 14.2 QuickCheck
QuickCheck ([website](http://www.cse.chalmers.se/~rjmh/QuickCheck/)) was the first library to offer what is today called property testing. The dependency is spelled `QuickCheck`.

```bash
cabal install QuickCheck
```

Sample snippet which uses QuickCheck in combination with hspec. QuickCheck itself does not provide the `describe` and `it` methods.
```haskell
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
```
Another workflow is calling `quickCheck (fn :: signature)`.

QuickCheck validates the property by plugging in random values and edge cases. These are generated in this manner: `sample (arbitrary :: Gen Int)`.

Generators select values from a list, e.g. lowercase characters.
```haskell
genChar :: Gen Char
genChar = elements ['a'..'z']
```

This generator is more sophisticated and capable of generating lists of _n_ elements of type `a`, where _n_ is randomly chosen within an upper and lower bound.
```haskell
arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList = flip genList arbitrary

genList :: (Int, Int) -> Gen a -> Gen [a]
genList (minL, maxL) g = sized $ \n -> do
  k <- choose (minL, min (max minL n) maxL)
  sequence [ g | _ <- [1..k] ]
```

`CoArbitrary` is used when random functions need to be generated.

# 15 Monoid and Semigroup
## 15.1 Monoid
A **monoid** is a binary associative operation with an identity. In other words, it is an operator that takes two arguments that follow the rules associativity and identity.

```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty, mappend #-}
```
Monoids are all types that let you join values together through the `mappend` function, in accordance with associativity. A `mempty` value exists for which the `mappend` becomes the identity.

Much more extended functionality lies in the **package `Data.Monoid`**. Opposed to many other Haskell typeclasses, monoids do often have multiple implementations per type. That is realized by wrapping the type with `newtype`. For example the `newtype` `Sum`, which wraps `Num`s and determines to use the addition monoid for the wrapped value. Calling `mappend` with two `Product` values, however, would multiply them. The resulting type wraps the sum or the product. The actual number can be retrieved through `getSum` and `getProduct` respectively. Similarly, the `Bool` monoid is wrapped in either `Any` (boolean disjuction) or `All` (boolean conjunction).

**`mconcat`** applies `mappend` to an arbitrary number of values. For the empty ist it returns `mempty`, for a list with one entry it is the identity.

The **Abelian monoid** has the commutative property. An **orphan instance** is an instance that is defined for a datatype
and typeclass, but not in the same module as either of them. If neither typeclass nor datatype were defined manually, the best workaround is to create a `newtype` which wraps the datatype.

## 15.2 Semigroup
A **semigroup** (Haskell package `Data.Semigroup`) is a monoid without the identity property. That is an operation which takes two inputs and reduces them to one, and suffices the law of associativity. In code, that means the semigroup defines
```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```
while satifying associativity, i.e. `(a <> b) <> c = a <> (b <> c)`.

The **`NonEmpty`** datatype resides in `Data.List.NonEmpty`. It is a list that contains one or more elements.

# 16 Functor
A functor is a structure preserving mapping. Such a mapping requires a function that is applied to each of the values that the wrapping type encloses. A functor satisfies that for an identity mapping, the values remain the same, also the composition law `fmap (f . g) == fmap f . fmap g` holds. The infix operator for `fmap` is `<$>`.
```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

Applying a function to a values that is inside a structure is refered to as **lifting**.

For nested **functor application**, e.g. when applying a function to characters which are stored in a list of `String`s, `(fmap . fmap) strFn dataStruct` can be used.

In order to use a higher kinded Type, e.g. `* -> * -> *`, as a `Functor`, one of the type parameters has to be applied. This can either be done with a concrete type such as `Integer` or with a type variable `a`, and results in the kind `* -> *`. Sample snippet:
```haskell
data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
```

A **natural transformation** is changing the structure while preserving the content.
```haskell
{-# LANGUAGE RankNTypes #-}
type Nat f g = forall a . f a -> g a
```

# 17 Applicative
An **applicative** is a monoidal functor. Opposed to `fmap`, with `<*>` the function (that is applied to the enclosed values) is inside a functor itself. Intuitively this can be understood as _mapping a plurality of functions over a plurality of values_. The type info is the following:
```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

The function **`pure`** can be though of as _embedding a value into any structure (functor)_. For example `pure 1 :: [Int]` gives `[1]`.

An `Applicative` satisfies the two following laws:
1. Identity: `pure id <*> v = v`
2. Composition: `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
3. Homomorphism (structure preserving): `pure f <*> pure x = pure (f x)`
4. Interchange: `u <*> pure y = pure ($ y) <*> u`

## 17.1 Examples
| Command | Result |
| --- | --- |
| `(,) <$> [1, 2] <*> [3, 4]` | `[(1,3),(1,4),(2,3),(2,4)]`|
| `(+) <$> [1, 2] <*> [3, 5]` | `[4,6,5,7]` |
| `liftA2 (+) [1, 2] [3, 5]` | `[4,6,5,7]` |

## 17.2 Testing
Validating whether a data structure satisfies the mentioned laws can be done with the [checkers](https://github.com/conal/checkers) package. The following snippets validates an `Applicative`. Note that the value is not actually being used. Its purpose is to indicate which types to validate.
```haskell
module ApplicativeTests where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

list = [("b", "w", 1)]

main = do
  quickBatch $ applicative list
```

## 17.3 Maybe
[Source code](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html)
```haskell
-- | @since 2.01
instance Applicative Maybe where
  pure = Just

  Just f  <*> m       = fmap f m
  Nothing <*> _m      = Nothing

  liftA2 f (Just x) (Just y) = Just (f x y)
  liftA2 _ _ _ = Nothing

  Just _m1 *> m2      = m2
  Nothing  *> _m2     = Nothing
```


# 18 Monad
Monad is a typeclass reifying an abstraction that is commonly used in Haskell. Instead of an ordinary function of type a to b, it is functorially **applying a function which produces more structure itself** and **using join to reduce the nested structure** that results.

```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
```

`>>=` is called _bind_ operator (`=<<` . Intuitively it can be understood as given a couple of wrapped values and a function that can be applied to these, the bind operator applies the function to each of the values. Special about it is (compared to `fmap`) that the argument order is flipped and the mapping function returns a monad itself which is joined to make sure the output is not nested. The application to the list monad clarifies: `(>>=) :: [a] -> (a -> [b]) -> [b]`.

`*>` for `Applicative` corresponds to `>>` for `Monad`. The `do` syntax is converted into each line being _concatenated_ with the following line using one of the two operators. Variable assignments `<-` are converted to `>>=`, for example 
```haskell
do
  name <- getLine
  putStrLn name
```
becomes the following:
```haskell
getLine >>= \name -> putStrLn name
```

`Control.Monad` contains a **`join`** function. The book introduced it with the example `join $ putStrLn <$> getLine`, which would, without `join`, fail because of nested `IO`s.

Example of using the `do` syntax in combination with the `List` monad:
```haskell
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]
```

The Monad **laws** are 
* Right identity `m >>= return = m`. Applying `return` leaves the data untouched.
* Left identity `return x >>= f = f x`. Applying `return` leaves the data untouched.
* Associativity `(m >>= f) >>= g = m >>= (\x -> f x >>= g)`. Regrouping the functions should not have any impact on the final result.

Using Checkers (as in 17.2) with `quickBatch (monad [(a, b, c)])` where `a`, `b`, and `c` are three values which indicate the type to be used.

The **Kleisli composition** (_fish_ operator: `>=>`) is about composing two functions which both return monads. It can be imported with `import Control.Monad ((>=>))` and has the following signature (in comparison to normal function composition):
```haskell
(.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

# 19 Applying Structure
The operators `*>`, `<*` and `>>` discard one of their arguments and are often used in combination with functions that emit side effects.

## 19.1 JSON Parsing Example
The data is nested inside of the `Parser` monad so the value constructor `Payload` needs to be lifted.
```haskell
parseJSON :: Value -> Parser a
(.:)      :: FromJSON a => Object -> Text -> Parser a

instance FromJSON Payload where
  parseJSON (Object v) =
    Payload <$> v .: "from"
      <*> v .: "to"
      <*> v .: "subject"
      <*> v .: "body"
      <*> v .: "offset_seconds"
  parseJSON v = typeMismatch "Payload" v
```

# 20 Foldable
The foldable typeclass has the following definition:
```haskell
class Foldable (t :: * -> *) where
  Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  Data.Foldable.foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
  {-# MINIMAL foldMap | foldr #-}
```

For its definition it is sufficient to provide an implementation for either `foldMap` or `foldr`. All other functions can be deduced from that.

`Monoid`s are related to the functions `foldr` and `foldMap`. The former uses the monoid definitions of elements inside of the foldable structure to combine them. The latter converts the elements to monoidal values and folds subsequently. In both cases the default-value is provided by the monoid identity.

The **`null`** function returns `True` if the data structure is empty. Note, that e.g. `null (Left 1)` is `True`, because `Left` is considered empty whereas `Right` is not.

Noteworthy are also **`toList`**, **`length`**, and **`elem`**. Both ignore the non-monoid values, for instance `length (1, 1)` is `1`.

Both, `maximum` and `minimum` require the contained types to be `Ord` and **return the maximum and minimum** value respectively. They cannot be applied to empty structures (otherwise an exception is being thrown).

# 21 Traversable
**`Traversable`** allows for the processing of values inside a data structure as if they were in sequencial order. Opposed to `Functor`, where function applications happen semantically in parallel. Return values of later function applications of `Traversable` can depend upon the earlier results. That can be seen as an __accumulation of applicative contexts__. The typeclass definition is the following:
```haskell
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
```

The typeclass satisfies the following rules:
1. Naturality: `t . traverse f = traverse (t . f)`
2. Identity: `traverse Identity = Identity`. Traversable instances cannot inject any additional structure.
3. Composition: `traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f`. Multiple traversals can be collapsed into a single traversal using `Compose` (which combines structure).

`traverse` and `sequenceA` can be defined in terms of each other: `traverse f = sequenceA . fmap f` and 
```haskell
sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceA = traverse id
```

The function `catMaybes` from `Data.Maybe` converts a `Traversable` of `Maybe` values into a `Traversable` with all `Just` values. For instance `catMaybes [Just 1, Just 2, Nothing]` is `[1,2]`.

The function **`traverse`** applies a function `(a -> f b)` onto values inside a data structure `t a` and flips the result by returning `f (t b)`.

`Traverable` implementation for `Either`: 
```haskell
instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y
```


* Difference on page 878 bottom?
* `(sequence .) . fmap`
* Naturality
* **"Why does `Traversable` _depend_ on `Foldable`?"** [Reddit](https://www.reddit.com/r/haskell/comments/7dmjh8/why_does_traversable_need_foldable/)


# 22 Reader

The core problem that `Reader` solves is the application of an argument to many functions. It is inconvenient to have a similar signature across many functions. The `Reader` is wrapping a function which maps from `r` to `a`.

```haskell
newtype Reader r a = Reader { runReader :: r -> a }
```

The `Functor` instance of a function `(-> r)` is composition. The `Applicative` and `Monad` instances allow for the mapping of a function that expects an `a` over another function that expects an `a` as well. The result of the first function is fed into the second together with an `a` (see code snippet below).

```haskell
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)
```

Here is the `Monad` instance with and without function:
```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)
```

The function `fmap` can be used for **function composition**: Let `f` and `g` be two functions, than `f . g = fmap f g`.

The extension `{-# LANGUAGE InstanceSigs #-}` allows for the explicit definition of function signatures of typeclasses. It is not necessary for any compilation purpose because the compiler knows the signatures anyways. However, it can be helpful for the sake of clarification.


## 23 State
Definition
```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

# 24 Parser Combinators
A **parser** converts textual input into some data structure output. The textual input must be in conformance with a set of rules. A **parser combinator** is a higher order function which composes multiple parsers to yield a single parser.

Using the parser module `trifeca` (`import Text.Trifecta`). Useful links:
* [Text.Trifecta.Parser.Char](https://hackage.haskell.org/package/trifecta-0.44/docs/Text-Trifecta-Parser-Char.html)
* [Text.Trifecta.Parser.Combinators](https://hackage.haskell.org/package/trifecta-0.44/docs/Text-Trifecta-Parser-Combinators.html) contains e.g. `eof`

`trifecta` contains parsers and is used in combination with the `parsers` library with defines common parser classes which abstract over common kinds of things parsers do.

It throws errors with the `unexpected` function.
```haskell
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"
```

The parser type is very similar to `State`. It takes a string, parses it and returns `Nothing` in case of failure. If parsing was successful, it returns a data structure and the remainder of the `String`.
```haskell
type Parser a = String -> Maybe (a, String)
```

`parseString :: Parser a -> Delta -> String -> Result a` is used to run a Trifecta parser. `Delta` may be `mempty`. The Attoparsec equivalent is `parseOnly`. Parsing functions of the most common libraries.
```haskell
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'
```

In the following simple example, a parser accepts only the character sequence `ab`:
```haskell
abAccepted = char 'a' >> char 'b'
abParser = parseString abAccepted mempty
abParser "ab"
-- Success 'b'
abParser "a"
-- Failure (ErrInfo {_errDoc = [1m(interactive)[0m:[1m1[0m:[1m2[0m: [91merror[0m: unexpected
--    EOF, expected: "b"
-- a[1m[94m<EOF>[0;1m[0m 
--  [92m^[0m     , _errDeltas = [Columns 1 1]})
```

The parser above does not necessarily consume all its input. Given `"abc"` it would return `Success` as well. That can be changed using `eof`: `string "ab" >> eof`. Inputs such as `"abc"` would result in `Failure [...] expected: end of input [...]`. `eof = notFollowedBy anyChar`

In most cases, a parser should not raise any error other than the parsing `Failure` that it may return. Other exceptions should be prevented from happening. The following example catches a division by zero error using `fail`.
```haskell
parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator most not be zero"
    _ -> return (numerator % denominator)
```

**Parser libraries** in Haskell are `parsec`, `attoparsec`, `megaparsec`, and `trifecta`. `aeson` parses JSON, `cassava` parses CSV. Polymorphic parsers are written in a general manner an can be executed with any parser that implements the functions. The following is an example signature of a polymorphic parser.
```haskell
parseFraction :: (Monad m, TokenParsing m) => m Rational
```

Example for a parser which parses a number **or** a string:
```haskell
-- parse number or string
type NumberOrString = Either Integer [Char]

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n ") >>
  (Left <$> integer) <|> (Right <$> some letter)

print $ parseString parseNos mempty "123"
print $ parseString parseNos mempty "\nabcdf"
```

The `<?>` operator can be used to annotate branches of a parsing instruction. In the following example, `Tried 12` will be printed it the `12` branch was chosen instead of the `3`:
```haskell
tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") <|> (char '3' <?> "Tried 3")
```

# 25 Composing Types
The following `newtype` constructs a datatype by **composing** datatype constructors. The kind is `Compose :: (* -> *) -> (* -> *) -> * -> *`, comparable to function composition `(.)`.
```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) } 
  deriving (Eq, Show)
```

The specialty of the `Monad` type is that cannot be implemented for the `Compose` `newtype` from above. That is the following function does not exist in a generic manner: `(>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b`, see ([Composing Monads](http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf)). This is where transformers come in, for instance `IdentityT`
```haskell
newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)
instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
```
where `IdentityT [...] runIdentityT . f` is required because the used `>>=` is the one of the `Monad m`, so `IdentityT` needs to be unpacked.

Transformers have additional information on how to do the unpacking and subsequent boxing that is specific to the given monads, and cannot be generalized. Conversion from `f (g (f b))` to `f (f b)` in order to be able to bind and return `g (f b)`.  
`m (T m b) -> m (m b) -> m b -> T m b`

With two nested Functors, say List of Maybes, there is a guarantee that the nested data type is also a Functor. Same with Applicative. For Monad, this fact does not hold.

# 26 Monad Transformers

A **monad transformer** is a type constructor that takes a monad as an argument. This monad is wrapped around another contained data type. The transformers themselfes are not generically monads, but implement the `Monad` typeclass logic. The inner type corresponds _commonly_ to the transformer, i.e. `MaybeT` is `m (Maybe a)`.

`Identity` can be used as a `Monad` which converts transformers into their original types. For instance `type Maybe a = MaybeT Identity a`.

The **[transformers](https://hackage.haskell.org/package/transformers) library** contains many implementations of transformers and should be preferred over own implementations if possible.

The **base monad** is the outer-most monad, here it would be `m`: `StateT { runStateT :: s -> m (a, s) }`

**Maybe Transformer `MaybeT`**
```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

**State Transformer `StateT`**
```haskell
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

module StateT where
  
import Control.Arrow (first)

newtype State s a = State { runState :: s -> (a, s) }

-- Structure: StateT function > Monad > Tuple
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT a) = StateT $ \s -> first f <$> a s
  
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  StateT g <*> StateT h = StateT $ \s -> do
    (f, s') <- g s
    (x, s'') <- h s'
    return (f x, s'')
    
instance (Monad m) => Monad (StateT s m) where
  return = pure
  StateT sma >>= f = StateT $ \s -> sma s >>= \(a, s') -> runStateT (f a) s'
```

For **streaming**, it is generally not recommended to use `Writer`, `WriterT`, or `ListT` for performance reasons. Libraries such as [pipes](http://hackage.haskell.org/package/pipes) or [conduit](http://hackage.haskell.org/package/conduit) are better choices.


**Lifting functions** exist with multiple different signatures but _should always_ do the same thing. The functions exist only for historical reasons.
```haskell
fmap  :: Functor f     => (a -> b) -> f a -> f b
liftA :: Applicative f => (a -> b) -> f a -> f b
liftM :: Monad m       => (a -> r) -> m a -> m r
```

**MonadTrans** is a typeclass with a lift method. Lifting means embedding an expression in a larger context by adding structure that doesn’t do anything.
```haskell
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a
```


**`MonadIO`** resides in `Control.Monad.IO.Class` and is intended to keep lifting an IO action until it is lifted over all structure that surrounds the outermost IO type. !!! (surrounds or embedded -- PDF page 1040)

```haskell
liftIO . return = return
liftIO (m >>= f) = liftIO m >>= (liftIO . f)
```

# 27 Nonstrictness
Evaluation can be _strict_, _nonstrict_. Haskell is nonstrict. Nonstrictness refers to semantics, that is how an expression is being evaluated. It means that expressions are evaluated outside-in. _Lazy_ refers to operational behavior, the way code is executed on a real computer, namely as late as possible. [SO answer](https://stackoverflow.com/a/7141537/3607984)

In Haskell an expression does not need to be recomputed once it is evaluated. "Don't re-evaluate if you don't have to."

The evaluation of an expression can be forced using the function `seq :: a -> b -> b`. It evaluates its first argument as soon as evaluation of the second argument is required.

There are some compiler options available that allow for observation of the interpretation of the Haskell code (_core dump_). For instance `:set -ddump-simpl` or `:set -dsuppress-all`.

The following snippet forces evaluation of `x` by tying it to `b`:
```haskell
discriminatory :: Bool -> Int
discriminatory b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True -> 1
```
This translates to two nested `case` blocks:
```haskell
discriminatory =
  \ b_a10D ->
    let {
      x_a10E
      x_a10E = undefined } in
    case
      case x_a10E of _ {
      __DEFAULT -> b_a10D
    } of _ {
      False -> I# 0;
      True -> I# 1
    }
```

This statement will not bottom out `case undefined of { _ -> False}` whereas `case undefined of { DEFAULT -> False }` cannot be simplified by the compiler and will result in an error.

**Call strategies** are _call by value_, i.e. evaluation of an argument before passing it to a function, and _call by reference_ arguments are not necessarily evaluated. _Call by need_ ensures that expressions are only evaluated once.

A **thunk** is used to reference suspended computations that might be performed or computed at a later point in your program.

The `trace` function in `Debug.Tace` allows for logging at places, where the `IO` type is not present. That way, evaluation can be debugged. For instance `let a = trace "a" 1` would log `"a"`, as soon as `a` is being evaluated. Writing pointfree functions allows for caching, as can be seen in the following example:
```haskell
import Debug.Trace

f :: Int -> Int
f = trace "f called" (+) 1

f' :: Int -> Int
f' b = trace "f' called" 1 + b

f 5
f 6
f' 5
f' 6
```
which outputs
```
f 5
f called
f' 5
f' called
f' 6
f' called
```
If `f` was, however, defined with the signature `f :: Num a => a -> a`, it would not be cached. That is because typeclass constraints will be resolved into additional arguments.

Forcing a value **not to be shared** can be done by applying unit to it, as if it were a function. `let f x = (x ()) + (x ())`, where the signature is `f'' :: (() -> Int) -> Int`.

`let` can be used to **force sharing**. Here, `(1 + 1) * (1 + 1)`, `1 + 1` would be computed twice. With `let` only once: `let x = 1 + 1 in x * x`.

A function's pattern matching can be **refutable** (German: _widerlegbar_) or **irrefutable**. The former is for instance `f True = ...`, the latter `f _ = ...` or `f x = ...`. The terminology is not about the function but about a single pattern matching expression, i.e. one line.

**Lazy pattern matches** can be implemented using the `~`. The first function will fail, when invoked with `undefined`, whereas the latter works. The latter also makes the pattern irrefutable though.
```haskell
strictPattern :: (a, b) -> Integer
strictPattern (a,b) = const 1 a
lazyPattern :: (a, b) -> Integer
lazyPattern ~(a,b) = const 1 a
```

**Bang patterns** can be used to force evaluation of function parameters (see example below) or value contstructor parameters.
```haskell
banging :: Bool -> Int
banging !b = 1
```

The extensions `{-# LANGUAGE Strict #-}` and `StrictData` force strictness for expressions in the particular source code file. Thereby they avoid `seq`, `~`, and `!` noise if everything is supposed to be strict. The meaning of `~` is now inverted, i.e. it forces lazyness.


# 28 Basic Libraries
## 28.1 Benchmarking and Profiling
The library `criterion` can be used for benchmarking.
* Import: `import Criterion.Main`
* Compiler options: `stack ghc -- -O2 file.hs` (or without Stack: `ghc -O2 file.hs`). `-02` enables the highest level of optimization
* Measures how long it takes (on average) to evluate a certain expression.
* `whnf` (weak head normal form) evaluates until it reaches the first data constructor (_used most of the time_); `nf` (normal form) evaluates everything.

The sample snippet
```haskell
main :: IO ()
main = defaultMain
  [ bench "test" $ whnf ([1..9999] !!) 9998 ]
```
could have this output:
```
benchmarking test
time                 41.14 μs   (37.59 μs .. 46.23 μs)
                     0.871 R²   (0.739 R² .. 0.992 R²)
mean                 39.91 μs   (37.36 μs .. 46.33 μs)
std dev              13.83 μs   (3.873 μs .. 25.74 μs)
variance introduced by outliers: 98% (severely inflated)
```

[GHC user guide on profiling](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html)

**CAF**s (constant applicative forms) are expressions that have no free variables and are held in memory (pointfree top-level declarations). For instance (in the file profile.hs)
```haskell
memoizedFib :: Int -> Integer
memoizedFib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoizedFib (n-2) + memoizedFib (n-1)

main :: IO ()
main = putStrLn . show $ memoizedFib 1000
```
the profiling
```
stack ghc -- -prof -fprof-auto > -rtsopts -O2 profile.hs
./profile +RTS -P
cat profile.prof
```
outputs (excerpt):
```
COST CENTRE     MODULE           SRC                       %time %alloc  ticks     bytes

memoizedFib.fib Main             profile.hs:(3,10)-(5,54)  100.0   48.2      7    124200
CAF             GHC.IO.Handle.FD <entire-module>             0.0   13.5      0     34704
CAF             GHC.IO.Encoding  <entire-module>             0.0    1.1      0      2768
main            Main             profile.hs:8:1-41           0.0    8.5      0     21800
memoizedFib     Main             profile.hs:(2,1)-(5,54)     0.0   28.0      0     72120
```

## 28.2 Map
Package: `Data.Map.Strict`  
Access to values through their keys, access time _O(log n)_.

## 28.3 Set
Package: `Data.Set`  
A set stores values (none must occur more than once). It can be seen as a map without values. Access time complexity: _O(log n)_.

## 28.4 Sequence
Package: `Data.Sequence`  
While appending to a normal Haskell list has _O(n)_ complexity, appending to a sequence is as fast as prepending to a normal list, i.e. _O(1)_.

## 28.5 Vector
Package: `Data.Vector` ([https://hackage.haskell.org/package/vector](package))  
A vector wraps an [array](http://hackage.haskell.org/package/array). Should be used when having high performance requirements, accessing elements by indexing with `Int`, slicing (partitioning) is done, or uniform access time is needed.

## 28.6 Strings
* `String`: Standard, slow, list of characters, possibly infinite.
* `Text`: Text encoded as UTF-16, more efficient than `String` in terms of storage.
* `ByteString`: Internally represented as a vector of `Word8` values (i.e. bytes), can contain non-text data. Easy to use via the `OverloadedStrings` extension.


# 29 IO
> The IO Monad is just an instance of the ST monad, where the state is the real world.
The `IO` `:info`:
```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
instance Applicative IO
instance Functor IO
instance Monad IO
instance Monoid a => Monoid (IO a)
```

* `IO` disables the reordering of operations.
* An expression is referentially transparent when it can be replaced with its value without changing the behavior of a program.

# 30 Error Handling
The `Exception` class resides in `GHC.Exception` and is defined as follows:
```haskell
class (Typeable e, Show e) => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
instance Exception SomeException
instance Exception ErrorCall
instance Exception ArithException
```

Some types that have an instance of the `Exception` class are `IOException`, `ErrorCall`, `AssertionFailed`, and `ArithException`. The latter contains several values, namely `Overflow`, `Underflow`, `LossOfPrecision`, `DivideByZero`, `Denormal`, and `RatioZeroDenominator`.

Existential quantification allows for the definition of an exception type that represents a variety of values, some of which may have been unknown at the type the exception type was defined. `SomeException` works that way and is defined as follows:
```haskell
data SomeException where
  SomeException :: Exception e => e -> SomeException
```

Exceptions occur most commonly in `IO`, because the function calls depend on the _outside world_. For a simple `writeFile` call, exception handling may look like that:
```haskell
import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print (typeOf e)
  putStrLn show e

main :: IO ()
main = writeFile "file.txt" "content" `catch` handler
```

A main function can be called with _command line arguments_ from within REPL using the command `:main -arg -arg2`.

Both, `trow` and `throwIO` allow for **raising** an exception. Generally, `throwIO` is being used.

A sum type is a convenient way of **grouping several exceptions** which can be caught collectively.

**Asynchronous exceptions** a exceptions raised in a thread other than the one which will handle the exception.

---

## Todo
* ~~Write benchmark for `newtype DList a = DL { unDL :: [a] -> [a] }`~~
* ~~Pop fast~~
* ~~Make `slice' from len = take len . drop from` point-free~~
* Play around with `CoArbitrary`, try to pass a number and see whether the `Gen` is reduced to a single value.
* Write Either with failure Monoid which does not store the same error twice.
* `tupled'`, `getDogReader`
* Understand `MonadIO` (and the general idea behind it). [Potentially interesting](https://stackoverflow.com/questions/38212294/why-is-monadio-specific-to-io-rather-than-a-more-generic-monadtrans)

## Quotes
12. > As natural as any competitive bodybuilder  
`data Nat = Zero | Succ Nat deriving (Eq, Show)`
13. > Do notation considered harmful! Just kidding.
14. > If that succeeded, let’s fire up a REEEEEEEPL and see if we can call sayHello.
16. > 16.4 Let’s talk about 𝑓, baby  
    > We’re going to return to the topic of natural transformations in the next chapter, so cool your jets for now.
17. >  If this seems confusing, it’s because it is.
18. > And putStrLn takes a String argument, performs I/O, and returns nothing interesting — parents of children with an allowance can sympathize.
    > Fail fast, like an overfunded startup
21. > This is how you learn to play type Tetris with the pros.
22. > The rest of the chapter will wait while you verify these things.
23. > Try it a couple of times to see what we mean. It seems unlikely that this will develop into a gambling addiction
24. > In reality, a modern and mature parser design in Haskell will often look about as familiar to you as the alien hellscape underneath the frozen crust of one of the moons of Jupiter.
25. > In this chapter we will... ...work through an `Identity` crisis.
26. > Keep in mind what these are doing, follow the types, lift till you drop.
27. > We will... ...live the Thunk Life
29. > We have measured time; now we shall measure space. Well, memory anyway; we’re not astrophysicists.
30. > Preserve context and try to make it so somebody could understand the problem you’re solving from the types. If necessary. On a desert island. With a lot of rum. And sea turtles.