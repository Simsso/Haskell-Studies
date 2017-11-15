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