# Parallel and Concurrent Programming in Haskell

_Simon Marlow_

Sample code: [parconc-examples](https://github.com/simonmar/parconc-examples)

## 1 Introduction

**Parallelism** is about programs that carry out multiple computations simultaneously using a multiplicity of computational hardware. **Concurrency** is a "program-structuring technique in which there are multiple _threads of control_". Computations are carried out during overlapping time periods.

The tool `threadscope` (https://wiki.haskell.org/ThreadScope) allows for performance profiling of parallel Haskell programs.


## 2 Basic Parallelism: The Eval Monad

An expression is **unevaluated** if its final value has not been determined yet. For instance `x = 1 + 2 :: Int`. With GHCi, `:sprint` can be used to peek at a variable without forcing its evaluation. In the example, this would yield `x = _`.

A **thunk** is the object in memory representing the unevaluated computation. After evaluation, the thunk is overwritten by the computed value. Thunks can refer to other thunks.

The evaluation of a variable can be forced, e.g. by printing it (`print x`) or with the [**seq** function](https://wiki.haskell.org/Seq) (`seq x ()`) which evaluates the first argument to _weak head normal form_ (WHNF) (until it hits the first constructor) and returns the second.

Evaluating the compute intense function `f` in **parallel** (for arguments `a` and `b`) and waiting for both comutations to terminate before returning the results:
```haskell
runEval $ do
    a <- rpar (f x)
    b <- rpar (f y)
    rseq a
    rseq b
    return (a,b)
```
_([more sample snippets](https://github.com/simonmar/parconc-examples/blob/master/rpar.hs))_

Build with `stack build` and configure the `ghc-options` in `package.yaml`: `-O2` and `-threaded`. Run with `stack run -- +RTS -N2` to use two cores. Running with `+RTS -s` prints some more statistics after the program terminates.

The function `force :: NFData a => a -> a` (from `Control.DeepSeq`) evaluates its argument entirely, not only to WHNF. The behaviour must be defined for each object individually with the `NFData` class which requires the method `rnf` (reduce normal-form). The module `Control.Deepseq` provides instances for common types. The relationship between `force` and `deepseq` (the _deep_ `seq` equivalent) is `force x = deepseq x x`

A parallelizable problem can be subdivided into _chunks_.  **Static partitioning** assigns chunks of a problem to cores in a static way, no matter how much work each chunk contains. **Dynamic partitioning** in contrast assigns the chunks depending on how long it took to execute previous ones. In practice, `rpar` can be called very frequently (at most once for every chunk) to achieve dynamic partitioning.

The argument passed to `rpar` is called **spark**. The number of created sparks can be seen in the `+RTS -s` log output. Sparks can have different states, namely 
* _overflowed_ if the spark pool size was exceeded, 
* _dud_ if a spark is already evaluated and calling `rpar` on it was unnecessary,
* _GC'd_ if the spark was unused by the program, and
* _fizzled_ if the expression was unevaluated at the time it was sparked but was later evaluated independently by the program.

The **speedup** of a parallel program is defined to be the ratio of two times _t1_ and _t2_, where both are wall-clock times: _s=t1/t2_. It may be a value like ~1.9, when _t1_ is measured on one CPU core and _t2_ on two. **Amdahl's law** makes a statement on the maximum possible theoretical speedup _S_ of a task depending on the number of processors _n_ and the proportion of the task that can be parallelized _p_: _S=1/((1-p)+p/n)_. For an infinite number of cores, _lim n-->inf S=1/(1-p)_.


## Other

New project
```
stack new MyProject
cd MyProject
stack setup
code .
```
