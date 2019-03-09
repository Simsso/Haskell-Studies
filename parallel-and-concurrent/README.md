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


## 3 Evaluation Strategies

An **evaluation strategy** defines one way of evaluating a data structure. It may do that in parallel using `rpar` and `rseq`. The `Strategy` type is defined as
```haskell
type Strategy a = a -> Eval a
```
The idea is that a strategy can be implemented for a certain data type, e.g. `parPair :: Strategy (a,b)` would define a strategy for evaluating a pair. Once a pair is available (not evaluated yet, due to lazyness) the strategy can be used as follows:
```haskell
somePair `using` strategy
```
which is equivalent to
```haskell
runEval $ strategy somePair
```
The `using` could be removed from the program at any place and nothing would break, because the pair (or whatever is the first argument to `using`) would then be evaluated without any particular strategy without any parallelism. For that to hold, the `Strategy` must staisfy the _identity property_. That is, the value it returns must be equal to the value it was passed. That is guaranteed for the strategies in `Control.Parallel.Strategy` but must be manually ensured for custom instances of the monad.

A strategy can becomes a **parameterized strategy** if arguments are passed to it. Note that `rpar :: Eval a` (and `rseq`) is a `Strategy` for any type.

Evaluating a **list in parallel** can be achieved with the function `parList :: Strategy a -> Strategy [a]` from `Control.Parallel.Strategies`.

A _foreign call_ typically indicates IO activity and comes with a parallelization slowdown.

Creating a spark comes at the cost of some operational overhead. There is a sweet spot for the optimal number of sparks. The tradeoff is between creation overhead and idle cores (while other cores process the last remaining sparks).

Sparks are also subject to garbage collection: The runtime automatically deletes _speculative tasks_, i.e. ones that are not being referenced. Speculative, because they might have been created with uncertainty about whether they will be needed and the reference was deleted later on, before the sparks were actually evaluated.

A type of Haskell programs follows a streaming pattern: Input is being consumed and output is being created with _O(1)_ memory requirements. This is known as **streaming**. In such cases, using `parList` can break the parallelism, because all the input would be consumed at once. The method `parBuffer :: Int -> Strategy a -> Strategy [a]` mitigates this problem by creatingat most _n_ (the first parameter) sparks.

**Chunking** data into parts to process them in parallel is a common thing to do. `Control.Parallel.Strategies` provides the method `parListChunk :: Int -> Strategy a -> Strategy [a]` for that purpose. The method's first parameter indicates the number of elements per chunk. 

## Other

New project
```
stack new MyProject
cd MyProject
stack setup
code .
```
