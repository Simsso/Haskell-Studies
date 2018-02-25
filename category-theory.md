# Category Theory
## Structures
**Semigroup** Takes two inputs, reduces them to one, and suffices the law of associativity.  
Package `Data.Semigroup`.
```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

**Monoid** Takes two input, reduces them to one, and suffices the laws of identity and associativity.  
Included by default, extended functions in `Data.Monoid`.
```haskell
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
```

**Functor** Applies a function to values inside a data structure while leaving the structure itself untouched.  
Included by default.
```haskell
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
```

**Applicative** Monoidal functor.  
Included by default, extended functions in `Control.Applicative`.
```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```


**Monad**
```haskell
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
```
## Operators
```
 ($)  ::   (a -> b) ->   a ->   b
(<$>) ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```