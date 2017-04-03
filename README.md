# purescript-io

An IO monad for PureScript.

Don't ask. Don't tell. Joyfully use in secrecy.

# Introduction

PureScript's effect system is based on row types, and has no semantic or
algebraic basis. Often, the effect labels have poorly-defined meanings, and
different libraries use completely different labels to represent the same or
overlapping effects.

While the effect system is undoubtedly useful, there are cases where it's more
of a hindrance — where it doesn't make reasoning about code easier, but instead,
merely adds a ton of boilerplate, as semantically meaningless or overlapping
labels get threaded through endless stacks of functions.

In those cases, `IO` is here to the rescue!

`IO a` represents a computation that may be synchronous or asynchronous, and
which will either yield a value of type `a`, run forever, or halt with a
catchable exception.

Under the covers, `IO` is based on `Aff`, and there is no wrapper so there is
no runtime overhead or performance penalty for `IO`.

# Effects vs MTL

In MTL, we would denote effects using type classes:

```haskell
class (Monad m) <= MonadConfig m where
  readConfig :: m Config

serverAddress :: forall m. (MonadConfig m) => m InetAddress
```

This serves a similar purpose to effect rows in PureScript, whereby every "label" corresponds to a set of functionality described by a type class. The advantage to using classes over labels is that the semantics can be well-described by the classes.

Similarly, if we were using `Free` directly, instead of using type classes to abstract over a `Free` encoding, we would denote effects using functors:

```haskell
data ConfigF a
  = ReadConfig (Config -> a)

serverAddress :: ReaderT (PrismT' f ConfigF) (Free f) InetAddress
```

In this case, we have all the benefits of finely-grained effects, including semantic descriptions of those effects, but without needing to use labels in effect rows.

In either MTL or direct-Free encodings, we have all the tools necessary to provide clear information to developers to help them reason about the code they are writing — all without using effect rows.

Therefore, MTL and direct-Free approaches can be considered alternatives to PureScript's own effect system (which is implemented as a library, and not baked into PureScript). When using one of these alternatives, it can simplify code and improve type inference to use `IO` instead of trying to doubly-encode effects using two systems — one of which has no semantic or algebraic basis.

# Usage

`IO` is a newtype for `Aff`, which you can unwrap to be used in your `main`:

```haskell
runIO :: forall a. IO a -> Aff (infinity :: INFINITY) a
```

This converts an `IO` into an `Aff`, which you can then "convert" into a
runnable `Eff` using `launchAff` or `runAff`.

The effect row is closed, which is intentional because `INFINITY` represents
all possible effects. This will help ensure you only call `runIO` at the top
level of your program.

Besides this, `IO` has almost all the same instances as `Aff`, and may be used
in the same way. In addition, a new `MonadIO` class has been introduced which
allows you to lift `IO` computations into other monads that are as powerful.

Similarly, `IOSync` exists as an alternative for `Eff`.

Happy nuke launching!
