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

# Usage

`IO` only has one function, which should only be used in your `main`:

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

Happy nuke launching!
