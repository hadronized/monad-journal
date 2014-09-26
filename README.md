# monad-journal

## Pure logger typeclass and monad transformer

### What is `monad-journal`?

`monad-journal` is a simple but powerful answer to the logging problem. A lot
of people think that “logging” is `IO`-related, while it’s not. Everyone must
know [MonadWriter](http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Writer-Class.html#t:MonadWriter)
, which is perfect to log things in pure computations. The issue is that you
can’t access those “things” inside the computation itself. `monad-journal`
exposes a cool typeclass called `MonadJournal` that enables you to do so.
