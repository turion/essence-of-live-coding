# README
--------

essence-of-live-coding is a general purpose and type safe live coding framework in Haskell.
You can run programs in it, and edit, recompile and reload them _while_ they're running.
Internally, the state of the live program is automatically migrated when performing hot code swap.

The library also offers an easy to use FRP interface.
It is parametrized by its side effects,
separates data flow cleanly from control flow,
and allows to develop live programs from reusable, modular components.
There are also useful utilities for debugging and quickchecking.

## Change the program, keep the state!

### Live programs

In essence, a live program consists of a current _state_,
and an effectful _state transition_, or _step_ function.

```haskell
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }
```

We execute it by repeatedly calling `liveStep` and mutating the state.
The behaviour of the program is given by the side effects in the monad `m`.

### Example

Here is a simple example program that starts with the state `0`,
prints its current state each step and increments it by 1:

```haskell
data State = State { nVisitors :: Int }

simpleProg = LiveProgram { .. }
  liveState = State 0
  liveStep  = \State { .. } -> do
    let nVisitors = nVisitors + 1
    print nVisitors
    return $ State { .. }
```

We can change the program to e.g. _decrement_,
by replacing the body of the `let` binding to `nVisitors - 1`.
It's then possible to replace the old program with the new program on the fly,
while keeping the state.

### Migration

The challenge consists in migrating old state to a _new state type_.
Imagine we would change the state type to:

```haskell
data State = State
  { nVisitors  :: Int
  , lastAccess :: UTCTime
  }
```

Clearly, we want to keep the `nVisitors` field from the previous state,
but initialise `lastAccess` from the initial state of the new program.
Both of this is done automatically by a generic function (see [`LiveCoding.Migrate`](https://github.com/turion/essence-of-live-coding/blob/master/essence-of-live-coding/src/LiveCoding/Migrate.lhs)) of this type:

```haskell
migrate :: (Data a, Data b) => a -> b -> a
```

It takes the new initial state `a` and the old state `b` and tries to migrate as much as possible from `b` into the migrated state,
using `a` only wherever necessary to make it typecheck.
`migrate` covers a lot of other common cases,
and you can also extend it with user-defined migrations.

### Functional Reactive Programming

In bigger programs, we don't want to build all the state into a single type.
Instead, we want to build our live programs modularly from reusable components.
This is possible with the arrowized FRP (Functional Reactive Programming) interface.
The smallest component is a _cell_ (the building block of everything live):

```haskell
data Cell m a b = forall s . Data s => Cell
  { cellState :: s
  , cellStep  :: s -> a -> m (b, s)
  }
```

It is like a live program, but it also has inputs and outputs.
For example, this cell sums up all its inputs, and outputs the current sum:

```haskell
sumC :: (Monad m, Num a, Data a) => Cell m a a
sumC = Cell { .. } where
  cellState = 0
  cellStep accum a = return (accum, accum + a)
```

Using `Category`, `Arrow`, `ArrowLoop` and `ArrowChoice`,
we can compose cells to bigger data flow networks.
There is also support for monadic control flow based on exceptions.

## Use it and learn it

### Setup and GHCi integration

For the full fledged setup, have a look at the [`gears`](https://github.com/turion/essence-of-live-coding/tree/master/gears) example.
The steps are:

* Create a new cabal project containing an executable
  (preferably in a single file),
  and add `essence-of-live-coding` as a dependency.
* There are sound (PulseAudio) and 2d vector graphics (`gloss`) backend packages,
  `essence-of-live-coding-pulse` and `essence-of-live-coding-gloss`,
  respectively.
  They will require external libraries.
  If you use `stack` with `nix` integration,
  those will be installed for you automatically.
* There are custom [GHCi scripts](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#the-ghci-and-haskeline-files) that supply quick commands to start, reload and migrate the program automatically.
  * If you use no backend, you can copy the `.ghci` file to your project from [`essence-of-live-coding-ghci`](https://github.com/turion/essence-of-live-coding/blob/master/essence-of-live-coding-ghci/.ghci),
  and add that package to your dependencies.
  * If you want to use PulseAudio or `gloss`, depend on the respective package and copy the `.ghci` file.
  * For multiple backends, write your own `.ghci` file.
* Write a main program called `liveProgram`
* Launch `ghci` with e.g. `stack ghci`.
* Run your program with `:liveinit` (or `:livegloss`, or `:livepulse` depending on your backend).
* Edit your program, and reload with `:

### GHCi integration

### Examples

* The [`gears`](https://github.com/turion/essence-of-live-coding/tree/master/gears) example uses [`gloss`](http://gloss.ouroborus.net/) for graphics and PulseAudio for sound.
* There is a demo using the [WAI](https://www.stackage.org/package/wai) web server [`demos/app/DemoWai`](https://github.com/turion/essence-of-live-coding/blob/master/demos/app/DemoWai.hs).
* The [article](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#section.4) contains numerous examples,
  for example a [webserver](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#subsection.3.2),
  a [sine generator](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#subsection.4.3),
  and a [control flow example](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#subsection.5.2)
* There are [test cases](https://github.com/turion/essence-of-live-coding/tree/master/essence-of-live-coding/test) that show how the automatic migration works precisely in certain examples.

### Reading

* [ICFP 2019 presentation](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCodingPresentation.html).
  For a quick pitch.
* [Abstract](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCodingAbstract.pdf)
  For a 2-page overview.
* [Preprint article](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf).
  For all the details.
* [Appendix](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCodingAppendix.pdf).
  For all the details that were left out in the article.
* The [`dunai`](http://hackage.haskell.org/package/dunai) and [`rhine`](https://github.com/turion/rhine) packages.
  For backup material on how to program in this FRP dialect.
  (Spoiler: `rhine` is going towards live coding soon.)

## Best practice

In order to get the best out of the automatic migration,
it's advisable to follow these patterns:

* Use the FRP interface.
  (Section 4 in the [article](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#section.4),
  [`LiveCoding.Cell`](https://github.com/turion/essence-of-live-coding/blob/master/essence-of-live-coding/src/LiveCoding/Cell.lhs))
  It builds up the state type in a modular way that is migratable well.
* Develop _modularly_.
  Your top level cell should not be a long arrow expression,
  but it should call separately defined arrows.
  This makes the state more migratable,
  which is useful when you edit only in the "leaves" of your arrow "tree".
* Wherever you write `Cell`s from scratch,
  or use `feedback`,
  use records and algebraic datatypes to structure your state.
* Use `ghcid` in order to save yourself the hassle of having to reload manually all the time.
  Vanilla [`.ghci`](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html) and [`.ghcid`](https://github.com/ndmitchell/ghcid) file templates can be found in the root directory,
  and usually it suffices to copy these two files into your project and launch `ghcid` from there.
* When the automatic migration would fail, there are _user migrations_ ([`LiveCoding.Migrate.Migration`](https://github.com/turion/essence-of-live-coding/blob/master/essence-of-live-coding/src/LiveCoding/Migrate/Migration.hs)).
  Often, it's good practice to wrap your state in a newtype first
  (migration to newtypes is automatic),
  and then migrate this newtype.
* Use exceptions for control flow.
  (Section 5 in the [article](https://www.manuelbaerenz.de/essence-of-live-coding/EssenceOfLiveCoding.pdf#section.5)
  That way, you can keep the _control state_ of your program when migrating.

### Known limitations

* For custom migrations, you currently need to write your own `.ghci` file.
* If your program doesn't compile when you _enter_ GHCi,
  you will need to leave GHCi again and recompile.
  Otherwise the live coding infrastructure will not be loaded.
  After it has successfully compiled, you can reload from within GHCi,
  and further compilation errors do not affect the infrastructure.
* The template `.ghci` and `.ghcid` files assume that you are using a recent version of `cabal`
  (at least version 3).
  If this is not the case for you,
  consider updating, or adapt the files to use `new-`style commands.
