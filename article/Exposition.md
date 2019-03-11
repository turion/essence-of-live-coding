Could write the final article as a big literate haskell containing the whole shown library maybe.
(Submodules contain the stuff that isn't shown)

# Introduction

The main point of this article is to extract the essence of what is live coding,
and to implement it in a type-safe and type-driven way in Haskell as a functional reactive programming framework.

Live coding comes in a variety of applications:
Music and visual arts, and hot code swap.
In arts, it often works by having a server that produces the actual media,
and a DSL that sends requests to the server like ``add a cell'', ''delete that cell', ``update that cell''.
So the server holds the state and infers a behaviour from it, and we're only allowed to update the state.
This kind of live coding is usually restricted to a domain,
because the server needs to have all possible behaviour implemented ahead of time.
With hot code swap (e.g. in Erlang/Elixir, and also Elm), the issue is different.
There we can write very general programs and exchange them.
In Erlang we have GenServers which keep some state and have some API that allows you to interact with that state.
What we generally do is the key mantra of live coding:

``Change the program, keep the state.''

This kind of thing is actually well-known from databases, there it's called a schema migration.
So what we're looking for here is some kind of reactive, stateful framework where we can hot-swap the program and perform a ``type migration'' on the state.

Often one uses dynamically typed languages (statically typed languages are rare) for ``real'' live coding,
and here is a reason why.
In Erlang you may change the API and how it interacts with the state.
When changing the program, the type of the state will change.
This means you typically need a migration function that updates the state,
and then you still have some danger of crashing the new program.
Changing the state is a no-go in statically typed languages at first because the new program won't compile with the old state type.
But we'll see how to get around this and get a migration for free.
This only works in a language with a proper type system like Haskell,
you won't get this in a hobby project like Elm because they don't have type classes.

Then we'll package everything up into an arrowized FRP framework and add some simple control flow.
We develop a simple wrapper and an example application.

# Change the program, keep the state

Let's assume our main loop programs will look like this:

TODO: Rewrite with record syntax
```
data LiveProg where
  LiveProg :: s -> (s -> IO s) -> LiveProg
```
Equivalently:
```
data LiveProg where
  LiveProg :: s -> StateT s IO () -> LiveProg
```
We have an initial state and a state modification function.
(Actually we might take other monads than IO in the end?)
The intended behaviour is stepping infinitely:
```
run :: LiveProg -> IO ()
run (LiveProg s step) = go s
  where
    go s_ = do
      s' <- step s_
      go s'
```
We could pack one of them `LiveProg`s in an `MVar`, get it out, run a step, and put it back,
and like this share the program with other threads, such as the interactive editor's thread.
Then the idea is in principle that another thread will wait until the prog is put in the `MVar`,
take it, modify the program and leave the state.
But GHC has no way of knowing the type of the state (because it's existential),
and typically it will be different than before.

## Keep the state if you can

Still in many cases it makes sense to keep the state.
For example, if the type stays the same, we should just keep it.
So our first measure would be to add `Typeable` as a constraint and cast if we can.
(Does this even work?)

But even if it changes only slightly,
we'd like to keep it as much as we can.
Say we have:
```
data State = State
  { ballHeight :: Double
  , ballVel :: Double
  }
  deriving Show
step State {..} = do
  let newState = if ballHeight > 0
    then State { ballHeight + ballVel, ballVel - 1 }
    else State { -ballHeight, -ballVel }
  print newState
  return newState
prog = LiveProg State { 10, 0 } step
```
This accelerates and bounces on the floor.
Say we want to add a bounce counter:
```
data State = State
  { ballHeight :: Double
  , ballVel :: Double
  , counter :: Integer
  }
  deriving Show
step State {..} = do
  let newState = if ballHeight > 0
    then State { ballHeight + ballVel, ballVel - 1, counter }
    else State { -ballHeight, -ballVel, counter + 1 }
  print newState
  return newState
prog = LiveProg State { 10, 0, 0 } step
```
When updating the prog, we'd like to keep `ballHeight` and `ballVel`, but can't:
We recompile and then the types are different and can't be casted.
But from the representation (type name, constructor name, record fields),
it's obvious which data could be preserved and which can't.
We still need to do something about the counter,
we'll need to initialise it with the new initial value since we have no other way of guessing a value.
TODO Reorder this possibly?

Abstractly we're looking for something like this:

```
migrate :: a -> b -> a
```
`migrate aNew bOld` takes a new initial state `a` and tries to update with the old state `b`,
preserving as much information as can be cast to `a`.
Whatever can't be preserved needs to be thrown away or taken from `a`.
But that can't work in all generality.
Pretty sure that the only function satisfying this type signature is `const`!
But we need to restrict `a` and `b` anyways.
We want to inspect the state in some way in order to decide how much can stay and what has to change.

## Inspecting the state

How to do that in a statically typed language?

Dynamical typed languages are good at certain things that statically typed languages first seem horrible at:
Introspection of terms and their types, arbitrary casting of terms to other types.
But it's actually not so bad.
These things can usually be done by using syb or generics or some such thing.
And in fact, we can just demand that `s` satisfy the the `Data` type class and be on our way.
We then compare the types, constructors and record fields by their names and keep everything that sounds the same.
[Explanation in code and examples]
Since this is all syb, users of the library can later add their own migrations.
Hmm maybe we want to offer a few helper functions for that?

## Execution

We now put the live program in an mvar and then we have a thread that takes it from there, executes one step, and puts it back.
This thread can run in the background.
In the foreground, we keep this mvar and allow the user to update it, automatically migrating the state.
We can also offer debug hooks that can be executed whenever the program is exchanged, or when a step is performed.
These can inspect the state my means of generic programming `Data`.
They can even interrupt the execution temporarily and allow you to interfere with the state.

## A first example

...

# Making this into an FRP framework

Of course noone wants to write out the state manually.
Haskell is a functional language after all,
so the central object of study should be some kind of function.
Obviously, a live coding framework is a reactive framework,
so it makes sense to build an effectful FRP framework.
So we want some signal processing with reusable components.

Instead of sharing a global state (which is unsafe und unwieldy),
we _encapsulate_ state in each component.
So an FRP component is a stateful, effectful function, `a -> StateT s m b`.
Together with its current state,
we can encapsulate it by making `s` existential,
so the state isn't visible to the outside anymore.
```
data Cell m a b where
  Cell :: Data s => s -> (a -> StateT s m b) -> Cell m a b
```
TODO I think it's easier to expand the StateT in order not to confuse with later effects.
This is an effectful Mealy machine,
and we'll show now that it is the basic building block of a an arrowized FRP framework.

## Arrowized FRP

Arrowized FRP is cool yadda yadda.
Dunai shows that monadic FRP works.
`Cell` is an `Arrow`, an `ArrowChoice` and whatnot.
`Arrow` means that we can compose the signal processing components,
`ArrowChoice` means that we have control flow (as we'll see later in detail).
This means we can modularly build our program from deterministic building blocks,
while the state is encapsulated/private, so the cells behave nearly like functions.
We can do simple signal processing with feedback,
and, with the right monads, even some sort of games.

We recover:
```
type LiveProg = Cell IO () ()
```
To reach this type, we generate input with `Cell m () a` and consume it with `Cell m b ()`,
transforming all data into side effects.
Then we handle all effects in `m` until we have `IO` left.

A simple example:
...

## Finality and Dunai (and arrowized FRP like Yampa, Rhine)

Recall:
```
data MSF m a b = MSF (StateTransition m a b (MSF m a b))
```
We can do FRP and what not with it.

This is the final coalgebra of:
```
type StateTransition m a b s = a -> m (b, s)
```
A usual coalgebra is simply:
```
type Step m a b s = s -> StateTransition m a b s
```
TODO Can introduce these two types already at the beginning.
So that's just the stepper function from before.
Finality means that whenever we have `step :: Step m a b s` we can create an `MSF` from an `s`.
In other words:
```
finality' :: s -> Step m a b s -> MSF m a b
finality :: Cell m a b -> MSF m a b
```
Also, `MSF m a b` is still a coalgebra,
which means there is a canonical function `unMSF :: Step m a b (MSF m a b)`
and thus there would be canonical function
```
coalgebra :: Data (MSF m a b) => MSF m a b -> Cell m a b
coalgebra msf = Cell msf unMSF
```
Which suggests that we should be able to do basically everything that `MSF`s can with `Cell`s,
except that we'll never have such a `Data` instance because it doesn't make sense to give functions one.
So `MSF`s are a bit more general than our `Cell`s,
by exactly the `Data` constraint on the internal state.

## Live FRP
We chuck in a few extra pieces in order to make transitions from `a` (or b) to `a >>> b` easier,
and the same for other combinator changes.

# Control flow

## ArrowChoice

It's not switching control flow, but a little less general.
We have to redecide every tick into which branch we go.
The control flow effect happens in the input,
not in the monad.

## Dunai/Yampa
For Dunai, control flow is one of the coolest inventions.
It replaces Yampa's switch and is really important and handy.
We handle the `Either` effect.
The `MSF` becomes a monad in the exception type.

## Live coding vs control flow, or: Monads vs Arrows & Applicatives
But here it's hard.
Take this example:
...
Assume we're in the middle of this control flow and then we have to replace by a new `Cell`.
We can't simply do that because we'd have to go through the different decisions and fiddle the state through.
This would mean re-simulating the whole program until this point.
Also, if we look at how to implement bind we see that the internal state of the `Cell` would depend on the exception value.
That's something the compiler can't deal with.
So it's clear that we won't be able to do full-fledged monadic control flow.
As so often (parsing efficiently (regular expressions), stream processing, async/par),
monads are simply too strong.

## Live bind

Look at the type of `(>>=)`:
```
(>>=) :: m e1 ->        (e1 -> m e2) -> m e2
      == m e1 -> ReaderT e1    m e2  -> m e2
```
So we can interpret bind as handling a `Reader` effect.
Remember that for arrows, a `Reader` effect is just adding another input.
What we can do is this:
```
-- TODO Is it worth calling it Reader? We could also write it out as Cell m (e, a) b
(>>>=) :: Cell (ExceptT e1 m) a b -> Cell (ReaderT e1 (ExceptT e2 m)) a b -> Cell (ExceptT e2 m) a b
```
You can think of this as bind except that the second argument is not a function, but rather an `Arrow` input,
as seen in this special case:
```
LiveProg (ExceptT e1 m) -> LiveProg (ReaderT e1 (ExceptT e2 m)) -> LiveProg (ExceptT e2 m)
Cell (ExceptT e1 m) () b -> Cell (ExceptT e2 m) e1 b -> Cell (ExceptT e2 m) () b
```
This is strictly less general than `(>>=)` because we have `Cell (ReaderT r m) a b -> ReaderT r (Cell m a b)`,
but not the other way around, since `Cell (ReaderT r m) a b` accepts a bigger class of inputs:
`r` may vary at every tick.
(This is ironic because we're not using it to vary `r` at every tick.)

But it's still more general than `Applicative`s,
we get `Applicative` back from this.

## Applicative control flow

Effects encoded in `Applicative`s can be sequentialised.
(That's the (lax) monoidality of the functor.)
Since it requires `Functor`, we can apply functions to the effect output (the exceptions).
So we can run a lot of cells after each other,
collect their exceptions and compute value and return that,
like a final exit code.
The one thing that `Applicative`s don't allow is choosing the later cells depending on the exceptions.

We can build this from `(>>>=)`:
```
andThen :: Data e1 => Cell (ExceptT e1 m) a b -> Cell (ExceptT e2 m) a b -> Cell (ExceptT (e1, e2) m) a b
andThen cell1 cell2 = cell >>>= hoistCell readException
  where
    readException :: ExceptT e2 m x -> ReaderT e1 (ExceptT (e1, e2) m) x
    readException e2x = ReaderT $ \e1 -> withExceptT (e1, ) e2x
```
It first runs the first cell, throwing an `e1`.
Then it uses the `ReaderT e1` environment in the second cell to read the first exception and throw it together with the second.

You might think at first that we'll never build a `(<*>)` from this because that takes a function as first argument and functions don't have `Data`.
But with a simple record, we make this into an `Applicative`:
```
data CellExcept m a b e where
  CellExcept ::              Data e' =>
    { cellExcept :: Cell (ExceptT e' m) a b
    , fmapExcept ::               e'  ->    e
    } -> CellExcept                  m  a b e
```
TODO: Possible other names: `Mode`
We reify its `Functor` by carrying around a function that `fmap`s the exception to any type,
even a function type.
This way we can still make sure that the thrown exception is `Data`.

This is now `Applicative`.
With `-XApplicativeDo`, you can use `do`-notation,
as long as you don't use the exceptions to calculate new `CellExcept`s.
The mere `Applicative` now allows us to sequentialise several "modes" through which the program steps,
and at the end calculate a return code.
This can already be run.

TODO: Example

But it's not very exciting control flow yet,
because we want to branch, depending on which exception was thrown.

## With ArrowChoice

But with `ArrowChoice`, this isn't so bad.
We can branch "temporarily" in the second argument, using the exception environment.
So in total we can do control flow like this:
Throw an exception, switch to the next cell and in there, branch depending on the value of the exception.
Like this:
```
eitherS :: Cell (ReaderT eL m) a b -> Cell (ReaderT eR m) a b -> Cell (ExceptT (Either eL eR) m) a b -> Cell m a b
```
This gives rise to an `ifS`.
Or you can just use the `if` expression inside `Arrow`.
TODO: Example

Trouble was, `ReaderT e` and `\m -> Cell m a b` do not commute,
i.e. `ReaderT e (Cell m a b)` is not isomorphic to `Cell (ReaderT e m) a b`.
Worse even, there is a map `Cell (ReaderT e m) a b -> ReaderT e (Cell m a b)`,
but not the other way around in general.

But imagine that `e` is _finite_ and satisfies `Data`.
Say we have `handler :: e -> Cell m a b` given.
With `ArrowChoice`, we can lazily instantiate every possible `handler e` and fan them next to each other.
Then the live input in `Cell m (e, a) b` can decide which of the choices to go down.
Because of the existential state type,
GHC has to be able to type check each state and verify its `Data` instance,
that's why `e` needs to be finite.
With dependent types, we could just say that the existential state in `handler e` simply depends on `e`,
but unfortunately we can't, so we have to go through type classes that do that for us.
Luckily, we have GHC Generics, and I've implemented this function for products and sums,
so you can derive it with a single line of boiler plate for any non-recursive algebraic data type.

## Loop

We could just `fmap fromLeft $ runExceptT $ reactimate prog` to run our cell until it throws an exception, and return it.
The last exception can be thought of as some kind of return code.
But that's often not what we want.
Often, in live coding, we want to have some kind of loop through control flow.

If we try `mainProg = doSomething *> mainProg`,
it goes horribly wrong.
TODO: Why exactly?
But we couldn't really expect this to work since this is a space leak.
Remember that `*>` brings the exception from its first argument into the scope of the second,
so this would remember all exceptions from the very first one.
We have to explicitly forget them.
But sometimes we want to pass on some state to the next iteration of the loop.
We do this by passing the last exception explicitly:
```
forever :: Cell (ReaderT e (ExceptT e m)) a b -> e -> Cell m a b
forever :: Cell (ReaderT e (ExceptT e m)) a b -> Reader e (Cell m a b)
```
They commute, so order doesn't matter.
\begin{spec}
ExceptT e (ReaderT r m) a
== ReaderT r m (Either e a)
== r -> m (Either e a)
ReaderT r (ExceptT e m) a
== r -> ExceptT e m a
== r -> m (Either e a)
\end{spec}
After an `e` has been thrown,
it's put into the `ReaderT` environment
and the `Cell` is restarted.
It is the user's responsibility that `e` is calculated in a forcing way,
and actually used,
to avoid bigger and bigger thunks building up.

# Live coding

## Execution

We have a simple

## Example

Putting everything together,
we have this cute little program now:

TODO

Now we change the code:

TODO

When we reload, this cool stuff happens:

...

# Outlook (if this is even needed)

* We could maybe make the MVar bit faster
* It's possible to bind to `Bool` and it should be possible to bind to any finite, serialisable value.
  So we could make a type class and a generic implementation and then do `RebindableSyntax` to sneak in our new bind in `do`-notation.
