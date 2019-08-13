\begin{comment}
\begin{code}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.LiveProgram where

-- base
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Data

\end{code}
\end{comment}

\section{Change the program. Keep the state (as far as possible).}
\label{sec:core}
\fxerror{Split up even more into the individual modules and integrate more with the code}

Our model of a live program will consist of a state and an effectful state transition function.
A preliminary version is shown in Figure \ref{fig:LiveProgramPreliminary}.
\input{../src/LiveCoding/Preliminary/LiveProgram/LiveProgramPreliminary.lhs}
The program is initialised at a certain state,
and from there its behaviour is defined by repeatedly applying the function \mintinline{haskell}{liveStep} to advance the state and produce effects.
This is implemented in \mintinline{haskell}{stepProgram}.
%The type of the state should be encapsulated and thus invisible to the outside,
%it is through its effects that the live program communicates.
Since we want to run the program in a separate thread while compiling a new version of the program in the foreground,
we have to store the program in a a concurrent variable, here an \mintinline{haskell}{MVar}.
Given this variable, stepping the program it contains is a simple \mintinline{haskell}{IO} action,
implemented in \mintinline{haskell}{stepProgramMVar}.
To run the program,
we fork a background thread and repeatedly call \mintinline{haskell}{stepProgramMVar} there.

In a dynamically typed language,
such a setup is in principle enough to implement hot code swap.
At some point, the execution will be paused,
and the function \mintinline{haskell}{liveStep} is simply exchanged for a new one.
Then the execution is resumed with the new transition function,
which operates on the old state.
Of course, the new step function has to take care of migrating the state to a new format,
should this be necessary.
The difficulties arise
(apart from the practicalities of the implementation),
from the inherent unsafety of this operation:
Even if the old transition function behaved correctly,
and the old state is in a valid format,
the new transition function may crash or otherwise misbehave on the old state.
It is very hard to reduce the probability of such a failure with tests since the current state constantly changes
(by design).
A static typechecker is missing,
to guarantee the safety of this operation.

But let us return to Haskell,
where we have such a typechecker.
It immediately points out the unsafety of the migration:
There is no guarantee that the new transition function will typecheck with the old state!
In fact, in many situations, the state type needs to be extended or modified.

This kind of problem is not unknown.
In the world of databases,
it is commonplace that a table lives much longer than its initial schema.
The services accessing the data can change,
and thus also the requirements to the data format.
The solution to this problem is a \emph{schema migration},
an update to the database schema that alters the table in such a way that as little data as possible is lost,
and that the table adheres to the new schema afterwards.
Data loss is not entirely preventable, though.
If a column has to be deleted, its data will not be recoverable.
In turn, if a column is created, one has to supply a sensible default value (often \verb|NULL| will suffice).

\subsection{Migrating the state}

We can straightforwardly adopt this solution by thinking of the program state as a small database table with a single row.
Its schema is the type \mintinline{haskell}{s}.
Given a \emph{type migration} function,
we can perform hot code swap,
as shown in Figure \ref{fig:hot code swap}.
\input{../src/LiveCoding/Preliminary/LiveProgram/HotCodeSwap.lhs}
This may be an acceptable solution to perform a planned, well-prepared intervention,
but it does spoil the fun in a musical live coding performance if the programmer has to write a migration function after every single edit.
What a live performer actually needs,
is a function with this mysterious type signature:
\begin{spec}
hotCodeSwap
  :: LiveProgram m s'
  -> LiveProgram m s
  -> LiveProgram m s'
\end{spec}
It is the same type signature as in Figure \ref{fig:hot code swap},
but with the first argument, the manual migration function, removed.
The new program,
including its initial state,
has just been compiled,
and the old program is still stored in a concurrent variable.
Can we possibly derive the new state by simply looking at the initial state of the new program and the old state?
Is there a magical, universal migration function?
If there were, it would have this type:
\begin{spec}
migrate :: s' -> s -> s'
\end{spec}
A theoretician will probably invoke a free theorem \cite{wadler1989theorems} here,
and infer that there is in fact a unique such function:
\mintinline{haskell}{const}!
But it is not what we were hoping for.
\mintinline{haskell}{const s' s} will throw away the old state and run the program with the new initial state
-- effectively restarting our program from a blank slate.

In this generality, we cannot hope for any other solution.
But in the following, we are going to see how to tweak the live program definition by only twenty characters,
and arrive at an effective migration function.

\subsection{Type-driven migrations}
In many cases, knowing the old state and the new initial state is sufficient to derive the new, migrated state safely.
As an example, imagine the internal state of a simple webserver that counts the number of visitors to a page.
\fxwarning{Later show how migrate behaves on these examples}
\fxwarning{Lib: All examples should be in a separate directory, not in src. We should only have the final library in src.}
\fxwarning{Typecheck the example somehow? Put it in different files and make figures?}
\fxerror{Extend the example to start from Int and migrate into the newtype?}
\begin{spec}
data State = State { nVisitors :: Int }
\end{spec}
The server is initialised at 0,
and increments the number of visitors every step.
(For a full-fledged webserver,
the reader is asked to patiently wait until the next section.)
\begin{spec}
server = LiveProgram (State 0) $ \State { .. }
  -> State $ return $ nVisitors + 1
\end{spec}
\fxwarning{Show the different definitions of State as different modules by directly quoting different files as figures?
Would like to use same files like the wai demo (but hard because of Data).
Maybe rename them in order not to say Wai before we've introduced it.}
We extend the state by the name of the last user agent to access the server (initially not present):
\fxwarning{What if we add another constructor Bar here?
Could it still find out that there is a State constructor in the type?}
\begin{spec}
data State = State Int (Maybe ByteString)
initState = State 0 Nothing
\end{spec}
From just comparing the two datatype definitions,
it is apparent that we would want to keep the number of visitors,
of type \mintinline{haskell}{Int},
when migrating.
For the new argument of type \mintinline{haskell}{Maybe ByteString},
we cannot infer any sensible value from the old state,
but we can take the value \mintinline{haskell}{Nothing} from the new initial state,
and interpret it as a default value.
A general state migration function should specialise to:
\begin{spec}
migrate (Server1.State nVisitors)
        (Server2.State _         mUserAgent)
      =  Server2.State nVisitors mUserAgent
\end{spec}
Our task was less obvious if we would have extended the state by the last access time,
encoded as a UNIX timestamp:
\begin{spec}
data State = State Int Int
\end{spec}
Here it is unclear to which of the \mintinline{haskell}{Int}s the old value should be migrated.
It is obvious again if the datatype was defined as a record as well:
\begin{spec}
data State = State
  { nVisitors      :: Int
  , lastAccessUNIX :: Int
  }
\end{spec}
We need to copy the \mintinline{haskell}{nVisitors} field from the old state,
and initialise the \mintinline{haskell}{lastAccessUNIX} field from the new state.
(Conversely, if we were to migrate back to the original definition,
there is no way but to lose the data stored in \mintinline{haskell}{lastAccessUNIX}.)
Clearly, the record labels enabled us to identify the correct target field.
%The solution lies in the type,
%or rather, the datatype definition.
The solution lies in the datatype definition.

We can meta-program a migration function by reasoning about the structure of the type definition.
This is possible with the techniques presented in the seminal, now classic article ``Scrap Your Boilerplate'' \cite{syb}.
It supplies a typeclass \mintinline{haskell}{Typeable} which enables us to compare types and safely type-cast at runtime,
and a typeclass \mintinline{haskell}{Data} which allows us
%amongst many other features,
to inspect constructor names and record field labels.
Using the package \texttt{syb},
which supplies common utilities when working with \mintinline{haskell}{Data},
our migration function is implemented in under 50 lines of code,
with the following signature:
\begin{spec}
migrate :: (Data a, Data b) => a -> b -> a
\end{spec}
It handles the two previously mentioned cases:
Constructors with the same names,
but some mismatching arguments,
and records with some coinciding field labels,
but possibly a different order.
\fxerror{We can also do newtype wrappings}
In nested datatype definitions,
the function recurses into all children of the data tree.
Needless to say, if the types do match, then the old state is identically copied.
\fxwarning{Show examples here?}

Sometimes it is necessary to manually migrate some part of the state.
Assume, for the sake of the example,
that our webserver has become wildly popular,
and \mintinline{haskell}{nVisitors} is close to \mintinline{haskell}{maxInt}.
We need to migrate this value to an arbitrary precision \mintinline{haskell}{Integer}.
It is easy to extend \mintinline{haskell}{migrate} by a special case provided by the user:
\begin{spec}
userMigrate
  :: (Data a, Data b, Typeable c, Typeable d)
  => (c -> d)
  -> a -> b -> a

intToInteger :: Int -> Integer
intToInteger = toInteger
\end{spec}
In our example, we would use \mintinline{haskell}{userMigrate intToInteger} to migrate the state.
\fxwarning{Show example. Extend runtime.}

To use the automatic migration function,
we only need to update the live program definition to include the \mintinline{haskell}{Data} constraint,
as shown in Figure \ref{fig:LiveProgram}.
\fxwarning{Idea for elsewhere: LiveProgram m = forall s (m s, s -> m s), to ease initialisation}
\begin{figure}
\begin{code}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }
\end{code}

\input{../src/LiveCoding/LiveProgram/HotCodeSwap.lhs}

\caption{\texttt{LiveProgram.lhs}}
\label{fig:LiveProgram}
\end{figure}
This is a small restriction.
The \mintinline{haskell}{Data} typeclass can be automatically derived for every algebraic data type,
except those that incorporate \emph{functions}.
We have to refactor our live program such that all functions are contained in \mintinline{haskell}{liveStep}
(and can consequently not be migrated),
and all data is contained in \mintinline{haskell}{liveState}.

Now that we have a universal migration function,
it is not necessary to carry the type of the state around in the type signature.
In fact it would be cumbersome in combination with \mintinline{haskell}{MVar}s
(which can't change their type),
and a real burden when later modularising the state.
Consequently, the type is made existential.
The only necessary information is that it is an instance of \mintinline{haskell}{Data}.

\begin{comment}
\begin{code}
-- | 'mappend' here is _not_ the migration function! (Compare 'migrate'.)
--   This instance simply tuples both states and performs the steps sequentially.
instance Monad m => Semigroup (LiveProgram m) where
  LiveProgram state1 step1 <> LiveProgram state2 step2 = LiveProgram { .. }
    where
      liveState = (state1, state2)
      liveStep (state1, state2) = do
        state1' <- step1 state1
        state2' <- step2 state2
        return (state1', state2')

instance Monad m => Monoid (LiveProgram m) where
  mempty = LiveProgram () return
\end{code}
\end{comment}

\input{../src/LiveCoding/RuntimeIO.lhs}

\subsection{Live coding a webserver}

\fxwarning{Consider redoing this as a GHCi session where we call the server from within Haskell, e.g. with the curl or a HTTP package}

To show that live coding can be applied to domains outside audio and video applications,
let us realise the example from the previous section and create a tiny webserver using the WAI/Warp framework \cite{Warp}.
It is supposed to count the number of visitors,
and keep this state in memory when we change the implementation.

The boiler plate code, which is suppressed here,
initialises the Warp server,
uses \mintinline{haskell}{launch} to start our live program in a separate thread
\fxwarning{"using launch"}
and waits for user input to update it.

To save ourselves an introduction to Warp,
we will communicate to it via two \mintinline{haskell}{MVar}s,
which we need to share with the live program.
The textbook solution is to supply the variables through a \mintinline{haskell}{Reader} environment,
\begin{comment}
We have to generalise the definition of live programs once more,
to arbitrary monads.
The final version is given in Figure \ref{fig:LiveProgram}.
\end{comment}
which needs to supplied to the live program before execution.
This can be done by transporting the program along the \mintinline{haskell}{runReaderT} monad morphism.
A function \mintinline{haskell}{hoistLiveProgram} does this
(borrowing nomenclature from the \texttt{mmorph} \cite{mmorph} package).
\begin{comment}
Abstracting this operation, we need a utility that applies a monad morphism to a live program.
(Borrowing nomenclature from the \texttt{mmorph} package,
we call it \mintinline{haskell}{hoistLiveProgram}.)
\fxwarning{Try to merge with the previous figure. Why not parametrise by monads straight away? (My hoist explanation is also very clunky)}
\begin{figure}
\begin{spec}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }
\end{spec}
\begin{code}
hoistLiveProgram
  :: (forall a . m1 a -> m2 a)
  -> LiveProgram m1
  -> LiveProgram         m2
hoistLiveProgram morph LiveProgram { .. } = LiveProgram
  { liveStep = morph . liveStep
  , ..
  }
\end{code}
\caption{LiveProgram.lhs}
\label{fig:LiveProgram}
\end{figure}
\end{comment}
