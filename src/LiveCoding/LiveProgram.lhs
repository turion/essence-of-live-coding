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

-- essenceoflivecoding
import LiveCoding.Migrate

\end{code}
\end{comment}

\section{Change the program. Keep the state (as far as possible).}
\label{sec:core}
\fxerror{Split up even more into the individual modules and integrate more with the code}

Our model of a live program will consist of a state and an effectful state transition function.
A preliminary version is shown in Figure \ref{fig:LiveProgramPreliminary}.
\input{../src/LiveCoding/LiveProgram/Preliminary/LiveProgramPreliminary.lhs}
The program is initialised at a certain state,
and from there its behaviour is defined by repeatedly applying the function \mintinline{haskell}{liveStep} to advance the state and produce effects.
The type of the state should be encapsulated and thus invisible to the outside,
it is through its effects that the live program communicates.
This is especially convenient if we want to run the program in a separate thread
(while compiling a new version of the program in the foreground).
We can store the program in an \mintinline{haskell}{MVar} and repeatedly call \mintinline{haskell}{stepProgram} from a background thread.
\fxerror{We really need to show the MVar code here and in the next paragraphs.
Of course, the migration won't quite work, we can only replace the whole program.
This is like migrating with `const`.
}
\fxerror{Demonstrate the runtime also with a GHCI session.}
\fxerror{The following sentence maybe makes no sense?}
Its result is the new, encapsulated state of the program,
which is to be stored in the \mintinline{haskell}{MVar} again.

In a dynamically typed language, this would in principle be enough to implement hot code swap.
At some point, the execution will be paused,
and the function \mintinline{haskell}{liveStep} is simply exchanged for a new one.
Then the execution is resumed with the new transition function,
which operates on the old state.
Of course, the new step function has to take care of migrating the state to a new format,
should this be necessary.
The difficulties arise
(apart from the practicalities of the implementation),
in the inherent unsafety of this operation:
Even if the old transition function behaved correctly,
and the old state is in a valid format,
the new transition function may crash or otherwise misbehave on the old state.
It is very hard to reduce the probability of such a failure with tests since the current state constantly changes
(by design).
A static typechecker is sorely missing,
to guarantee the safety of this operation.

But let us return to Haskell,
where we have such a typechecker.
Now we encounter a problem at an earlier stage:
\fxwarning{Or instead of the last sentence: ``It immediately manifests the problem/unsafelty'' or so. ``It will immediately point out the unsafetye of the migration.''}
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
\input{../src/LiveCoding/LiveProgram/Preliminary/HotCodeSwap.lhs}
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
The new program (including its initial state!)
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
But it is hardly what we were hoping for.
\mintinline{haskell}{const s' s} will simply throw away the old state and run the program with the new initial state
-- effectively restarting our program from a blank slate.

In this generality, we cannot hope for any other solution.
But in the following, we are going to see how to tweak the live program definition by only twenty characters,
and arrive at an effective migration function.

\subsection{Type-driven migrations}
\fxerror{This is the essence of the article. Show explicitly what the migration function would do.}
In many cases, knowing the old state and the new initial state is sufficient to derive the new, migrated state safely.
As an example, imagine the internal state of a simple webserver that counts the number of visitors to a page.
\fxwarning{Later show how migrate behaves on these examples}
\fxwarning{In general: All examples should be in a separate directory, not in src. We should only have the final library in src.}
\fxwarning{Typecheck the example somehow? Put it in different files and make figures?}
\fxwarning{Possible example for user migration: Start with Int and upgrade to Integer}
\begin{spec}
data State = State { nVisitors :: Integer }
\end{spec}
The server is initialised at 0,
and increments the number of visitors every step.
(For a full-fledged webserver,
the reader is asked to patiently wait until the end of this section.)
\begin{spec}
server = LiveProgram (State 0) $ \State { .. }
  -> State $ return $ nVisitors + 1
\end{spec}
\fxerror{Show the different definitions of State as different modules by directly quoting different files as figures.
Use same files like the wai demo. Maybe rename them in order not to say Wai before we've introduced it.}
We extend the state by the name of the last user agent to access the server (initially not present):
\fxwarning{What if we add another constructor Bar here? 
Could it still find out that there is a State constructor in the type?}
\begin{spec}
data State = State Integer (Maybe ByteString)
initState = State 0 Nothing
\end{spec}
From just comparing the two datatype definitions,
it is apparent that we would want to keep the number of visitors,
of type \mintinline{haskell}{Integer},
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
data State = State Integer Integer
\end{spec}
Here it is unclear to which of the \mintinline{haskell}{Integer}s the old value should be migrated.
It is obvious again if the datatype was defined as a record as well:
\begin{spec}
data State = State
  { nVisitors      :: Integer
  , lastAccessUNIX :: Integer
  }
\end{spec}
We need to copy the \mintinline{haskell}{nVisitors} field from the old state,
and initialise the \mintinline{haskell}{lastAccessUNIX} field from the new state.
Clearly, the record labels enabled us to identify the correct target field.
The solution lies in the type,
or rather, the datatype definition.

If we were to migrate back to the original definition,
there is no way but to lose the data stored in \mintinline{haskell}{lastAccessUNIX}.
This is also apparent just from the definition of the algebraic datatype.

We can meta-program a migration function by reasoning about the structure of the type definition.
This is possible with the techniques presented in the seminal, now classic article ``Scrap Your Boilerplate'' \cite{syb}.
It supplies a typeclass \mintinline{haskell}{Typeable} which enables us to compare types and safely type-cast at runtime,
and a typeclass \mintinline{haskell}{Data} which allows us,
amongst many other features,
to inspect constructor names and record field labels.
Using the \verb|syb|-package, which supplies common utilities when working with \mintinline{haskell}{Data},
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
In nested datatype definitions,
the function recurses into all children of the data tree.
Needless to say, if the types do match, then the old state is identically copied.
\fxerror{Not sure whether I know how to insert user migrations in this}
\fxerror{This works also across recompilations! Demonstrate in the following!}
To use this migration function,
we only need to update the live program definition to include the \mintinline{haskell}{Data} constraint,
as shown in Figure \ref{fig:LiveProgram}.
\begin{figure}
\begin{code}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }

hotCodeSwap
  :: LiveProgram m
  -> LiveProgram m
  -> LiveProgram m
hotCodeSwap
  (LiveProgram newState newStep)
  (LiveProgram oldState _)
  = LiveProgram
  { liveState = migrate newState oldState
  , liveStep  = newStep
  }
\end{code}
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
it is not necessary to carry the type of the state around.
(In fact it would be a burden when later trying to modularise this approach.)
Consequently, it is made existential.
The only necessary information is that it is an instance of \mintinline{haskell}{Data}.

\input{../src/LiveCoding/RuntimeIO.lhs}

\subsection{Livecoding a webserver}

\fxwarning{Consider redoing this as a GHCi session where we call the server from within Haskell, e.g. with the curl or a HTTP package}

To show that live coding can be applied to domains outside audio and video applications,
let us create a tiny webserver using the WAI/Warp framework \fxfatal{Cite}.
It is supposed to count the number of visitors,
and keep this state in memory when we change the implementation.

The boiler plate code, which is suppressed here,
initialises the Warp server, launches our live program in a separate thread
\fxwarning{"using launch"}
and waits for user input to update it.

To save ourselves an introduction to Warp,
we will communicate to it via two \mintinline{haskell}{MVar}s,
which we will need to share with the live program.
The textbook solution is to supply the variables through a \mintinline{haskell}{Reader} environment,
\begin{comment}
We have to generalise the definition of live programs once more,
to arbitrary monads.
The final version is given in Figure \ref{fig:LiveProgram}.
\end{comment}
which needs to supplied to the live program before execution.
This can be done by transporting the program it along the \mintinline{haskell}{runReaderT} monad morphism.
For this, the library supplies the utility \mintinline{haskell}{hoistLiveProgram}
(borrowing nomenclature from the \texttt{mmorph} package).
\fxrerror{Citation or link}
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
