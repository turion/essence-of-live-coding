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

Our model of a live program will consist of a state and a state transition function.
A preliminary version is shown in Figure \ref{fig:LiveProgram1}.
\input{LiveProgram/LiveProgram1.lhs}
The program is initialised at a certain state,
and from there its behaviour is defined by repeatedly applying the function \mintinline{haskell}{liveStep} to advance the state and produce effects.
The type of the state should be encapsulated and thus invisible to the outside,
it is through its effects that the live program communicates.
This is especially convenient if we want to run the program in a separate thread
(while compiling a new version of the program in the foreground).
We can store the program in an \mintinline{haskell}{MVar} and repeatedly call \mintinline{haskell}{stepProgram} from a background thread:
\fxerror{We really need to show the MVar code here and in the next paragraphs.
Of course, the migration won't quite work, we can only replace the whole program.
This is like migrating with `const`.
}
\fxerror{Demonstrate the runtime also with a GHCI session.}
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
\input{LiveProgram/HotCodeSwap.lhs}
This may be an acceptable solution to perform a planned, well-prepared intervention,
but it does spoil the fun in a musical live coding performance if the programmer has to write a migration function after every single edit.
What a live performer actually needs,
is a function with this mysterious type signature:
\begin{spec}
hotCodeSwap
  :: LiveProgram s'
  -> LiveProgram s
  -> LiveProgram s'
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
As an example, assume the old state were defined as:
\fxwarning{Later quote from examples}
\fxerror{Have an example that threads through the article. Introduce earlier}
\begin{spec}
data Foo = Foo { n :: Integer }
\end{spec}
\fxerror{Show the different definitions of Foo as different modules, either in the same file if this works or by directly quoting different files as figures.}
We now change the definition to:
\fxwarning{What if we add another constructor Bar here? 
Could it still find out that there is a Foo constructor in the type?}
\begin{spec}
data Foo = Foo Integer Bool
\end{spec}
Obviously, we would want to keep the \mintinline{haskell}{Integer} argument when migrating.
For the new \mintinline{haskell}{Bool} argument
we cannot infer any sensible value from the old state,
but we can take the \mintinline{haskell}{Bool} value from the new initial state,
and interpret it as a default value.

Our task was less obvious if the definition had been updated to:
\begin{spec}
data Foo = Foo Integer Integer
\end{spec}
Here it is unclear to which of the \mintinline{haskell}{Integer}s the old value should be migrated.
It is obvious again if the datatype was defined as a record as well:
\begin{spec}
data Foo = Foo { k :: Integer, n :: Integer }
\end{spec}
Clearly, the labels help us to identify the correct target field.
The solution lies in the type,
or rather, the datatype definition.

If we were to migrate back to the original definition,
there is no way but to lose the data stored in \mintinline{haskell}{k}.
This is also apparent just from the definition of the algebraic datatype!

We can meta-program a migration function by reasoning about the structure of the type definition.
This is possible with the techniques presented in the seminal, now classical article ``Scrap Your Boilerplate'' \cite{syb}.
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
as shown in Figure \ref{fig:LiveProgram2}.
\input{LiveProgram/LiveProgram2.lhs}
\fxerror{I've only made the state existential here now. Explain here and remove previous mention. (If this is too heavy a burden, move to the place where the monad is in the type signature.)}
This is a small restriction.
The \mintinline{haskell}{Data} typeclass can be automatically derived for every algebraic data type,
except those that incorporate \emph{functions}.
We have to refactor our live program such that all functions are contained in \mintinline{haskell}{liveStep}
(and can consequently not be migrated),
and all data is contained in \mintinline{haskell}{liveState}.

\subsection{Livecoding a webserver}

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
The textbook solution is to supply the variables through a \mintinline{haskell}{Reader} environment.
We have to generalise the definition of live programs once more,
to arbitrary monads.
The final version is given in Figure \ref{fig:LiveProgram}.
Additionally, the environment needs to supplied to the live program before execution.
This can be done by transporting the state along the \mintinline{haskell}{runReaderT} monad morphism.
Abstracting this operation, we need a utility that applies a monad morphism to a live program.
(Borrowing nomenclature from the \texttt{mmorph} package,
\fxrerror{Citation or link}
we call it \mintinline{haskell}{hoistLiveProgram}.)
\fxwarning{Try to merge with the previous figure. Why not parametrise by monads straight away? (My hoist explanation is also very clunky)}
\begin{figure}
\begin{code}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }

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
