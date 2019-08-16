\documentclass{essence}
% Make sure to switch off ACM stuff

\setlength{\belowcaptionskip}{-2pt}

\begin{document}
\title{The essence of live coding}
\subtitle{Change the program, keep the state!}
\author{Manuel Bärenz}

\begin{abstract}
I present a general-purpose and type safe live coding framework in Haskell.
The internal state of the live program is automatically migrated when performing hot code swap.
The approach is then generalised to an easy to use FRP interface.
It is parametrized by its side effects,
separates data flow cleanly from control flow,
and allows to develop live programs from reusable, modular components.
Useful utilities for debugging and quickchecking are presented.
\end{abstract}

\maketitle

\section{Introduction}
For our purposes, a live coding framework has to allow a program to be updated, recompiled and reloaded,
without interrupting its execution.
The essential idea can be summed up in the motto of live coding:
\begin{center}
\textbf{Change the program. Keep the state.}
\end{center}
In a dynamically typed language like Erlang \cite{armstrong2013programming},
this is conceptually simple, but error-prone and tedious:
There is no guarantee that the current state of the old program is compatible with the new program.
Usually, one has to manually migrate the state to a new schema,
but one cannot rule out failure.
In webserver applications, the risk of runtime errors from incorrect state migrations may well outweigh the benefits.

The solution to avoid runtime errors is to employ a type checker.
What is more, we will see how it is possible to arrive at an \emph{automatic} migration function:
By generic, type-driven programming.

Typed, automatically migrating live coding frameworks already exist \cite{wilson2011supercollider},
even for Haskell \cite{mclean2014tidal},
but they are typically restricted to a particular domain such as audio or video.
Here, an effect-polymorphic, universal live coding framework is developed.
By being parametrised over arbitrary monads,
it can connect to many backends and external libraries.
Consequentially, the framework can be used in virtually any domain were live coding makes sense.

From an API perspective, Essence Of Livecoding follows the arrowized Functional Reactive Programming (FRP) viewpoint,
in particular Dunai \cite{Dunai} and Rhine \cite{Rhine}.
For the library user, this is essential in order to build programs modularly from reusable components,
and to separate data flow from control flow.
It is also essential from an implementation perspective,
for two corresponding purposes:
To build up state types modularly which greatly facilitates automatic and generic state migration,
and to be able to migrate the control state\footnote{%
A hard problem according to \href{https://elm-lang.org/blog/interactive-programming}{https://elm-lang.org/blog/interactive-programming}.
}
(the information which branch of the program is currently active).

\begin{figure}
\includegraphics[width=\linewidth]{gears.png}
\caption{Example with Gloss and PulseAudio}
\end{figure}
In the demonstration, I show in more detail how to live code a \texttt{gloss} program,
and add or remove certain features from it.
This program is then linked to a simple sound synthesizer using the PulseAudio backend.

The talk slides and a full article version are available at \href{https://www.manuelbaerenz.de/}{https://www.manuelbaerenz.de/}.
The source repository containing the library and examples
(console, audio, video, web servers) is available at \href{https://github.com/turion/essence-of-live-coding}{https://github.com/turion/essence-of-live-coding}.

\section{Change the program. Keep the state (as far as possible).}

Our model of a live program consists of a state and an effectful state transition (``step'') function,
shown in Figure \ref{fig:LiveProgram}.
\begin{figure}
\begin{code}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }
\end{code}
\caption{Definition of live programs}
\label{fig:LiveProgram}
\end{figure}
The semantics of a live program is given by the side effects resulting from repeatedly applying \mintinline{haskell}{liveStep} to its current state,
which is initialised with \mintinline{haskell}{liveState}.
We hide the state type from the type signature by making it existential,
since it will typically change over the course of a live coding session.
The \mintinline{haskell}{Data} constraint will later be the crucial ingredient for a generic state migration function.

A live coding session is started via GHCi, into which we load a file containing a live program.
We store it in an \mintinline{haskell}{MVar} and launch a separate thread that steps the state repeatedly and stores it in the \mintinline{haskell}{MVar} again.
When we have edited the file, we reload it from GHCi,
making use of the \texttt{foreign-store} \cite{foreign-store} package to persist the concurrent variable.
Now, we would like to update the state transition function \mintinline{haskell}{liveStep} in the live program,
while keeping the current state, as far as possible.
Here lies the fundamental challenge of live coding:
The new program does not match the old state type.

The key insight is that we can use the datatype definition of the state to generically derive the correct migration.
Assume, for example, that the state of some webserver is defined as:
\begin{spec}
data State = State { nVisitors :: Int }
\end{spec}
And after reloading,
a new record field was added:
\begin{spec}
data State = State
  { nVisitors  :: Int
  , lastAccess :: UTCTime
  }
\end{spec}
Clearly, when migrating the old state to the new datatype,
we want to preserve the \mintinline{haskell}{nVisitors} field.
For \mintinline{haskell}{lastAccess} on the other hand, we cannot compute any sensible value.
and thus have to initialise this field from the initial state of the new program.
By reasoning about record fields and their types,
we were able to find the best state migration.
Similar generic criteria include matching constructor names, matching builtin types, and automatic newtype wrapping.
All of these can be implemented in less than 100 lines using the \mintinline{haskell}{Data} type class from \cite{syb},
as a function of this type signature:
\begin{spec}
migrate :: (Data a, Data b) => a -> b -> a
\end{spec}
It receives the new initial state and the old current state,
and tries to migrate the old state as far as possible to the new state type.
Wherever the automatic migration would perform suboptimally
-- as were the case if we wanted to migrate \mintinline{haskell}{nVisitors} from \mintinline{haskell}{Int} to \mintinline{haskell}{Integer} --
it is possible to extend by a special case provided by the user.

\section{Live coding as arrowized FRP}

Writing out the complete state of the live program explicitly and separating its state from the step function is tedious.
Instead, we want to develop modularly,
and an arrowized FRP interface will allow us to do so.
The live program definition is generalized to ``cells''\footnote{
Cells are the building blocks of everything live.},
shown in Figure \ref{fig:cell}.
\begin{figure}
\begin{code}
data Cell m a b = forall s . Data s => Cell
  { cellState :: s
  , cellStep  :: s -> a -> m (b, s)
  }
\end{code}
\caption{The definition of a live coding cell}
\label{fig:cell}
\end{figure}
Additionally to a state and a step function,
cells also have an input type \mintinline{haskell}{a}
and an output type \mintinline{haskell}{b}.
They can be composed sequentially,
by feeding the output of one cell as input into another cell.
By being instances of the \mintinline{haskell}{Arrow} type class,
they can also be composed in parallel,
giving rise to clear data flow declarations through the arrow syntax extension.
The migration function has special cases for the state types of composed cells,
making FRP cells suitable for live coding.

As a simple example, consider the following \mintinline{haskell}{Cell} which adds all input and returns the delayed sum each step:
\begin{code}
sumC :: (Monad m, Num a, Data a) => Cell m a a
sumC = Cell { .. } where
  cellState = 0
  cellStep accum a = return (accum, accum + a)
\end{code}
Cells may also create side effects in a monad.
A cell of type \mintinline{haskell}{Cell IO () a} produces data,
using the \mintinline{haskell}{IO} monad,
while \mintinline{haskell}{Cell IO a ()} consumes data.
Composing effectful data producers with data processing cells,
and finally with effectful consumers,
we recover live programs as the special case of trivial input and output.

Using the \mintinline{haskell}{ExceptT} monad transformer,
we also provide a monadic control flow interface based on type-safe exceptions.
It enables the library user to permanently switch from one cell to another,
triggered by events thrown anywhere within the cell.
The crucial advantage of embedding control flow into cell states as an effect is that the migration function preserves the current control flow state
(e.g., the information to which cell we currently switched)
out of the box.

\section{Tooling}

For ease of use, custom GHCi commands are supplied that start a live program in a separate thread and allow reload it when it is edited.
These cover ordinary live programs in \mintinline{haskell}{IO},
but also video and audio backends.
Utilities for integration with other external loops are given.

It is easy to add debugging functionality to the framework,
e.g. displaying the state or changing it after interacting with the user.
In short, a debugger is itself a live program that can, as an effect,
read and modify the state of an arbitrary other live program,
i.e. it is of this type:
\begin{spec}
forall s . Data s => LiveProgram (StateT s m)
\end{spec}
As examples, there are debuggers printing the current state to the console,
displaying it graphically via \texttt{gloss} \cite{Gloss},
or pausing the execution upon user interaction.

Testing live programs or cells with arbitrary input using QuickCheck \cite{quickcheck} before reloading is often sensible.
By collecting test results of components in a writer monad,
we can modularly check properties of intermediate data.
Thanks to the \mintinline{haskell}{Data} constraint,
cells and live programs can be tested by generating arbitrary \emph{state}.

\clearpage
\bibliography{EssenceOfLiveCoding.bib}
\end{document}
