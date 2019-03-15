\documentclass[sigplan,10pt,screen]{acmart}

\usepackage[utf8]{inputenc}
\usepackage{minted}
\usepackage[draft]{fixme}
\usepackage{hyperref}

\bibliographystyle{ACM-Reference-Format}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

\begin{document}
\title{The essence of live coding: Change the program, keep the state!}
\subtitle{Functional pearl}

\author{Manuel Bärenz}
\orcid{0000-0003-1843-0773}             %% \orcid is optional
\affiliation{
  \institution{sonnen eServices GmbH}            %% \institution is required
  \country{Deutschland}
}
\email{programming@manuelbaerenz.de}

\begin{abstract}
  One rarely encounters programming languages and frameworks that provide general-purpose and type-safe hot code swap.
  It is demonstrated here that this is entirely possible in Haskell,
  by faithfully following the motto of livecoding:
  ``Change the program, keep the state.''
  With generic programming,
  one easily arrives at an automatic state migration function.
  The approach can be generalised to an arrowized Functional Reactiv Programming framework that is parametrized by its side effects.
  It allows for building up complete live programs from reusable, modular components,
  and to separate data flow cleanly from control flow.
\end{abstract}

\maketitle

\section{Introduction}

Livecoding has come to denote various concepts,
from hot code swap on a production server to artistic performances with code that is written, executed and updated live.
From the implementor's perspective,
the common denominator is a framework to update the definition of a program while not interrupting its execution.

In a dynamically typed language with a virtual machine, such as Erlang \cite{armstrong2013programming},
hot code swap is conceptually simple:
A new server is started, the state of the old server is transferred to the new one, and the old server is shut down.
The challenge lies in ensuring that the state is correctly migrated,
as corrupt state might crash the server.
This is a nontrivial task if no type checker is available to verify if the migrated state conforms to the schema required by the new server.

Domain specific live coding frameworks \cite{wilson2011supercollider}
usually have a central server that generates the desired effect
-- be it audio, video or device communication --
and offers an API through which users can create ``cells'',
such as oscillators or signal processors in the case of audio,
and connect, reorder, update or destroy them during execution.
The central server guarantees safe updates,
and strongly typed user-side libraries such as Tidal \cite{mclean2014tidal} exist,
but ultimately the user is restricted to a domain specific language.

In this article, we implement a lightweight general purpose livecoding framework in Haskell from scratch.
It is not only type-safe, but also type-driven, in that boilerplate code for state migrations
(which is hard to get right without a static type checker)
is automatically derived from the type.
It is not restricted to a particular domain, by virtue of being parametrised over an arbitrary monad:
Any domain- or library-specific effect can be incorporated effortlessly,
and handled with standard Haskell functions.
The framework follows the tradition of monadic arrowized Functional Reactive Programming (FRP) as developed in \cite{Dunai} and \cite{Yampa}.
To run live programs created in it,
a simple runtime environment \fxerror{is this the right term?}
in the \mintinline{haskell}{IO} monad is supplied,
but since the framework does not hide \mintinline{haskell}{IO} in its abstraction
(such as many other FRP frameworks do),
it is an easy exercise to execute the live programs in e.g. the \mintinline{haskell}{STM} monad \cite{composable-memory-transactions}
\fxerror{Then do it}
or any other concurrency context such as an external main loop.
The state of the live program can be inspected and debugged safely at every step of the execution.
\fxerror{Consider talking about testing? We don't have anything implemented but it's easily possible}
\fxerror{Something something plants}

There is no subtly clever trick.
We simply follow, dutifully and without compromise,
the mantra of live coding:
\begin{center}
\textbf{Change the program. Keep the state.}
\end{center}
This is not a new idea in itself.
Hot code swap in Erlang realises this motto,
and
similar views are expressed about live coding in Elm\footnote{%
Compare \href{https://elm-lang.org/blog/interactive-programming}{https://elm-lang.org/blog/interactive-programming}.}
(a domain specific web frontend language inspired by Haskell).
\fxerror{Reference to blog entry}
What is new about this approach is the consequential application of this motto to create a general purpose, type-safe FRP framework\footnote{%
It shall be remarked that FRP is long past niche applications in the video and audio domains.
It is possible to write web servers and frontends, simulations and games in it,
FRP can even be used for file batch processing.}
with \emph{automatic state migration}.

Arriving at a simple live coding framework by faithfully following the live coding mantra is a manageable task,
carried out in Section \ref{sec:core}.
It is much more rewarding to recast this framework in the form of functional reactive programming,
which allows us to reuse modular, functional components and separate data flow from control flow.
The result is presented in Section \ref{sec:FRP},
which heavily draws inspiration from Dunai,
a monadic arrowized FRP framework.
After having implemented the data flow aspects of our framework,
we turn to control flow in Section \ref{sec:control flow}.
A monadic interface to our live programs is presented.

This article is written in literate Haskell and supplies the library presented here.
The source code will be made openly available upon publication.\fxerror{Do it}

\section{Change the program. Keep the state (as far as possible).}
\label{sec:core}

Our basic model of a live program will consist of a state and a state transition function:
\begin{code}
data LiveProgram1 = forall s . LiveProgram1
  { liveState :: s
  , liveStep  :: s -> IO s
  }
\end{code}
The program is initialised at a certain state,
and from there its behaviour is defined by repeatedly applying the function \mintinline{haskell}{liveStep} to advance the state and produce effects.
The type of the state should be encapsulated and thus invisible to the outside,
it is through its effects that the live program communicates.
This is especially convenient if we want to run the program in a separate thread
(while compiling a new version of the program in the foreground).
We can store the program in an \mintinline{haskell}{MVar} and repeatedly call the following function from a background thread:
\begin{code}
stepProgram :: LiveProgram1 -> IO LiveProgram1
stepProgram LiveProgram1 { .. } = do
  liveState' <- liveStep liveState
  return LiveProgram1 { liveState = liveState', .. }
\end{code}
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
It is very hard to reduce the probability of such a failure with tests since current state constantly changes
(by design).
A static typechecker is sorely missing,
to guarantee the safety of this operation.

But let us return to Haskell,
where we have such a typechecker.
Now we encounter a problem at an earlier stage:
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
To inspect it, we will -- for the moment -- expose it as a parameter:
\begin{code}
data LiveProgram2 s = LiveProgram2
  { liveState :: s
  , liveStep  :: s -> IO s
  }
\end{code}
Given a \emph{type migration} function,
we can perform hot code swap:
\begin{code}
hotCodeSwap
  :: (s -> s')
  -> LiveProgram2 s'
  -> LiveProgram2 s
  -> LiveProgram2 s'
hotCodeSwap migrate newProgram oldProgram
  = LiveProgram2
    { liveState = migrate $ liveState oldProgram
    , liveStep  = liveStep newProgram
    }
\end{code}
\fxwarning{The thing with the MVar doesn't work on the spot anymore. But it can still work with a "typed" handle. Every time you swap, you get a new handle that carries the currently saved type. Worth commenting upon?}
This may be an acceptable solution to perform a planned, well-prepared intervention,
but it does spoil the fun in a musical live coding performance if the programmer has to write a migration function after every single edit.
What a live performer actually needs,
is a function with this mysterious type signature:
\begin{spec}
hotCodeSwap
  :: LiveProgram2 s'
  -> LiveProgram2 s
  -> LiveProgram2 s'
\end{spec}
(This is the same type signature as before, but with the first argument, the manual migration function, removed.)
The new program (including its initial state!)
has just been compiled,
and the old program is still stored in a concurrent variable.
Can we possibly derive the new state by simply looking at the initial state of the new program and the old state,
i.e. is there a magical migration function?
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
But in the following, we are going to see how to tweak the live program definition by only ten characters,
and arrive at an effective migration function.

\subsection{Type-driven migrations}

In many cases, knowing the old state and the new initial state is sufficient to derive the new, migrated state safely.
As an example, assume the old state were defined as:
\fxwarning{Later quote from examples}
\fxerror{Have an example that threads through the article. Introduce earlier}
\begin{spec}
data Foo = Foo { n :: Integer }
\end{spec}
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
and records with the same field labels,
but possibly a different order.
Needless to say, if the types do match, then the old state is identically copied.
\fxerror{Not sure whether I know how to insert user migrations in this}

To use this migration function,
we only need to update the live program definition to include the \mintinline{haskell}{Data} constraint:
\fxerror{Maybe it was silly to enumerate the definitions before}
\begin{code}
data LiveProgram = forall s . Data s
  => LiveProgram
  { state :: s
  , step  :: s -> IO s
  }
\end{code}
This is a small restriction.
The \mintinline{haskell}{Data} typeclass can be automatically derived for every algebraic data type,
except those that incorporate \emph{functions}.
We have to refactor our live program such that all functions are contained in \mintinline{haskell}{step}
(and can consequently not be migrated),
and all data is contained in \mintinline{haskell}{state}.

\section{Livecoding as arrowized FRP}
\label{sec:FRP}

\subsection{Arrowized FRP with effects}

Writing out the complete state of the live program explicitly is, of course, tedious.
Worse even, it prevents us from writings programs in a modular fashion.
But composing simple reusable building blocks is a key tenet in functional programming which we would not want to miss.

\input{../src/LiveCoding/Cell.lhs}

\subsection{Exceptions}
Section \ref{sec:msfs and final coalgebras} showed that \mintinline{haskell}{Cell}s and Dunai's monadic stream functions are very much alike,
and it makes only sense to adopt its approach to control flow.
In Dunai, we can switch from executing one stream function to another by \emph{throwing an exception}.
Whenever we wish to hand over control to another component,
we throw an exception as an effect in the \mintinline{haskell}{ExceptT} monad
(which is simply the \mintinline{haskell}{Either} monad beefed up as a monad transformer).
This exception has to be handled by choosing a new component based on the exception.
The type checker can verify at the end that all exceptions have been handled.

Dunai offers a \mintinline{haskell}{Monad} interface where the values in the context are the \emph{thrown exceptions}
(and not the output data).
This offers a comfortable and idiomatic way of separating data flow and control flow,
resulting in well-structured code.
We would like to adopt this approach here,
but we are forewarned:
\mintinline{haskell}{Cell}s are slightly less expressive than Dunai's stream functions,
due to the \mintinline{haskell}{Data} constraint on the internal state.
It will turn out that we can implement \mintinline{haskell}{Functor} and \mintinline{haskell}{Applicative} instances effortlessly,
but the \mintinline{haskell}{Monad} instance will be quite a high bar to clear.

\input{../src/LiveCoding/Exceptions.lhs}
\input{../src/LiveCoding/CellExcept.lhs}

\fxerror{Possibly cut the applicative detour? Need to reorder forever then}
\input{../src/LiveCoding/Bind.lhs}
\input{../src/LiveCoding/Forever.lhs}
\fxerror{reactimate}

\section{Conclusion}

General purpose live coding can be simple and free from boilerplate.
It is most naturally cast in the form of a Functional Reactive Programming (FRP) framework,
and conforms well with the synchronous, arrowized paradigm in the tradition of Yampa and Dunai.
The state migration is type-safe,
and type-driven, in that it is derived generically from the datatype definition.
By parametrizing the cells over arbitrary monads,
and leveraging the exception monad,
we can reason about effects and separate data flow aspects from control flow.

\paragraph{Further directions}
Given that the state of the live programs always satisfies the \mintinline{haskell}{Data} typeclass,
and the control state is even finite,
this opens up the possibility to implement a rich debugger that inspects and displays the state live,
and even allows to modify it.

To use the framework in any setting beyond a toy application,
wrappers have to be written that explicitly integrate it in the external loops of existing frameworks,
such as OpenGL, Gloss, or audio libraries.

The \mintinline{haskell}{Typeable} class allows to extend a generic function such as \mintinline{haskell}{migrate}
by type-specific cases.
Usability could be increased by offering to extend the automatic migration by manual migration functions supplied by the user.

The automatic migration only guarantees that the new state will typecheck.
However, if further invariants beyond the reach of Haskell's type system are expected to hold for the old state,
those are not guaranteed for the new state.
An extension such as refinement types
(see e.g. \cite{LiquidHaskell} about LiquidHaskell)
would allow to specify, for example,
certain algebraic constraints.
It would be very interesting to see whether automatic migration can be generalised to such a context.

The author thanks Iván Pérez for his work on Yampa, Dunai, and numerous other projects in the FRP world,
Paolo Capriotti for the initial idea that led to monadic exception control flow,
and the sonnen VPP team, especially Fabian Linges,
for helpful discussions about hot code swap in Erlang.

\bibliography{EssenceOfLiveCoding.bib}
\end{document}
