\documentclass[sigplan,10pt,screen]{acmart}

\usepackage[utf8]{inputenc}
\usepackage{minted}
\usepackage[draft]{fixme}

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
...
\end{abstract}

\maketitle

\section{Introduction}

Livecoding has come to denote various concepts,
from hot code swap on a production server to artistic performances with code that is written, executed and updated live.
From the implementor's perspective,
the common denominator is a framework to update the definition of a program while not interrupting its execution.

In a dynamically typed language with a virtual machine, such as Erlang,
\fxfatal{reference to hot code swap}
hot code swap is conceptually simple:
A new server is started, the state of the old server is transferred to the new one, and the old server is shut down.
The challenge lies in ensuring that the state is correctly migrated,
as corrupt state might crash the server.
This is a nontrivial task if no type checker is available to verify if the migrated state conforms to the schema required by the new server.

Domain specific live coding frameworks
\fxfatal{reference to supercollider}
usually have a central server that generates the desired effect
-- be it audio, video or device communication --
and offers an API through which users can create ``cells'',
such as oscillators or signal processors in the case of audio,
and connect, reorder, update or destroy them during execution.
The central server guarantees safe updates,
and strongly typed user-side libraries such as tidal often exist,
\fxfatal{tidal reference}
but ultimately the user is restricted to a domain specific language.

In this article, we implement a lightweight general purpose livecoding framework in Haskell from scratch.
It is not only type-safe, but also type-driven, in that boiler plate code for state migrations which is hard to get right without a static type checker is automatically derived from the type.
It is not restricted to a particular domain, by virtue of being parametrised over an arbitrary monad:
Any domain- or library-specific effect can be incorporated effortlessly,
and handled with standard Haskell functions.
The framework follows the tradition of monadic arrowized Functional Reactive Programming (FRP) as developed in \fxfatal{yampa and dunai}.
To run live programs created in it,
a simple runtime environment \fxerror{is this the right term?}
in the \mintinline{haskell}{IO} monad is supplied,
but since the framework does not hide \mintinline{haskell}{IO} in its abstraction
(such as many other FRP frameworks do),
it is an easy exercise to execute the live programs in e.g. the \mintinline{haskell}{STM} monad
\fxfatal{Reference}
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
similar views are expressed about live coding in Elm
(a domain specific web frontend language inspired by Haskell).
\fxerror{Reference to blog entry}
What is new about this approach is the consequential application of this motto to create a general purpose, type-safe FRP framework.
(At this point, it shall be remarked that FRP is long past niche applications in the video and audio domains.
It is possible to write web servers, simulations and games in it,
FRP can even be used for file batch processing.)

Arriving at a simple live coding framework by faithfully following the live coding mantra is a manageable task,
carried out in Section \ref{sec:core}.
It is much more rewarding to recast this framework in the form of functional reactive programming,
which allows us to reuse modular, functional components and separate data flow from control flow.
The result is presented in Section \ref{sec:FRP},
which heavily draws inspiration from Dunai,
a monadic arrowized FRP framework.

This article is written in literate Haskell and supplies the library presented here.
The source code will be made openly available upon publication.\fxerror{Do it}

\section{Change the program. Keep the state...}
\label{sec:core}

Our basic model of a live program will consist of a state and a state transition function:
\begin{code}
data LiveProgram = forall s . LiveProgram
  { state :: s
  , step  :: s -> IO s
  }
\end{code}
The program is initialised at a certain state,
and from there its behaviour is defined by repeatedly applying the function \mintinline{haskell}{step} to advance the state and produce effects.

In a dynamically typed language, this would in principle be enough to implement hot code swap.
At some point, the execution will be paused,
and the function \mintinline{haskell}{step} is simply exchanged for a new one.
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

\fxfatal{continue}
statically typed how does tat work even earlier problem
could annotate type and send migration function
but that's tedious and not very live
let's turn the problem into a solution and do type-guided migrations

\fxerror{Order ok like this? DB earlier?}
The basic idea of updating the old state to a new format
bla database
type migration

\fxerror{Have an example that threads through the article}
\subsection{...as far as possible}

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
\fxerror{Fix Forever}
%\input{../src/LiveCoding/Forever.lhs} % Currently not ready

\end{document}
