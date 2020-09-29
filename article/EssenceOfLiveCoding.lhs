\documentclass{essence}

\begin{document}
\title{The essence of live coding: Change the program, keep the state!}
%\subtitle{Functional pearl}

%\author{Manuel Bärenz}
%\orcid{0000-0003-1843-0773}             %% \orcid is optional
%\affiliation{
%  \institution{sonnen eServices GmbH}            %% \institution is required
%  \country{Deutschland}
%}
%\email{programming@manuelbaerenz.de}

\begin{abstract}
  One rarely encounters programming languages and frameworks that provide general-purpose and type-safe hot code swap.
  It is demonstrated here that this is entirely possible in Haskell,
  by faithfully following the motto of live coding:
  ``Change the program, keep the state.''

  With generic programming,
  one easily arrives at an automatic state migration function.
  The approach can be generalised to an arrowized Functional Reactive Programming framework that is parametrized by its side effects.
  It allows for building up complete live programs from reusable, modular components,
  and to separate data flow cleanly from control flow.
  Useful utilities for debugging and quickchecking are presented.
\end{abstract}

\maketitle

\section{Introduction}

\fxfatal{Lots of literature on "transducers" and "resumptions" that should be at least acknowledged:
Milner's 1975 paper "Processes: a mathematical model of computing agents" (https://www.sciencedirect.com/science/article/pii/S0049237X08719487), for which I can't find a free pdf. They typically went by the name "resumption", e.g. https://group-mmm.org/~ichiro/papers/component-trace.pdf Section 7, https://www.cs.ox.ac.uk/files/6660/crm.pdf, https://www.researchgate.net/publication/220173613_Geometry_of_Interaction_and_Linear_Combinatory_Algebras Section 5.4.
}

Live coding has come to denote various concepts,
from hot code swap on a production server to artistic performances with code that is written, executed and updated live.
From the implementor's perspective,
the common denominator is a framework to update the definition of a program while not interrupting its execution.

In a dynamically typed, process-oriented language, such as Erlang \cite{armstrong2013programming},
hot code swap is conceptually simple:
A new server process is started, the state of the old server is transferred to the new one, and the old server is shut down.
The challenge lies in ensuring that the state is correctly migrated,
as corrupt state might crash the server.
This is a nontrivial task if no type checker is available to verify if the migrated state conforms to the schema required by the new server.

Domain specific live coding frameworks
usually have a central server that generates the desired effect
-- be it audio, video or device communication --
and offers an API through which users can create ``cells'',
such as oscillators or signal processors in the case of audio
(such as \cite{wilson2011supercollider}),
and connect, reorder, update or destroy them during execution.
The central server guarantees safe updates,
and strongly typed user-side libraries
(such as Tidal \cite{mclean2014tidal} for audio applications)
exist,
but the user is restricted to a domain specific language.

In dynamic languages like SmallTalk \cite{ingalls2020evolutionofsmalltalk},
\fxfatal{Cite SmallTalk. Is it interpreted?}
new avenues to live coding open by interacting with the interpreter,
which holds the whole program in memory.
In a statically compiled language lacking a comparable depth of reflection,
this not directly possible, so one must abstract the program and its state into data structures,
and implement hot code swap.
One end of the design space are concurrent references to program components which can be exchanged,
\cite{HyperHaskell, apfelmus2019functors, murphy2016livecoding}
which are somewhat closer to an interpreted environment.
Let us restrict to the other end, though,
where the possible data and control states are known at compile time,
constraining the resulting framework slightly,
but allowing for a very smooth development experience.

In this article, we implement a lightweight general purpose live coding framework in Haskell from scratch.
It is not only type-safe, but also type-driven, in that boilerplate code for state migrations
(which is hard to get right without a static type checker)
is automatically derived from the type.
It is not restricted to a particular domain, by virtue of being parametrised over an arbitrary monad:
Any domain- or library-specific effect can be incorporated effortlessly,
and handled with standard Haskell functions.
The framework follows the tradition of monadic arrowized Functional Reactive Programming (FRP) as developed in \cite{Dunai} and \cite{Yampa}.
To run live programs created in it,
a runtime environment \fxwarning{is this the right term?}
in the \mintinline{haskell}{IO} monad is supplied,
but since the framework does not hide \mintinline{haskell}{IO} in its abstraction
(such as many other FRP frameworks do),
it is an easy exercise to execute the live programs in e.g. the \mintinline{haskell}{STM} monad \cite{composable-memory-transactions}
\fxwarning{Then do it}
or any other concurrency context such as an external main loop.
The state of the live program can be inspected and debugged safely during the execution.

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
What is new about this approach is the consequential application of this motto to create a general purpose, type-safe FRP framework\footnote{%
It shall be remarked that FRP is long past niche applications in the video and audio domains.
It is possible to write web servers and frontends, simulations and games in it.
%FRP can even be used for file batch processing.
}
with \emph{automatic state migration}.

Arriving at a simple \emph{state migration function} by faithfully following the live coding mantra is a manageable task,
carried out in Section \ref{sec:core}.
Upon the migration function, a live coding framework is constructed in Section \ref{sec:runtime}.
It is much more rewarding to recast this framework in the form of functional reactive programming,
which allows us to reuse modular, functional components and separate data flow from control flow.
Crucially, the state of our live programs is built up automatically by using FRP idioms.
The result is presented in Section \ref{sec:FRP},
which heavily draws inspiration from Dunai,
a monadic arrowized FRP framework;
and from Caspi's and Pouzet's work on synchronous stream functions \cite{CaspiPouzet}.
After having implemented the data flow aspects of our framework,
we turn to control flow in Section \ref{sec:control flow},
and encode it completely algebraically within the program state,
automatically getting a grip on exception handling and loops.
A monadic interface to our live programs is presented.
In Section \ref{sec:tooling}, several useful tools such as debuggers and quickchecking utilities are shown.

This article is written in literate Haskell and supplies the library presented here.
The source code is available at \href{https://github.com/turion/essence-of-live-coding}{https://github.com/turion/essence-of-live-coding},
while additional resources such as a presentation can be found at \href{https://www.manuelbaerenz.de/#computerscience}{https://www.manuelbaerenz.de/\#{}computerscience}.

\input{../essence-of-live-coding/src/LiveCoding/LiveProgram.lhs}
\fxerror{I believe this is even easier with Servant because it has simple functions!}
\input{../demos/app/DemoWai/Env.lhs}
\input{../demos/app/DemoWai/DemoWai1.lhs}
\input{../demos/app/DemoWai/DemoWai2.lhs}

\section{Live coding as arrowized Functional Reactive Programming}
\label{sec:FRP}

\fxwarning{Small overview paragraph here? And in the other corresponding places?}

Writing out the complete state of the live program explicitly is tedious.
We have to plan the whole program in advance and artificially separate its state from the step function.
Such a development approach prevents us from writing programs in a modular fashion.
%But composing simple reusable building blocks is a key tenet in functional programming which we would not want to miss.
The purpose of this section is to show that we can develop live programs modularly by extending the approach presented so far to an arrowized FRP framework.
Coincidentally, but naturally, we will end up with a coalgebraic presentation of synchronous stream functions which differs from Caspi's and Pouzet's work \cite{CaspiPouzet} only in notation and the presence of monads.

%\subsection{Arrowized FRP with effects}

\input{../essence-of-live-coding/src/LiveCoding/Cell.lhs}

\subsection{Exceptions}

\fxwarning{Shortening candidate, together with previous paragraph}
%Section \ref{sec:msfs and final coalgebras} showed that \mintinline{haskell}{Cell}s and Dunai's monadic stream functions are very much alike,
%and it makes sense to adopt its approach to control flow.
% In Dunai, we can switch from executing one stream function to another by \emph{throwing an exception}.
As in Dunai, whenever we wish to hand over control to another component,
we \emph{throw an exception} as an effect in the \mintinline{haskell}{ExceptT} monad
(which is simply the \mintinline{haskell}{Either} monad generalised to a monad transformer).
This exception has to be handled by choosing a new component based on the exception value.
The type checker can verify at the end that all exceptions have been handled.

Dunai offers a \mintinline{haskell}{Monad} interface where the values in the context are the \emph{thrown exceptions}
(and not the output data).
This offers a comfortable and idiomatic way of separating data flow and control flow,
resulting in well-structured code.
It will turn out that we can implement a \mintinline{haskell}{Functor} instance effortlessly,
and an \mintinline{haskell}{Applicative} instance with a little work,
but the \mintinline{haskell}{Monad} instance will be quite a high bar to clear.

\input{../essence-of-live-coding/src/LiveCoding/Exceptions.lhs}
%\input{../essence-of-live-coding/src/LiveCoding/CellExcept.lhs}

\input{../essence-of-live-coding/src/LiveCoding/Bind.lhs}
\input{../essence-of-live-coding/src/LiveCoding/Forever.lhs}

\section{Tooling}
\label{sec:tooling}
\input{../essence-of-live-coding/src/LiveCoding/Debugger.lhs}
\input{../essence-of-live-coding-quickcheck/src/LiveCoding/QuickCheck.lhs}

% \subsection{External main loops}

\fxfatal{Can integrate into external main loops using \mintinline{haskell}{step} for cells or an equivalent \mintinline{haskell}{stepMVar}.}
\fxfatal{Need to mention Handles and NonBlocking}

\section{Conclusion}

General purpose live coding can be simple and free from boilerplate.
It is most naturally cast in the form of a Functional Reactive Programming (FRP) framework,
and conforms well with the synchronous, arrowized paradigm in the tradition of Yampa and Dunai.
The state migration is type-safe,
and type-driven, in that it is derived generically from the datatype definition.
By parametrizing the cells over arbitrary monads,
and leveraging the exception monad,
we can reason about effects and separate data flow aspects from control flow.
The approach is extensible as debugging and testing methods can be added easily.

\fxerror{Speedtest: Nearly double as fast as dunai. Could use Data to optimize the state even more? Or rather we'd use GADTs for that, I guess. We still have to speed up 3-4 times to reach Yampa.}

\paragraph{Further directions}
To use the framework in any setting beyond a toy application,
wrappers have to be written that explicitly integrate it in the external loops of existing frameworks,
such as web frontends and backends, OpenGL, \texttt{gloss} \cite{Gloss}, or audio libraries.
As a start, the multi-clock FRP library \texttt{rhine} \cite{Rhine} could be adapted to this approach.
\fxerror{And web front- and backends! Reflex is FRPish, so why not this here as well?}
\fxerror{Implement some example apps in these to show that it works in principle}

\fxerror{FRP was originally about the passage of time. Can recover that and much more by reimplementing Rhine in this framework (should now be possible)}

\fxwarning{LTL}
\fxerror{Say somewhere why we used syb instead of Generics?}

The automatic migration only guarantees that the new state will typecheck.
However, if further properties beyond the reach of Haskell's type system are expected to hold for the old state,
those are not guaranteed for the new state.
Within Haskell, quickchecking is our only hope.
An extension such as refinement types
(see e.g. \cite{LiquidHaskell} about LiquidHaskell)
can automatically verify certain algebraic constraints, though.
It would be a great enrichment to generalise automatic migration to such a type system.

\medskip

The author thanks Iván Pérez for his work on Yampa, Dunai, and numerous other projects in the FRP world;
the reviewers of Haskell Symposium 2018 and REBLS 2020 for very helpful comments that enriched and streamlined this work;
Paolo Capriotti for the initial idea that led to monadic exception control flow;
and the sonnen VPP team, especially Fabian Linges,
for helpful discussions about hot code swap in Erlang.

\clearpage
\bibliography{EssenceOfLiveCoding.bib}
\end{document}
