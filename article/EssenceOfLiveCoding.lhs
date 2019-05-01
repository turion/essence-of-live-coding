\documentclass{essence}

\begin{document}
\title{The essence of live coding: Change the program, keep the state!}
\subtitle{Functional pearl}

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
  by faithfully following the motto of livecoding:
  ``Change the program, keep the state.''

  With generic programming,
  one easily arrives at an automatic state migration function.
  The approach can be generalised to an arrowized Functional Reactive Programming framework that is parametrized by its side effects.
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
\fxwarning{``Server''? Program? Or introduce ``Server''? E.g. ''Erlang deals with servers. There hot code swap is simple.''}
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
The source code will be made openly available upon publication.\fxerror{Do it, or at least post a preview link for the reviewers here.}

\input{../src/LiveCoding/LiveProgram.lhs}
\input{../app/DemoWai/Env.lhs}
\input{../app/DemoWai/DemoWai1.lhs}
\input{../app/DemoWai/DemoWai2.lhs}

\section{Livecoding as arrowized FRP}
\label{sec:FRP}

\fxwarning{Small overview paragraph here? And in the other corresponding places?}

Writing out the complete state of the live program explicitly is, of course, tedious.
We have to plan the whole program in advance and artificially separate its state from the step function.
Such a development approach prevents us from writing programs in a modular fashion.
But composing simple reusable building blocks is a key tenet in functional programming which we would not want to miss.
The purpose of this section is to show that we can develop live programs in a modular fashion by extending the approach presented so far to an arrowized FRP framework.

\subsection{Arrowized FRP with effects}

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
It will turn out that we can implement a \mintinline{haskell}{Functor} instance effortlessly,
and an \mintinline{haskell}{Applicative} instance with a little work,
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

\fxerror{Speedtest: Nearly double as fast as dunai. Could use Data to optimize the state even more? Or rather we'd use GADTs for that, I guess. We still have to speed up 3-4 times to reach Yampa.}

\paragraph{Further directions}
Given that the state of the live programs always satisfies the \mintinline{haskell}{Data} typeclass,
and the control state is even finite,
this opens up the possibility to implement a rich debugger that inspects and displays the state live,
and even allows to modify it.

To use the framework in any setting beyond a toy application,
wrappers have to be written that explicitly integrate it in the external loops of existing frameworks,
such as OpenGL, Gloss, or audio libraries.
\fxerror{And web front- and backends! Reflex is FRPish, so why not this here as well?}
\fxerror{Implement some example apps in these to show that it works in principle}

The \mintinline{haskell}{Typeable} class allows to extend a generic function such as \mintinline{haskell}{migrate}
by type-specific cases.
Usability could be increased by offering to extend the automatic migration by manual migration functions supplied by the user.
\fxerror{Implement that!}

\fxerror{FRP was originally about the passage of time. Can recover that and much more by reimplementing Rhine in this framework (should now be possible)}

The automatic migration only guarantees that the new state will typecheck.
However, if further invariants beyond the reach of Haskell's type system are expected to hold for the old state,
those are not guaranteed for the new state.
An extension such as refinement types
(see e.g. \cite{LiquidHaskell} about LiquidHaskell)
would allow to specify, for example,
certain algebraic constraints.
It would be very interesting to see whether automatic migration can be generalised to such a context.

\medskip

The author thanks Iván Pérez for his work on Yampa, Dunai, and numerous other projects in the FRP world,
Paolo Capriotti for the initial idea that led to monadic exception control flow,
and the sonnen VPP team, especially Fabian Linges,
for helpful discussions about hot code swap in Erlang.

\bibliography{EssenceOfLiveCoding.bib}
\end{document}
