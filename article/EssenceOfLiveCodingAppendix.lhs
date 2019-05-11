\documentclass{essence}

\begin{document}
\title{The essence of live coding: Change the program, keep the state!}
\subtitle{Appendix}

\begin{abstract}
This file supplies the somewhat technical derivations of the \mintinline{haskell}{Applicative} and \mintinline{haskell}{Monad} type classes.
It is not necessary to read it in order to appreciate the main paper,
but some readers may want to satisfy their curiosity.
\end{abstract}

\maketitle


\fxwarning{Say what feedback is and that it's not the same as ArrowLoop?}
\input{../src/LiveCoding/Coalgebra.lhs}
\fxerror{We've now switched to using runExceptC, so liveBind is implemented in terms of it.}
We would like to adopt this approach here,
but we are forewarned:
\mintinline{haskell}{Cell}s are slightly less expressive than Dunai's stream functions,
due to the \mintinline{haskell}{Data} constraint on the internal state.
\section{Monads for control flow}
\input{../src/LiveCoding/LiveBind.lhs}
\input{../src/LiveCoding/CellExcept/Applicative.lhs}
\input{../src/LiveCoding/CellExcept/Monad.lhs}

\bibliography{EssenceOfLiveCoding.bib}
\end{document}
