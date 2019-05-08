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


\input{../src/LiveCoding/Coalgebra.lhs}
\fxerror{This is already in the article:}
\input{../src/LiveCoding/CellExcept/Newtype.lhs}
\fxerror{We've now switched to using runExceptC, so liveBind is implemented in terms of it.}
\input{../src/LiveCoding/LiveBind.lhs}
\input{../src/LiveCoding/CellExcept/Applicative.lhs}
\input{../src/LiveCoding/CellExcept/Monad.lhs}

\end{document}
