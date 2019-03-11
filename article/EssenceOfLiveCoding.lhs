\documentclass[sigplan,10pt,screen]{acmart}

\usepackage[utf8]{inputenc}
\usepackage{minted}

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

\section{Livecoding as arrowized FRP}

\subsection{Arrowized FRP with effects}

Writing out the complete state of the live program explicitly is, of course, tedious.
Worse even, it prevents us from writings programs in a modular fashion.
But composing simple reusable building blocks is a key tenet in functional programming which we would not want to miss.
\input{../src/LiveCoding/Cell.lhs}

\end{document}
