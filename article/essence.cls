\Providesclass{essence}

\LoadClass[sigplan,10pt,screen]{acmart}

\RequirePackage[utf8]{inputenc}
\RequirePackage{minted}
\RequirePackage[final, nomargin,inline]{fixme}
\RequirePackage{hyperref}
\RequirePackage{verbatim}

\bibliographystyle{ACM-Reference-Format}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

