\ProvidesClass{essence}

\LoadClass[sigplan,screen]{acmart}

\RequirePackage[utf8]{inputenc}
\RequirePackage{minted}
\setmintedinline{style=bw}
\RequirePackage[nomargin,inline]{fixme}
\RequirePackage{hyperref}
\RequirePackage{verbatim}
\RequirePackage{microtype}

\bibliographystyle{ACM-Reference-Format}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
