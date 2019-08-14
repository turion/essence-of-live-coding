\ProvidesClass{essence}

\LoadClass[sigplan,10pt,screen,nonacm]{acmart}

\RequirePackage[utf8]{inputenc}
\RequirePackage{minted}
\RequirePackage[nomargin,inline]{fixme}
\RequirePackage{hyperref}
\RequirePackage{verbatim}
\RequirePackage{microtype}

\bibliographystyle{ACM-Reference-Format}

\newenvironment{code}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}
\newenvironment{spec}{\VerbatimEnvironment\begin{minted}{haskell}}{\end{minted}}

