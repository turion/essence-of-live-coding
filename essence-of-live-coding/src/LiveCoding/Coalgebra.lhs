\begin{comment}
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.Coalgebra where

-- base
import Control.Arrow (second)
import Data.Data

-- essence-of-live-coding
import LiveCoding.Cell

\end{code}
\end{comment}

\section{Monadic stream functions and final coalgebras}

\label{sec:msfs and final coalgebras}

\mintinline{haskell}{Cell}s mimick Dunai's \cite{Dunai} monadic stream functions (\mintinline{haskell}{MSF}s) closely.
But can they fill their footsteps completely in terms of expressiveness?
If not, which programs exactly can be represented as \mintinline{haskell}{MSF}s and which can't?
To find the answer to these questions,
let us reexamine both types.

With the help of a simple type synonym,
the \mintinline{haskell}{MSF} definition can be recast in explicit fixpoint form:

\begin{code}
type StateTransition m a b s = a -> m (b, s)

data MSF m a b = MSF
  { unMSF :: StateTransition m a b (MSF m a b)
  }
\end{code}
This definition tells us that monadic stream functions are so-called \emph{final coalgebras} of the \mintinline{haskell}{StateTransition} functor
(for fixed \mintinline{haskell}{m}, \mintinline{haskell}{a}, and \mintinline{haskell}{b}).
An ordinary coalgebra for this functor is given by some type \mintinline{haskell}{s} and a coalgebra structure map:
\begin{code}
data Coalg m a b where
  Coalg
    :: s
    -> (s -> StateTransition m a b s)
    -> Coalg m a b
\end{code}
But hold on, the astute reader will intercept,
is this not simply the definition of \mintinline{haskell}{Cell}?
Alas, it is not, for it lacks the type class restriction \mintinline{haskell}{Data s},
which we need so dearly for the type migration.
Any cell is a coalgebra,
but only those coalgebras that satisfy this type class are a cell.

Oh, if only there were no such distinction.
By the very property of the final coalgebra,
we can embed every coalgebra therein:
\begin{code}
finality :: Monad m => Coalg m a b -> MSF m a b
finality (Coalg state step) = MSF $ \a -> do
  (b, state') <- step state a
  return (b, finality $ Coalg state' step)
\end{code}
And analogously, every cell can be easily made into an \mintinline{haskell}{MSF} without loss of information:
\begin{code}
finalityC :: Monad m => Cell m a b -> MSF m a b
finalityC Cell { .. } = MSF $ \a -> do
  Result cellState' b <- cellStep cellState a
  return (b, finalityC $ Cell cellState' cellStep)
\end{code}
And the final coalgebra is of course a mere coalgebra itself:
\begin{code}
coalgebra :: MSF m a b -> Coalg m a b
coalgebra msf = Coalg msf unMSF
\end{code}
But we miss the abilty to encode \mintinline{haskell}{MSF}s as \mintinline{haskell}{Cell}s by just the \mintinline{haskell}{Data} type class:
\begin{code}
coalgebraC
  :: (Data (MSF m a b), Functor m)
  => MSF m a b
  -> Cell m a b
coalgebraC msf = Cell msf $ fmap (fmap (\(b, msf) -> Result msf b)) . unMSF
\end{code}
We are out of luck if we would want to derive an instance of \mintinline{haskell}{Data (MSF m a b)}.
Monadic stream functions are, well, functions,
and therefore have no \mintinline{haskell}{Data} instance.
The price of \mintinline{haskell}{Data} is loss of higher-order state.
Just how big this loss is will be demonstrated in the following section.

\begin{comment}
\subsection{Initial algebras}

\begin{code}
type AlgStructure m a b s = StateTransition m a b s -> s
data Alg m a b where
  Alg
    :: s
    -> AlgStructure m a b s
    -> Alg m a b

algMSF :: MSF m a b -> Alg m a b
algMSF msf = Alg msf MSF

-- TODO Could explain better why this is simpler in the coalgebra case
initiality
  :: Functor m
  => AlgStructure m a b s
  -> MSF m a b
  -> s
initiality algStructure = go
  where
    go msf = algStructure $ \a -> second go <$> unMSF msf a

\end{code}
\end{comment}
