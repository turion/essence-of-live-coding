\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module LiveCoding.QuickCheck where

-- base
import Control.Arrow
import Control.Monad (foldM)

-- QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- boltzmann-samples
import Boltzmann.Data

-- essenceoflivecoding
import LiveCoding.Cell
\end{code}
\end{comment}

\subsection{Testing with \texttt{QuickCheck}}

Often, some cells in a live program
should satisfy certain correctness properties.
It is good practice in Haskell to build up a program from functions,
and ensure their correctness with property-based testing.
\texttt{QuickCheck} \cite{quickcheck}
is the primeval framework for this.
It generates
%, type-driven,
arbitrary input for a function,
and checks whether given assertions are valid.

In our livecoding approach, programs are not composed of mere functions, but of cells,
and of course we wish to test them in a similar way before reloading.
\fxwarning{Say that it's really good to know that your cells do what you expect before you just reload into them. We could need some tooling to call quickcheck before reloading.}
As a simple example,
we wish to assure that \mintinline{haskell}{sumC} will never output negative numbers if only positive numbers are fed into it.
Our test cell is thus defined as:
\begin{code}
testCell :: Cell IO (Positive Int) Bool
testCell
  = arr getPositive >>> sumC  >>> arr (>= 0)
\end{code}
(The \mintinline{haskell}{IO} monad only occurs here for monomorphization.
But let it be remarked that we will be able to test cells with actual side effects in the same way as pure ones.)

Given a faulty cell, it is impossible to predict how often it must be stepped until it returns an invalid value.
The number of successive inputs has to be variable in a test.
We therefore begin by running a cell repeatedly against a list of inputs, collecting its outputs:
\fxerror{Shortening candidate}
\begin{code}
embed
  :: Monad m
  =>        [a]
  -> Cell  m a b
  ->       m  [b]
embed [] _ = return []
embed (a : as) cell = do
  (b, cell') <- step cell a
  bs <- embed as cell'
  return $ b : bs
\end{code}
If the input type \mintinline{haskell}{a} can be generated arbitrarily,
then so can a list of \mintinline{haskell}{a}s.
Once we have run the cell with the given inputs,
we form the conjunction of the properties tested at each step,
with \texttt{QuickCheck}'s \mintinline{haskell}{conjoin}.
Effects in \mintinline{haskell}{IO} can be embedded in \texttt{QuickCheck} \cite{QuickCheckIO}
with the monad morphism \mintinline{haskell}{run},
and executed with \mintinline{haskell}{monadicIO}.
Cobbling all those pieces together makes cells testable:
\begin{code}
instance (Arbitrary a, Show a, Testable prop)
  => Testable (Cell IO a prop) where
  property cell = property
    $ \as -> monadicIO $ fmap conjoin
    $ embed as $ hoistCell run cell
\end{code}
Let us execute our test:
\begin{verbatim}
 > quickCheck testCell
+++ OK, passed 100 tests.
\end{verbatim}
A large class of properties can be tested this way.
If we want to ensure that the output of some complex \mintinline{haskell}{cell1} satisfies a property depending on the current input and internal state,
we can remodel the relevant portions of its state in a simplified \mintinline{haskell}{cell2} and check the property:
\begin{code}
agreesWith
  :: (Arbitrary a, Show a, Testable prop)
  => Cell IO  a  b
  -> Cell IO (a, b) prop
  -> Property
cell1 `agreesWith` cell2 = property $ proc a -> do
  b <- cell1 -<  a
  cell2      -< (a, b)
\end{code}
Along these lines, one can set up stateful property-based testing \cite{ProperTesting} for the livecoding environment.
\begin{comment}
Similarly, we can check the output of one cell against a reference implementation:
\begin{code}
bisimulates
  :: (Arbitrary a, Show a, Eq b, Show b)
  => Cell IO a b
  -> Cell IO a b
  -> Property
cell1 `bisimulates` cell2 = property $ proc a -> do
  b1 <- cell1 -< a
  b2 <- cell2 -< a
  returnA -< b1 === b2
\end{code}
\end{comment}

One shortcoming of the testing methods presented so far is that the cells will always be initialised at the same state.
This can restrict the search space for the cell state greatly,
as it will only reach those states reachable from the initial state after a number of steps,
depending on the generator size.
Luckily, since the state of our cells is an instance of \mintinline{haskell}{Data},
we can use generic programming to automatically generate values for it.
For example, the package \texttt{boltzmann-samplers}
\cite{boltzmann-samplers}
provides a function \mintinline{haskell}{generator' :: Data a => Size' -> Gen a}.
We can use it to reinitialise an arbitrary cell:
\begin{code}
reinitialise :: Cell m a b -> Gen (Cell m a b)
reinitialise Cell { .. } = do
  cellState <- generator' 1000
  return Cell { .. }
\end{code}
This can be used to test cells starting at arbitrary states.
\fxerror{But how to test the cell after migration? This is really hard! Black box vs. white box testing}
\begin{comment}
Still, what we are actually interested in is whether the state after a migration would be valid!
We can apply our insights from the last section:
This is a job for a debugger.
Given our current test cell implementation,
\begin{code}
quickCheckDebugger
  :: (Arbitrary a, Show a, Testable prop)
  => Cell IO a prop
  -> Debugger
quickCheckDebugger testCell
  = Debugger $ \s -> do
    let Cell { .. } = 
    testCell <- 
\end{code}
\end{comment}
\fxwarning{Could use quickcheck `counterexamples` on `gshow cellState` somehow}
