\begin{comment}
\begin{code}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module LiveCoding.QuickCheck where

-- base
import Control.Arrow
import Control.Monad (foldM, when)
import Data.Data

-- transformers
import Control.Monad.Trans.Writer

-- syb
import Data.Generics.Aliases

-- QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- boltzmann-samples
import Boltzmann.Data

-- essence-of-live-coding
import LiveCoding
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

\paragraph{Unit tests}
In our live coding approach, programs are not composed of mere functions, but of cells,
and of course we wish to test them in a similar way before reloading.
\fxwarning{Say that it's really good to know that your cells do what you expect before you just reload into them. We could need some tooling to call quickcheck before reloading.}
As a simple example,
we wish to assure that \mintinline{haskell}{sumC} will never output negative numbers if only positive numbers are fed into it.
Our test cell is thus defined as:
\fxwarning{Shortening SF}
\begin{code}
testCell :: Monad m => Cell m (Positive Int) Bool
testCell
  = arr getPositive >>> sumC >>> arr (>= 0)
\end{code}
\begin{comment}
(The \mintinline{haskell}{IO} monad only occurs here for monomorphization.
But let it be remarked that we will be able to test cells with actual side effects in the same way as pure ones.)
\end{comment}
\fxwarning{Test in IO}
%Given a faulty cell, it is impossible to predict how often it must be stepped until it returns an invalid value.
%The number of successive inputs has to be variable in a test.
%We therefore 
We
begin by running a cell repeatedly against a list of inputs, collecting its outputs:
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
After running the cell with all inputs,
we form the conjunction of all properties,
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
\begin{comment}
\begin{code}
cellCheck
  :: (Arbitrary a, Show a, Testable prop)
  => Cell IO a prop
  -> IO ()
cellCheck = quickCheck
\end{code}
\end{comment}
\fxerror{Actually need to go through cellCheck (commented). Include if enough space.}
Let us execute our test:
\begin{verbatim}
 > quickCheck testCell
+++ OK, passed 100 tests.
\end{verbatim}
A large class of properties can be tested this way.
We can unit test all components of a new version of our live program before reloading it.
To go further, one could set up \emph{stateful property-based testing} \cite{ProperTesting} for the live coding environment.

\paragraph{Migration tests}
Even better, we can test \emph{before reloading}
whether the newly migrated state would be valid.
Given some tests on intermediate values in the computation,
we collect all test properties in a \mintinline{haskell}{Writer} effect:
\begin{code}
logTest
  :: Monad m
  => Cell m a prop
  -> Cell (WriterT [prop] m) a ()
logTest cell
  =   liftCell cell
  >>> arrM (return >>> tell)
\end{code}
Now the tests can be included in the definition of the whole live program without adding new outputs.
\fxerror{Need some migration into and out of Writer if this is supposed to work}
When the program is built,
we can optionally test the properties:
\begin{code}
liveCheck
  :: Testable prop
  => Bool
  -> LiveProgram (WriterT [prop] IO)
  -> LiveProgram                 IO
liveCheck test = hoistLiveProgram performTests
  where
    performTests action = do
      (s, props) <- runWriterT action
      when test $ quickCheck $ conjoin props
      return s
\end{code}
The function \mintinline{haskell}{liveCheck True} will run \mintinline{haskell}{quickCheck} on all properties,
while \mintinline{haskell}{liveCheck False} gives the ``production'' version of our program,
with tests disabled.
We launch two separate threads and run the test version in one of them and the production version in the other.
Always reloading into the test version first,
we can ensure that the migration will create valid state before migrating the live system.

\begin{comment}
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
Along these lines, one can set up stateful property-based testing \cite{ProperTesting} for the live coding environment.
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
\fxwarning{I cut reinitialise here}
\begin{comment}
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
\end{comment}
\fxerror{But how to test the cell after migration? This is really hard! Black box vs. white box testing}
\begin{comment}
Still, what we are actually interested in is whether the state after a migration would be valid!
We can apply our insights from the last section:
This is a job for a debugger.
Given our current test cell implementation,
\begin{spec}
quickCheckDebugger
  :: (Arbitrary a, Show a, Testable prop)
  => Cell IO a prop
  -> Debugger
quickCheckDebugger testCell
  = Debugger_ $ \s -> do
    let Cell { .. } = 
    testCell <- 
\end{spec}
\end{comment}
\fxwarning{Could use quickcheck `counterexamples` on `gshow cellState` somehow}
\fxerror{Should test properties of the state by putting state in a newtype and specify a property that is added to a generic query}
\fxerror{There is new code here that I'd like to talk about}
\begin{comment}
\begin{code}
testState
  :: GenericQ Property
  -> LiveProgram m
  -> Property
testState query LiveProgram { .. } = conjoin
  $ gmapQ query liveState

mkGenericProperty
  :: Typeable b
  =>         (b -> Property)
  -> GenericQ      Property
mkGenericProperty = mkQ $ property True

posSumC :: (Monad m, Num a, Data a) => Cell m a a
posSumC = Cell { .. }
  where
    cellState = Positive 0
    cellStep accum a = return
      ( getPositive accum
      , Positive $ getPositive accum + a
      )

deriving instance Data a => Data (Positive a)
\end{code}
\end{comment}
\fxerror{This is missing a test case. E.g. sum and internal accum must be positive.}
