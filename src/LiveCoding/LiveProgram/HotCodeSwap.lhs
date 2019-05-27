\begin{comment}
\begin{code}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module LiveCoding.LiveProgram.HotCodeSwap where

-- base
import Data.Data

-- essenceoflivecoding
import LiveCoding.Migrate
\end{code}
\end{comment}

\subsection{Type-driven migrations}
In many cases, knowing the old state and the new initial state is sufficient to derive the new, migrated state safely.
As an example, imagine the internal state of a simple webserver that counts the number of visitors to a page.
\fxwarning{Later show how migrate behaves on these examples}
\fxwarning{Lib: All examples should be in a separate directory, not in src. We should only have the final library in src.}
\fxwarning{Typecheck the example somehow? Put it in different files and make figures?}
\fxerror{Extend the example to start from Int and migrate into the newtype?}
\begin{spec}
data State = State { nVisitors :: Int }
\end{spec}
The server is initialised at 0,
and increments the number of visitors every step.
(For a full-fledged webserver,
the reader is asked to patiently wait until the next section.)
\begin{spec}
server = LiveProgram (State 0) $ \State { .. }
  -> State $ return $ nVisitors + 1
\end{spec}
\fxwarning{Show the different definitions of State as different modules by directly quoting different files as figures?
Would like to use same files like the wai demo (but hard because of Data).
Maybe rename them in order not to say Wai before we've introduced it.}
We extend the state by the name of the last user agent to access the server (initially not present):
\fxwarning{What if we add another constructor Bar here?
Could it still find out that there is a State constructor in the type?}
\begin{spec}
data State = State Int (Maybe ByteString)
initState = State 0 Nothing
\end{spec}
From just comparing the two datatype definitions,
it is apparent that we would want to keep the number of visitors,
of type \mintinline{haskell}{Int},
when migrating.
For the new argument of type \mintinline{haskell}{Maybe ByteString},
we cannot infer any sensible value from the old state,
but we can take the value \mintinline{haskell}{Nothing} from the new initial state,
and interpret it as a default value.
A general state migration function should specialise to:
\begin{spec}
migrate (Server1.State nVisitors)
        (Server2.State _         mUserAgent)
      =  Server2.State nVisitors mUserAgent
\end{spec}
Our task was less obvious if we would have extended the state by the last access time,
encoded as a UNIX timestamp:
\begin{spec}
data State = State Int Int
\end{spec}
Here it is unclear to which of the \mintinline{haskell}{Int}s the old value should be migrated.
It is obvious again if the datatype was defined as a record as well:
\begin{spec}
data State = State
  { nVisitors      :: Int
  , lastAccessUNIX :: Int
  }
\end{spec}
We need to copy the \mintinline{haskell}{nVisitors} field from the old state,
and initialise the \mintinline{haskell}{lastAccessUNIX} field from the new state.
(Conversely, if we were to migrate back to the original definition,
there is no way but to lose the data stored in \mintinline{haskell}{lastAccessUNIX}.)
Clearly, the record labels enabled us to identify the correct target field.
%The solution lies in the type,
%or rather, the datatype definition.
The solution lies in the datatype definition.

We can meta-program a migration function by reasoning about the structure of the type definition.
This is possible with the techniques presented in the seminal, now classic article ``Scrap Your Boilerplate'' \cite{syb}.
It supplies a typeclass \mintinline{haskell}{Typeable} which enables us to compare types and safely type-cast at runtime,
and a typeclass \mintinline{haskell}{Data} which allows us
%amongst many other features,
to inspect constructor names and record field labels.
Using the package \texttt{syb},
which supplies common utilities when working with \mintinline{haskell}{Data},
our migration function is implemented in under 50 lines of code,
with the following signature:
\begin{spec}
migrate :: (Data a, Data b) => a -> b -> a
\end{spec}
It handles the two previously mentioned cases:
Constructors with the same names,
but some mismatching arguments,
and records with some coinciding field labels,
but possibly a different order.
In nested datatype definitions,
the function recurses into all children of the data tree.
Needless to say, if the types do match, then the old state is identically copied.
\fxwarning{Show examples here?}

Sometimes it is necessary to manually migrate some part of the state.
Assume, for the sake of the example,
that our webserver has become wildly popular,
and \mintinline{haskell}{nVisitors} is close to \mintinline{haskell}{maxInt}.
We need to migrate this value to an arbitrary precision \mintinline{haskell}{Integer}.
It is easy to extend \mintinline{haskell}{migrate} by a special case provided by the user:
\begin{spec}
userMigrate
  :: (Data a, Data b, Typeable c, Typeable d)
  => (c -> d)
  -> a -> b -> a

intToInteger :: Int -> Integer
intToInteger = toInteger
\end{spec}
In our example, we would use \mintinline{haskell}{userMigrate intToInteger} to migrate the state.
\fxwarning{Show example. Extend runtime.}

To use the automatic migration function,
we only need to update the live program definition to include the \mintinline{haskell}{Data} constraint,
as shown in Figure \ref{fig:LiveProgram}.
\fxwarning{Idea for elsewhere: LiveProgram m = forall s (m s, s -> m s), to ease initialisation}
\begin{figure}
\begin{code}
data LiveProgram m = forall s . Data s
  => LiveProgram
  { liveState :: s
  , liveStep  :: s -> m s
  }
\end{code}
\begin{code}
hotCodeSwap
  :: LiveProgram m
  -> LiveProgram m
  -> LiveProgram m
hotCodeSwap
  (LiveProgram newState newStep)
  (LiveProgram oldState _)
  = LiveProgram
  { liveState = migrate newState oldState
  , liveStep  = newStep
  }
\end{code}
\caption{\texttt{LiveProgram.lhs}}
\label{fig:LiveProgram}
\end{figure}
This is a small restriction.
The \mintinline{haskell}{Data} typeclass can be automatically derived for every algebraic data type,
except those that incorporate \emph{functions}.
We have to refactor our live program such that all functions are contained in \mintinline{haskell}{liveStep}
(and can consequently not be migrated),
and all data is contained in \mintinline{haskell}{liveState}.

Now that we have a universal migration function,
it is not necessary to carry the type of the state around in the type signature.
In fact it would be cumbersome in combination with \mintinline{haskell}{MVar}s
(which can't change their type),
and a real burden when later modularising the state.
Consequently, the type is made existential.
The only necessary information is that it is an instance of \mintinline{haskell}{Data}.
