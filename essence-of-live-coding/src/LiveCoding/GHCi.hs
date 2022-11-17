{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Support functions to call common live coding functionalities like launching and reloading
from a @ghci@ or @cabal repl@ session.

You typically don't need to import this module in your code,
but you should load it in your interactive session,
ideally by copying the file `essence-of-live-coding/.ghci` to your project,
adjusting it to your needs and launching @cabal repl@.
-}
module LiveCoding.GHCi where

-- base
import Control.Concurrent
import Control.Exception (Exception (displayException, toException), SomeException, try)
import Control.Monad (join, void, (>=>))
import Data.Data
import Data.Function ((&))

-- transformers
import Control.Monad.Trans.State.Strict

-- foreign-store
import Foreign.Store

-- essence-of-live-coding
import LiveCoding.LiveProgram
import LiveCoding.RuntimeIO.Launch

proxyFromLiveProgram :: LiveProgram m -> Proxy m
proxyFromLiveProgram _ = Proxy

-- | An exception type marking the absence of a foreign store of the correct type.
data NoStore = NoStore
  deriving (Show)

instance Exception NoStore

-- * Retrieving launched programs from the foreign store

{- | Try to retrieve a 'LiveProgram' of a given type from the 'Store',
   handling all 'IO' exceptions.
   Returns 'Right Nothing' if the store didn't exist.
-}
possiblyLaunchedProgram ::
  Launchable m =>
  Proxy m ->
  IO (Either SomeException (LaunchedProgram m))
possiblyLaunchedProgram _ = do
  storeMaybe <- lookupStore 0
  fmap join $ try $ traverse readStore $ maybe (Left $ toException NoStore) Right storeMaybe

{- | Try to load a 'LiveProgram' of a given type from the 'Store'.
   If the store doesn't contain a program, it is (re)started.
-}
sync :: Launchable m => LiveProgram m -> IO ()
sync program = do
  launchedProgramPossibly <- possiblyLaunchedProgram $ proxyFromLiveProgram program
  case launchedProgramPossibly of
    -- Looking up the store failed in some way, restart
    Left (e :: SomeException) -> do
      putStrLn $ displayException e
      launchAndSave program

    -- A program is running, update it
    Right launchedProgram -> do
      putStrLn "update"
      update launchedProgram program

-- | Launch a 'LiveProgram' and save it in the 'Store'.
launchAndSave :: Launchable m => LiveProgram m -> IO ()
launchAndSave = launch >=> save

-- | Save a 'LiveProgram' to the store.
save :: Launchable m => LaunchedProgram m -> IO ()
save = writeStore $ Store 0

{- | Try to retrieve a 'LaunchedProgram' from the 'Store',
   and if successful, stop it.
-}
stopStored ::
  Launchable m =>
  Proxy m ->
  IO ()
stopStored proxy = do
  launchedProgramPossibly <- possiblyLaunchedProgram proxy
  either (putStrLn . displayException) stop launchedProgramPossibly

-- * GHCi commands

-- ** Debugging

-- TODO Could also parametrise this and all other commands by the 'liveProgram'

{- | Initialise a launched program in the store,
   but don't start it.
-}
liveinit _ =
  return $
    unlines
      [ "programVar <- newMVar liveProgram"
      , "threadId <- myThreadId"
      , "save LaunchedProgram { .. }"
      ]

-- | Run one program step, assuming you have a launched program in a variable @launchedProgram@.
livestep _ = return "stepLaunchedProgram launchedProgram"

-- ** Running

-- | Launch or restart a program and save its reference in the store.
livelaunch _ = return "sync liveProgram"

-- | Reload the code and do hot code swap and migration.
livereload _ =
  return $
    unlines
      [ ":reload"
      , "sync liveProgram"
      ]

-- | Stop the program.
livestop _ = return "stopStored $ proxyFromLiveProgram liveProgram"
