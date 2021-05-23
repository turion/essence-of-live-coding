{- | * Support for [PortMidi](http://hackage.haskell.org/package/PortMidi)

With this module, you can add cells which receive and send MIDI events.

You don't need to initialise PortMidi, or open devices,
this is all done by @essence-of-live-coding@ using the "LiveCoding.Handle" mechanism.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module LiveCoding.PortMidi where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad (void, forM, join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (fromRight)
import Data.Foldable (traverse_, find)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import GHC.Generics
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)

-- transformers
import Control.Monad.Trans.Class

-- PortMidi
import Sound.PortMidi

-- essence-of-live-coding
import LiveCoding

-- essence-of-live-coding-PortMidi
import LiveCoding.PortMidi.Internal

-- * The 'PortMidiT' monad transformer

{- | Monad transformer adding PortMidi-related effects to your monad.

This transformer adds two kinds of effects to your stack:

* PortMidi exceptions (See 'EOLCPortMidiError')
* Automatic initialisation of PortMidi devices (using 'HandlingStateT')
-}
newtype PortMidiT m a = PortMidiT
  { unPortMidiT :: ExceptT EOLCPortMidiError (HandlingStateT m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans PortMidiT where
  lift = PortMidiT . lift . lift

{- | Exceptions that can occur while doing livecoding with PortMidi.

There are two kinds of exceptions:

* Internal PortMidi exceptions (see 'PMError')
* When a device is not correctly specified by name and input/output configuration
-}
data EOLCPortMidiError
  -- | An internal error occurred in the PortMidi library
  = PMError PMError
  -- | There is no device of that name
  | NoSuchDevice
  -- | There is a device of that name, but it doesn't support input
  | NotAnInputDevice
  -- | There is a device of that name, but it doesn't support output
  | NotAnOutputDevice
  -- | There are multiple devices of the same name
  | MultipleDevices
  deriving (Data, Generic, Show)

instance Finite EOLCPortMidiError

deriving instance Data PMError
deriving instance Generic PMError
instance Finite PMError

-- ** Constructing values in 'PortMidiT'

-- | Given an exception value, throw it immediately.
throwPortMidi :: Monad m => EOLCPortMidiError -> PortMidiT m arbitrary
throwPortMidi = PortMidiT . throwE

-- | Like 'throwPortMidi', but as a 'Cell'.
throwPortMidiC :: Monad m => Cell (PortMidiT m) EOLCPortMidiError arbitrary
throwPortMidiC = arrM throwPortMidi

-- | Given a monadic action that produces a value or a 'PMError',
--   run it as an action in 'PortMidiT'.
--   Typically needed to lift PortMidi backend functions.
liftPMError :: Monad m => m (Either PMError a) -> PortMidiT m a
liftPMError = PortMidiT . ExceptT . fmap (left PMError) . lift

-- | Given a cell with existing handles, lift it into 'PortMidiT'.
liftHandlingState :: Monad m => Cell (HandlingStateT m) a b -> Cell (PortMidiT m) a b
liftHandlingState = hoistCell $ PortMidiT . lift

-- ** Running values in 'PortMidiT'

{- | Run a cell containing PortMidi effects.

@'runPortMidiC' cell@ goes through the following steps:

1. Initialize the MIDI system
2. Run @cell@, until possibly an exception occurs
3. Shut the MIDI system down
4. Throw the exception in 'CellExcept'
-}
runPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> CellExcept (HandlingStateT m) a b EOLCPortMidiError
runPortMidiC cell = try $ proc a -> do
  _ <- liftCell $ handling portMidiHandle -< ()
  hoistCell unPortMidiT cell -< a

{- | Repeatedly run a cell containing PortMidi effects.

Effectively loops over 'runPortMidiC',
and prints the exception after it occurred.
-}
loopPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> Cell (HandlingStateT m) a b
loopPortMidiC cell = foreverC $ runCellExcept $ do
  e <- runPortMidiC cell
  once_ $ liftIO $ do
    putStrLn "Encountered PortMidi exception:"
    print e
    threadDelay 1000
  return e

{- | Execute the 'PortMidiT' effects'.

This returns the first occurring exception.
For details on how to automatically start and garbage collect handles,
such as the PortMidi backend and devices,
see "LiveCoding.HandlingState".

You will rarely need this function.
Look at 'runPortMidiC' and 'loopPortMidiC' instead.
-}
runPortMidiT :: PortMidiT m a -> HandlingStateT m (Either EOLCPortMidiError a)
runPortMidiT PortMidiT { .. } = runExceptT unPortMidiT

-- * Input- and output streams

-- | A stream associated to a PortMidi input device
newtype PortMidiInputStream = PortMidiInputStream { unPortMidiInputStream :: PMStream }

-- | A stream associated to a PortMidi output device
newtype PortMidiOutputStream = PortMidiOutputStream { unPortMidiOutputStream :: PMStream }

-- | A marker to specify which kind of device to search
data DeviceDirection = Input | Output

{- | Look up a PortMidi device by its name and direction.

You will rarely need this function.
Consider 'readEventsC' and 'writeEventsC' instead.
-}
lookupDeviceID
  :: MonadIO m
  => String
  -> DeviceDirection
  -> m (Either EOLCPortMidiError DeviceID)
lookupDeviceID nameLookingFor inputOrOutput = do
  nDevices <- liftIO countDevices
  -- This is a bit of a race condition, but PortMidi has no better API
  devices <- forM [0..nDevices-1] $ \deviceID -> do
    deviceInfo <- liftIO $ getDeviceInfo deviceID
    return (deviceInfo, deviceID)
  let allDevicesWithName = filter ((nameLookingFor ==) . name . fst) devices
      inputDevices = filter (input . fst) allDevicesWithName
      outputDevices = filter (output . fst) allDevicesWithName
  return $ case (inputOrOutput, inputDevices, outputDevices) of
    (_, [], []) -> Left NoSuchDevice
    (Input, [], _ : _) -> Left NotAnInputDevice
    (Output, _ : _, []) -> Left NotAnOutputDevice
    (Input, [(_, deviceID)], _) -> Right deviceID
    (Output, _, [(_, deviceID)]) -> Right deviceID
    _ -> Left MultipleDevices

-- | A 'Handle' that opens a 'PortMidiInputStream' of the given device name.
portMidiInputStreamHandle
  :: MonadIO m
  => String
  -> Handle m (Either EOLCPortMidiError PortMidiInputStream)
portMidiInputStreamHandle name = Handle
  { create = runExceptT $ do
      deviceID <- ExceptT $ lookupDeviceID name Input
      fmap PortMidiInputStream $ withExceptT PMError $ ExceptT $ liftIO $ openInput deviceID
  -- TODO I don't get the error from closing here.
  -- Actually I really want ExceptT in the monad
  , destroy = either (const $ return ()) $ liftIO . void . close . unPortMidiInputStream
  }

-- | Read all events from the 'PortMidiInputStream' that accumulated since the last tick.
readEventsFrom
  :: MonadIO m
  => Cell (PortMidiT m) PortMidiInputStream [PMEvent]
readEventsFrom = arrM $ liftPMError . liftIO . readEvents . unPortMidiInputStream

{- | Read all events from the input device of the given name.

Automatically opens the device.

This is basically a convenient combination of 'portMidiInputStreamHandle' and 'readEventsFrom'.
-}
readEventsC
  :: MonadIO m
  => String -> Cell (PortMidiT m) arbitrary [PMEvent]
readEventsC name = proc _ -> do
  pmStreamE <- liftHandlingState $ handling $ portMidiInputStreamHandle name -< ()
  pmStream <- hoistCell PortMidiT exceptC -< pmStreamE
  readEventsFrom -< pmStream

-- | A 'Handle' that opens a 'PortMidiOutputStream' of the given device name.
portMidiOutputStreamHandle
  :: MonadIO m
  => String
  -> Handle m (Either EOLCPortMidiError PortMidiOutputStream)
portMidiOutputStreamHandle name = Handle
  { create = runExceptT $ do
      deviceID <- ExceptT $ lookupDeviceID name Output
      -- Choose same latency as supercollider, see https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangPrimSource/SC_PortMIDI.cpp#L416
      -- Thanks Miguel Negrão
      fmap PortMidiOutputStream $ withExceptT PMError $ ExceptT $ liftIO $ openOutput deviceID 0
  , destroy = either (const $ return ()) $ liftIO . void . close . unPortMidiOutputStream
  }

-- | Write all events to the 'PortMidiOutputStream'.
writeEventsTo
  :: MonadIO m
  => Cell (PortMidiT m) (PortMidiOutputStream, [PMEvent]) ()
writeEventsTo = arrM writer
  where
    writer (PortMidiOutputStream { .. }, events) = writeEvents unPortMidiOutputStream events
      & liftIO
      & liftPMError
      & void

{- | Write all events to the output device of the given name.

Automatically opens the device.

This is basically a convenient combination of 'portMidiOutputStreamHandle' and 'writeEventsTo'.
-}
writeEventsC
  :: MonadIO m
  => String
  -> Cell (PortMidiT m) [PMEvent] ()
writeEventsC name = proc events -> do
  portMidiOutputStreamE <- liftHandlingState $ handling (portMidiOutputStreamHandle name) -< ()
  portMidiOutputStream <- hoistCell PortMidiT exceptC -< portMidiOutputStreamE
  writeEventsTo -< (portMidiOutputStream, events)

-- | All devices that the PortMidi backend has connected.
data PortMidiDevices = PortMidiDevices
  { inputDevices :: [DeviceInfo]
  , outputDevices :: [DeviceInfo]
  }

-- | Retrieve all PortMidi devices.
getPortMidiDevices :: IO PortMidiDevices
getPortMidiDevices = do
  nDevices <- countDevices
  devices <- mapM getDeviceInfo [0..nDevices-1]
  return PortMidiDevices
    { inputDevices = filter input devices
    , outputDevices = filter output devices
    }

-- | Print input and output devices separately, one device per line.
prettyPrintPortMidiDevices :: PortMidiDevices -> IO ()
prettyPrintPortMidiDevices PortMidiDevices { .. } = do
  putStrLn "\nPortMidi input devices:"
  putStrLn $ unlines $ printName <$> inputDevices
  putStrLn "\nPortMidi output devices:"
  putStrLn $ unlines $ printName <$> outputDevices
  where
    printName dev = "- \"" ++ name dev ++ "\""
