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

data EOLCPortMidiError
  = PMError PMError
  | NoSuchDevice
  | NotAnInputDevice
  | NotAnOutputDevice
  deriving (Data, Generic, Show)

instance Finite EOLCPortMidiError

-- | The monad transformer of PortMidi exceptions
newtype PortMidiT m a = PortMidiT
  { unPortMidiT :: ExceptT EOLCPortMidiError (HandlingStateT m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runPortMidiT :: PortMidiT m a -> HandlingStateT m (Either EOLCPortMidiError a)
runPortMidiT PortMidiT { .. } = runExceptT unPortMidiT

throwPortMidi :: Monad m => EOLCPortMidiError -> PortMidiT m arbitrary
throwPortMidi = PortMidiT . throwE

throwPortMidiC :: Monad m => Cell (PortMidiT m) EOLCPortMidiError arbitrary
throwPortMidiC = arrM throwPortMidi

instance MonadTrans PortMidiT where
  lift = PortMidiT . lift . lift

liftPMError :: Monad m => m (Either PMError a) -> PortMidiT m a
liftPMError = PortMidiT . ExceptT . fmap (left PMError) . lift

liftHandlingState :: Monad m => Cell (HandlingStateT m) a b -> Cell (PortMidiT m) a b
liftHandlingState = hoistCell $ PortMidiT . lift

-- | Initialize the MIDI system, run the action, and shut it down again.
runPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> CellExcept (HandlingStateT m) a b EOLCPortMidiError
runPortMidiC cell = try $ proc a -> do
  _ <- liftCell $ handling portMidiHandle -< ()
  hoistCell unPortMidiT cell -< a

loopPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> Cell (HandlingStateT m) a b
loopPortMidiC cell = foreverC $ runCellExcept $ do
  e <- runPortMidiC cell
  once_ $ liftIO $ do
    putStrLn "Encountered PortMidi exception:"
    print e
    threadDelay 1000
  return e

deriving instance Data PMError
deriving instance Generic PMError
instance Finite PMError

-- | A marker witnessing that PortMidi was initialized
data PortMidiHandle = PortMidiHandle

portMidiHandle :: MonadIO m => Handle m PortMidiHandle
portMidiHandle = Handle
  { create = do
      liftIO initialize
      return PortMidiHandle
  , destroy = const $ liftIO $ void terminate
  }

newtype PortMidiInputStream = PortMidiInputStream { unPortMidiInputStream :: PMStream }
newtype PortMidiOutputStream = PortMidiOutputStream { unPortMidiOutputStream :: PMStream }

lookupDeviceID :: MonadIO m => String -> (DeviceInfo -> Maybe EOLCPortMidiError) -> m (Either EOLCPortMidiError DeviceID)
lookupDeviceID nameLookingFor filter = do
  nDevices <- liftIO countDevices
  -- This is a bit of a race condition, but PortMidi has no better API
  devices <- forM [0..nDevices-1] $ \deviceID -> do
    deviceInfo <- liftIO $ getDeviceInfo deviceID
    return (deviceInfo, deviceID)
  let maybeDevice = find ((nameLookingFor ==) . name . fst) devices
  return $ do
    (deviceInfo, deviceID) <- maybe (Left NoSuchDevice) Right maybeDevice
    maybe (Right deviceID) Left $ filter deviceInfo

portMidiInputStreamHandle
  :: MonadIO m
  => String
  -> Handle m (Either EOLCPortMidiError PortMidiInputStream)
portMidiInputStreamHandle name = Handle
  { create = runExceptT $ do
      deviceID <- ExceptT $ lookupDeviceID name
        $ \DeviceInfo { .. }
          -> if input then Nothing else Just NotAnInputDevice
      fmap PortMidiInputStream $ withExceptT PMError $ ExceptT $ liftIO $ openInput deviceID
  -- TODO I don't get the error from closing here.
  -- Actually I really want ExceptT in the monad
  , destroy = either (const $ return ()) $ liftIO . void . close . unPortMidiInputStream
  }

readEventsFrom
  :: MonadIO m
  => Cell (PortMidiT m) PortMidiInputStream [PMEvent]
readEventsFrom = arrM $ liftPMError . liftIO . readEvents . unPortMidiInputStream

readEventsC
  :: MonadIO m
  => String -> Cell (PortMidiT m) arbitrary [PMEvent]
readEventsC name = proc _ -> do
  pmStreamE <- liftHandlingState $ handling $ portMidiInputStreamHandle name -< ()
  pmStream <- hoistCell PortMidiT inExceptT -< pmStreamE
  readEventsFrom -< pmStream

portMidiOutputStreamHandle
  :: MonadIO m
  => String
  -> Handle m (Either EOLCPortMidiError PortMidiOutputStream)
portMidiOutputStreamHandle name = Handle
  { create = runExceptT $ do
      deviceID <- ExceptT $ lookupDeviceID name
        $ \DeviceInfo { .. }
          -> if output then Nothing else Just NotAnOutputDevice
      -- Choose same latency as supercollider, see https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangPrimSource/SC_PortMIDI.cpp#L416
      -- Thanks Miguel Negrão
      fmap PortMidiOutputStream $ withExceptT PMError $ ExceptT $ liftIO $ openOutput deviceID 0
  , destroy = either (const $ return ()) $ liftIO . void . close . unPortMidiOutputStream
  }

writeEventsTo
  :: MonadIO m
  => Cell (PortMidiT m) (PortMidiOutputStream, [PMEvent]) ()
writeEventsTo = arrM writer
  where
    writer (PortMidiOutputStream { .. }, events) = writeEvents unPortMidiOutputStream events
      & liftIO
      & liftPMError
      & void

writeEventsC
  :: MonadIO m
  => String
  -> Cell (PortMidiT m) [PMEvent] ()
writeEventsC name = proc events -> do
  portMidiOutputStreamE <- liftHandlingState $ handling (portMidiOutputStreamHandle name) -< ()
  portMidiOutputStream <- hoistCell PortMidiT inExceptT -< portMidiOutputStreamE
  writeEventsTo -< (portMidiOutputStream, events)

data PortMidiDevices = PortMidiDevices
  { inputDevices :: [DeviceInfo]
  , outputDevices :: [DeviceInfo]
  }

getPortMidiDevices :: IO PortMidiDevices
getPortMidiDevices = do
  nDevices <- countDevices
  devices <- mapM getDeviceInfo [0..nDevices-1]
  return PortMidiDevices
    { inputDevices = filter input devices
    , outputDevices = filter output devices
    }

prettyPrintPortMidiDevices :: PortMidiDevices -> IO ()
prettyPrintPortMidiDevices PortMidiDevices { .. } = do
  putStrLn "\nPortMidi input devices:"
  putStrLn $ unlines $ printName <$> inputDevices
  putStrLn "\nPortMidi output devices:"
  putStrLn $ unlines $ printName <$> outputDevices
  where
    printName dev = "- \"" ++ name dev ++ "\""
