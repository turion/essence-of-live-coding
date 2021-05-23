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
import Control.Monad (void, forM, join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (fromRight)
import Data.Foldable (traverse_)
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
newtype PortMidiT m a = PortMidiT { unPortMidi :: ExceptT EOLCPortMidiError m a }
  deriving (Functor, Applicative, Monad, MonadIO)

throwPortMidi :: Monad m => EOLCPortMidiError -> PortMidiT m arbitrary
throwPortMidi = PortMidiT . throwE

throwPortMidiC :: Monad m => Cell (PortMidiT m) EOLCPortMidiError arbitrary
throwPortMidiC = arrM throwPortMidi

instance MonadTrans PortMidiT where
  lift = PortMidiT . lift

liftPMError :: Functor m => m (Either PMError a) -> PortMidiT m a
liftPMError = PortMidiT . ExceptT . fmap (left PMError)

-- | Initialize the MIDI system, run the action, and shut it down again.
runPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> CellExcept (HandlingStateT m) a b EOLCPortMidiError
runPortMidiC cell = try $ proc a -> do
  _ <- liftCell $ handling portMidiHandle -< ()
  hoistCell (mapExceptT lift . unPortMidi) cell -< a

loopPortMidiC :: MonadIO m => Cell (PortMidiT m) a b -> Cell (HandlingStateT m) a b
loopPortMidiC cell = foreverC $ runCellExcept $ do
  e <- runPortMidiC cell
  once_ $ liftIO $ do
    putStrLn "Encountered PortMidi exception:"
    print e
  return e

deriving instance Data PMError
deriving instance Generic PMError
instance Finite PMError

-- FIXME do we still need this?
-- | A marker to make sure that PortMidi was initialized
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

lookupDeviceID :: MonadIO m => String -> (DeviceInfo -> Maybe EOLCPortMidiError) -> PortMidiT m DeviceID
lookupDeviceID nameLookingFor filter = do
  nDevices <- liftIO countDevices
  devices <- forM [0..nDevices-1] $ \deviceID -> do
    deviceInfo <- liftIO $ getDeviceInfo deviceID
    traverse_ throwPortMidi $ filter deviceInfo
    return (name deviceInfo, deviceID)
  maybe (throwPortMidi NoSuchDevice) return $ lookup nameLookingFor devices

portMidiInputStreamHandle
  :: MonadIO m
  => String
  -> Handle (PortMidiT m) PortMidiInputStream
portMidiInputStreamHandle name = Handle
  { create = do
      deviceID <- lookupDeviceID name $ \DeviceInfo { .. } -> if input then Nothing else Just NotAnInputDevice
      PortMidiInputStream <$> liftPMError (liftIO (openInput deviceID))
  , destroy = liftIO . void . close . unPortMidiInputStream
  }

readEventsFrom
  :: MonadIO m
  => Cell (PortMidiT m) PortMidiInputStream [PMEvent]
readEventsFrom = arrM $ liftPMError . liftIO . readEvents . unPortMidiInputStream

readEventsC
  :: MonadIO m
  => String -> Cell (HandlingStateT (PortMidiT m)) arbitrary [PMEvent]
readEventsC name = proc _ -> do
  pmstream <- handling $ portMidiInputStreamHandle name -< ()
  liftCell readEventsFrom -< pmstream

portMidiOutputStreamHandle
  :: MonadIO m
  => String
  -> Handle (PortMidiT m) PortMidiOutputStream
portMidiOutputStreamHandle name = Handle
  { create = do
      deviceID <- lookupDeviceID name $ \DeviceInfo { .. } -> if output then Nothing else Just NotAnOutputDevice
      -- Choose same latency as supercollider, see https://github.com/supercollider/supercollider/blob/18c4aad363c49f29e866f884f5ac5bd35969d828/lang/LangPrimSource/SC_PortMIDI.cpp#L416
      -- Thanks Miguel Negrão
      PortMidiOutputStream <$> liftPMError (liftIO (openOutput deviceID 0))
  , destroy = liftIO . void . close . unPortMidiOutputStream
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
  :: String -> Cell (HandlingStateT (PortMidiT IO)) [PMEvent] ()
writeEventsC name = proc events -> do
  portMidiOutputStream <- handling (portMidiOutputStreamHandle name) -< ()
  liftCell writeEventsTo -< (portMidiOutputStream, events)

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
