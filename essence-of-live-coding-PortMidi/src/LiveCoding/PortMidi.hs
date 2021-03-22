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
import Data.Maybe (catMaybes)
import GHC.Generics
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)

-- transformers
import Control.Monad.Trans.Class

-- PortMidi
import Sound.PortMidi

-- essence-of-live-coding
import LiveCoding
import LiveCoding.LiveProgram.Except (foreverCLiveProgram)
import qualified LiveCoding.LiveProgram.Except as LiveProgram

-- | The monad transformer of PortMidi exceptions
newtype PortMidiT m a = PortMidiT { unPortMidi :: ExceptT PMError m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans PortMidiT where
  lift = PortMidiT . lift

-- | Initialize the MIDI system, run the action, and shut it down again.
runPortMidiT :: MonadIO m => PortMidiT m a -> ExceptT PMError m a
runPortMidiT PortMidiT { .. } = ExceptT $ do
  liftIO initialize
  a <- runExceptT unPortMidi
  liftIO $ void terminate
  return a

instance (Launchable m, MonadIO m) => Launchable (PortMidiT m) where
  runIO liveProgram = runIO $ foreverCLiveProgram $ do
    e <- LiveProgram.try $ hoistLiveProgram unPortMidi liveProgram
    once $ liftIO $ do
      putStrLn "Encountered PortMidi exception:"
      print e
    return e
    where
      -- Will be part of essence-of-live-coding-0.2.6
      once :: (Monad m, Data e, Finite e) => m e -> LiveProgram.LiveProgramExcept m e
      once action = LiveProgram.try $ liveCell $ constM $ ExceptT $ Left <$> action

deriving instance Data PMError
deriving instance Generic PMError
instance Finite PMError

-- | A marker to make sure that PortMidi was initialized
data PortMidiHandle = PortMidiHandle

portMidiHandle :: MonadIO m => Handle m PortMidiHandle
portMidiHandle = Handle
  { create = do
      liftIO initialize
      return PortMidiHandle
  , destroy = const $ liftIO $ void terminate
  }

newtype PortMidiInputStream (name :: Symbol) = PortMidiInputStream { unPortMidiInputStream :: PMStream }

portMidiInputStreamAtProxyName :: Proxy name -> PMStream -> PortMidiInputStream name
portMidiInputStreamAtProxyName _ = PortMidiInputStream

portMidiInput
  :: (MonadIO m, KnownSymbol name)
  => PortMidiHandle
  -> Handle m (Maybe (PortMidiInputStream (name :: Symbol)))
portMidiInput _ = portMidiInputWithProxy Proxy

portMidiInputWithProxy
  :: (MonadIO m, KnownSymbol name)
  => Proxy (name :: Symbol)
  -> Handle m (Maybe (PortMidiInputStream (name :: Symbol)))
portMidiInputWithProxy proxy = Handle
  { create = liftIO $ do
      nDevices <- countDevices
      devices <- forM [0..nDevices-1] $ \deviceID -> do
        deviceInfo <- getDeviceInfo deviceID
        return $ if input deviceInfo then Just (name deviceInfo, deviceID) else Nothing
      let deviceID = lookup (symbolVal proxy) $ catMaybes devices
      pmStreamE <- mapM openInput deviceID
      let pmStream = either (const Nothing) Just =<< pmStreamE
      return $ portMidiInputStreamAtProxyName proxy <$> pmStream
  , destroy = maybe (return ()) $ liftIO . void . close . unPortMidiInputStream
  }

handlingPortMidiInput :: (KnownSymbol name, MonadIO m) => Cell (HandlingStateT m) PortMidiHandle (Maybe (PortMidiInputStream (name :: Symbol)))
handlingPortMidiInput = handling $ portMidiInputWithProxy Proxy
