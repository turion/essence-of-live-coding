{- |
Utilities for integrating live programs into external loops, using 'IO' concurrency.
The basic idea is two wormholes (see Winograd-Court's thesis).
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module LiveCoding.External where

-- base
import Control.Arrow
import Control.Concurrent
import Control.Monad.IO.Class

-- transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.Exceptions

type ExternalCell m eIn eOut a b = Cell (ReaderT eIn (WriterT eOut m)) a b

type ExternalLoop eIn eOut = Cell IO eIn eOut

runWriterC :: Monad m => Cell (WriterT w m) a b -> Cell m a (b, w)
runWriterC Cell { .. } = Cell
  { cellStep = \state a -> fmap (\((b, w), s) -> ((b, s), w)) $ runWriterT $ cellStep state a
  , ..
  }

concurrently :: MonadIO m => ExternalCell m eIn eOut a b -> IO (Cell m a b, ExternalLoop eIn eOut)
concurrently externalCell = do
  inVar  <- newEmptyMVar
  outVar <- newEmptyMVar
  let
    cell = proc a -> do
      eIn       <- constM (liftIO $ takeMVar inVar)      -< ()
      (b, eOut) <- runWriterC (runReaderC' externalCell) -< (eIn, a)
      arrM (liftIO . putMVar outVar)                     -< eOut
      returnA                                            -< b
    externalLoop = arrM (putMVar inVar) >>> constM (takeMVar outVar)
  return (cell, externalLoop)

type CellHandle a b = MVar (Cell IO a b)

makeHandle :: Cell IO a b -> IO (CellHandle a b)
makeHandle = newMVar

stepHandle :: CellHandle a b -> a -> IO b
stepHandle handle a = modifyMVar handle $ \cell -> do
  (b, cell') <- step cell a
  return (cell', b)
