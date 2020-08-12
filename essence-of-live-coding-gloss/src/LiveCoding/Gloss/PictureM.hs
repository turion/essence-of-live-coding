module LiveCoding.Gloss.PictureM
  ( module LiveCoding.Gloss.PictureM
  , module X
  ) where

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader as X
import Control.Monad.Trans.Writer

-- gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- essence-of-live-coding
import LiveCoding

{- | The monad transformer that captures the effects of gloss,
which are reading events and writing pictures.

You can call these effects for example by...

* ...using 'ask' to read the events that occurred,
* ...composing a cell with 'addPicture' to paint a picture.
-}
type PictureT m = ReaderT [Event] (WriterT Picture m)

-- | 'PictureT' specialised to the 'IO' monad.
type PictureM = PictureT IO

-- | Run the effects of the gloss monad stack by explicitly passing events and pictures.
runPictureT
  :: Monad m
  => Cell (PictureT m) a b
  -> Cell m ([Event], a) (Picture, b)
runPictureT = hoistCellOutput (fmap massageWriterOutput . runWriterT) . runReaderC'
  where
    massageWriterOutput :: ((b, s), pic) -> ((pic, b), s)
    massageWriterOutput ((b, s), pic) = ((pic, b), s)

addPicture :: Monad m => Cell (PictureT m) Picture ()
addPicture = arrM $ lift . tell
