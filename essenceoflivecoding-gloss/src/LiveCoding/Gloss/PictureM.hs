module LiveCoding.Gloss.PictureM where

-- transformers
import Control.Monad.Trans.Writer

-- gloss
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- essenceoflivecoding
import LiveCoding

type PictureM = WriterT Picture IO

runPictureM :: GlossCell -> Cell IO [Event] Picture
runPictureM = transformOutput $ fmap massageWriterOutput . runWriterT

massageWriterOutput (((), s), pic) = (pic, s)

-- TODO Rhine integration instead of fixed sample size
type GlossCell = Cell PictureM [Event] ()

addPicture :: Cell PictureM Picture ()
addPicture = arrM tell
