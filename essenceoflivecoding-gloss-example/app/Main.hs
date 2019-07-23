{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
-- base
import Control.Arrow hiding (left, right)
import qualified Control.Arrow as Arrow

import Data.Data

import Prelude hiding (Bounded)

-- transformers
import Control.Monad.Trans.Accum

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer

-- syb
import Data.Generics.Aliases (ext2Q)

-- gloss
import Graphics.Gloss hiding (translate)
import qualified Graphics.Gloss as Gloss

-- essenceoflivecoding
import LiveCoding

-- essenceoflivecoding-gloss
import LiveCoding.Gloss hiding (statePicture, every)

main :: IO ()
main = playCellForeground glossCell

glossCell' :: GlossCell
glossCell' = arr (const ()) >>> sine 3 >>> arr realToFrac >>> arr circleThing >>> addPicture
  where
    circleThing x = Gloss.translate (x * 10 - 90) 0 myCircle
    myCircle = color white $ thickCircle 10 20

glossCell :: GlossCell
glossCell = withDebuggerC glossCell' glossDebugger

-- * To be ported to essenceoflivecoding-gloss (and delete the Arrows)

statePicture :: Data s => s -> Picture
statePicture = Gloss.translate (-100) 100 . scale 0.1 0.1 . color red . getPic . stateBoundedPic

stateBoundedPic :: Data s => s -> BoundedPic
stateBoundedPic =
  (boundPic . text . stateShow)
  `ext2Q` compPic

data Bounds = Bounds
  { left   :: Float
  , right  :: Float
  , bottom :: Float
  , top    :: Float
  }

instance Semigroup Bounds where
  bounds1 <> bounds2 = Bounds
    { left   = min (left   bounds1) (left   bounds2)
    , right  = max (right  bounds1) (right  bounds2)
    , bottom = min (bottom bounds1) (bottom bounds2)
    , top    = max (top    bounds1) (top    bounds2)
    }

instance Monoid Bounds where
  mempty = boundPoint (0, 0)

boundPoint :: Point -> Bounds
boundPoint (x, y) = Bounds x x y y

boundPath :: Path -> Bounds
boundPath path = mconcat $ boundPoint <$> path

pad :: Float -> Bounds -> Bounds
pad padding Bounds { .. } = Bounds
  { left   = left   - padding
  , right  = right  + padding
  , bottom = bottom - padding
  , top    = top    + padding
  }

translateBounds :: Point -> Bounds -> Bounds
translateBounds (x, y) Bounds { .. } = Bounds
  { left   = left   + x
  , right  = right  + x
  , bottom = bottom + y
  , top    = top    + y
  }

scaleBounds :: Float -> Float -> Bounds -> Bounds
scaleBounds x y Bounds { .. } = Bounds
  { left   = left   * x
  , right  = right  * x
  , bottom = bottom * y
  , top    = top    * y
  }

type Bounded a = Accum Bounds a

type BoundedPic = Bounded Picture

-- deriving via Ap Bounded Picture instance Monoid BoundedPic

getBounds :: BoundedPic -> Bounds
getBounds = flip execAccum mempty

getPic :: BoundedPic -> Picture
getPic  = flip evalAccum mempty

transformBounds :: (Bounds -> Bounds) -> BoundedPic -> BoundedPic
transformBounds f = mapAccumT $ fmap $ Arrow.second f

transformPic :: (Picture -> Picture) -> BoundedPic -> BoundedPic
transformPic = fmap

translate :: Float -> Float -> BoundedPic -> BoundedPic
translate x y = transformBounds (translateBounds (x, y)) . transformPic (Gloss.translate x y)

bounds :: Picture -> Bounds
bounds Blank = mempty
bounds (Polygon path) = boundPath path
bounds (Line path) = boundPath path
bounds (Circle r) = Bounds r r r r
bounds (ThickCircle _ r) = Bounds r r r r
bounds (Arc _ _ r) = Bounds r r r r -- TODO Not the tightest bound
bounds (ThickArc _ _ _ r) = Bounds r r r r -- TODO Not the tightest bound
bounds (Text s) = Bounds 0 (fromIntegral $ length s * 105) (-33.3) (120) -- TODO Only rough estimates from https://hackage.haskell.org/package/GLUT-2.7.0.12/docs/Graphics-UI-GLUT-Fonts.html
bounds (Bitmap bitmapData) = let (right, top) = bitmapSize bitmapData in Bounds 0 (fromIntegral right) 0 (fromIntegral top) -- TODO Untested
bounds (BitmapSection _ bitmapData) = let (right, top) = bitmapSize bitmapData in Bounds 0 (fromIntegral right) 0 (fromIntegral top) -- TODO Untested
bounds (Color _ pic) = bounds pic
bounds (Translate x y pic) = translateBounds (x, y) $ bounds pic
bounds (Rotate angle pic) = error "Too hard right now" -- TODO
bounds (Scale x y pic) = scaleBounds x y $ bounds pic
bounds (Pictures pics) = mconcat $ bounds <$> pics

boundPic :: Picture -> BoundedPic
boundPic picture = accum $ const (picture, bounds picture)

resetLeft :: BoundedPic -> BoundedPic
resetLeft boundedPic = translate (- (left $ getBounds boundedPic)) 0 boundedPic

distance = 5

addRight :: BoundedPic -> BoundedPic -> BoundedPic
addRight bpic1 bpic2 = do
  pic1 <- bpic1
  x <- looks right
  pic2 <- translate (x + distance) 0 $ resetLeft bpic2
  return $ pic1 <> pic2

center :: BoundedPic -> BoundedPic
center boundPic = let Bounds { .. } = getBounds boundPic in translate (left + right) (bottom + top) boundPic

compPic :: (Data s1, Data s2) => Composition s1 s2 -> BoundedPic
compPic (Composition (s1, s2))
  | isUnit s1 = stateBoundedPic s2
  | isUnit s2 = stateBoundedPic s1
  | otherwise = stateBoundedPic s1 `addRight` stateBoundedPic s2

glossDebugger :: Debugger PictureM
glossDebugger = Debugger $ liveCell $ every 100 >>> keep blank >>> arrM (lift . tell)

every :: Data s => Integer -> Cell (StateT s PictureM) () (Maybe Picture)
every maxN = proc () -> do
  n <- sumC -< 1
  if n `mod` maxN == 0
  then do
    s <- getC -< ()
    let pic = statePicture s
    returnA -< Just pic
  else
    returnA  -< Nothing
