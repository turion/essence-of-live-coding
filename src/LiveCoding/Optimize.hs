module LiveCoding.Optimize where

-- base
import Data.Data

-- syb
import Data.Generics.Aliases

-- essenceoflivecoding
import LiveCoding.Cell
import LiveCoding.LiveProgram


type LiveProgramContent m s = (s, s -> m s)

leftUnit :: LiveProgramContent m (Composition () s) -> LiveProgramContent m s
leftUnit (Composition ((), s), step) = (s, fmap getState2 . step . Composition . ((), ))

optimize :: LiveProgram -> LiveProgram
optimize LiveProgram { .. } = let
  (optimizedState, optimized

Should maybe optimize Cells and not programs?
