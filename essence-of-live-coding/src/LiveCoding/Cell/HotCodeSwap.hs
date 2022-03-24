module LiveCoding.Cell.HotCodeSwap where

-- essence-of-live-coding
import LiveCoding.Cell
import LiveCoding.Migrate

hotCodeSwapCell ::
  Cell m a b ->
  Cell m a b ->
  Cell m a b
hotCodeSwapCell
  (Cell newState newStep)
  (Cell oldState _) =
    Cell
      { cellState = migrate newState oldState,
        cellStep = newStep
      }
