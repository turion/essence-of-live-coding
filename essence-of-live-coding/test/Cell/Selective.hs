module Cell.Selective where

-- base
import Control.Arrow
import Control.Category (id)
import Prelude hiding (id)

-- selective
import Control.Selective

-- test-framework
import Test.Framework (testGroup)

-- test-framework-quickcheck2
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Util (CellSimulation(..))

-- essence-of-live-coding
import LiveCoding.Preliminary.CellExcept
import LiveCoding.Preliminary.CellExcept.Selective

test = testGroup "Selective"
  [ testProperty "Short circuits" CellSimulation
      { cell = safely
          $ pure (Left ())
          <*? safe id
      , input = "hello"
      , output = "hello"
      }
  ]
