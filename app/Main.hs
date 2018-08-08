module Main
  ( main
  ) where

import Data.Map (Map, fromList)
import System.Random (randomRIO)

import Evolve (evolve)
import Tree (evalSTree)
import Types (P(..), STree, evalFunction)
import Utils (defaultENV)

-- ======================    The problem  ===================================================
hiddenFunction :: Int -> Int -> Int
hiddenFunction x y = x * x + 2 * y + 3 * x + 5

-- ====================== FITNESS ======================================================
buildSingleSet :: IO (Map P Int, Int)
buildSingleSet = do
  x <- randomRIO (0, 40)
  y <- randomRIO (0, 40)
  return (fromList [(X, x), (Y, y)], hiddenFunction x y)

buildHiddenSet :: IO [(Map P Int, Int)]
buildHiddenSet = traverse (const buildSingleSet) [1 .. 200]

evaluateFitness :: STree -> [(Map P Int, Int)] -> Int
evaluateFitness tree =
  sum . map (\(m, value) -> abs . (-) value . evalSTree tree $ m)

-- ===================== main prog ==============================================================
main :: IO ()
main = do
  ms <- buildHiddenSet
  let def = defaultENV {evalFunction = flip evaluateFitness ms}
  evolve ms def
