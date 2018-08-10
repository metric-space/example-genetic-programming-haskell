module Utils
  ( selectIndex
  , defaultENV
  ) where

import System.Random (randomIO)

import Types (ENV(..), STreeENV(..))

selectIndex :: Float -> IO Int
selectIndex pexp = (round . flip (/) (log pexp) . log) <$> randomIO

defaultENV :: ENV
defaultENV = ENV 0.1 0.4 500 500 0.7 0.05 (STreeENV 4 0.5 0.6) (const 1000000)
