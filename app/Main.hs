module Main (P(..), Aop(..), STree(..), main) where

import Data.Map.Strict ((!), Map, fromList)
import System.Random

import Lib


hiddenFunction :: Int -> Int -> Int
hiddenFunction x y = x*x + 2*y + 3*x + 5 


data STreeENV = STreeENV { maxDepth :: Int , fpr :: Double, ppr :: Double }

-- Arithmetic operators
data Aop = MULT | ADD | SUB deriving (Eq, Enum)


-- do I really fucking the need the Ord?
data P = X | Y deriving (Eq, Show, Enum, Ord)


data STree =  Arithmetic Aop STree STree
            | Constant Int
            | Param P
            deriving (Eq)


instance Show Aop where
  show MULT = "*"
  show ADD = "+"
  show SUB = "-"


instance Show STree where
  show (Constant x)  = show x
  show (Param x) = show x
  show (Arithmetic x a b) = "(" <> (show a) <> " " <> (show x) <> " " <> (show b) <> ")"


-- =======================   TREE EVALUATION ==========================================

-- change to safer version later
evalSTree :: STree -> Map P Int -> Int
evalSTree (Constant x) _ = x  
evalSTree (Param x) m = m ! x 
evalSTree (Arithmetic op o1 o2) m = case op of MULT -> (evalSTree o1 m) * (evalSTree o2 m) 
                                               ADD -> (evalSTree o1 m) + (evalSTree o2 m)
                                               SUB -> (evalSTree o1 m) - (evalSTree o2 m) 

-- =======================   TREE BUILDING =============================================

randomParameter :: IO P
randomParameter = randomRIO (0,1) >>= return . toEnum


randomAop :: IO Aop
randomAop = randomRIO (0,2) >>= return . toEnum


randomBranch ::  Double -> STreeENV  -> IO STree
randomBranch r env@(STreeENV maxdepth fpr ppr)
  | (r < fpr) && maxdepth > 0 = do 
                                a <- randomAop 
                                let nenv = env {maxDepth = maxdepth - 1 }
                                c1 <- randomSTree nenv
                                c2 <- randomSTree nenv
                                return (Arithmetic a c1 c2)
  | r < ppr = fmap Param randomParameter
  | otherwise = (fmap Constant) . randomRIO $ (1,10)


randomSTree ::  STreeENV -> IO STree
randomSTree env = randomIO >>= (flip randomBranch env)


-- ====================== FITNESS ======================================================

buildSingleSet :: IO (Map P Int, Int)
buildSingleSet = do
                  x <- randomRIO (0,40)
                  y <- randomRIO (0,40)
                  return (fromList [(X, x), (Y, y)], hiddenFunction x y)


buildHiddenSet :: IO [(Map P Int, Int)]
buildHiddenSet = traverse (const buildSingleSet) [1..200]



evaluateFitness :: STree -> [(Map P Int,Int)] -> Int
evaluateFitness tree lm = foldl1 (+) . map (\(m,value) -> abs . (-) value . evalSTree tree $ m) $ lm
                  


main :: IO ()
main = someFunc
