module Tree (
  evalSTree ,
  randomSTree
) where

import Data.Map
import System.Random (randomRIO, randomIO)

import Types

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


