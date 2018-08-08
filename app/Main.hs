module Main (P(..), Aop(..), STree(..), main) where

import Control.Monad (foldM)
import Data.Map.Strict ((!), Map, fromList)
import Data.List (sortOn)
import System.Random

import Lib

-- ======================    The problem  ===================================================

hiddenFunction :: Int -> Int -> Int
hiddenFunction x y = x*x + 2*y + 3*x + 5 


-- ======================    the ENVIRONMENT    =============================================

data STreeENV = STreeENV { maxDepth :: Int , fpr :: Double, ppr :: Double }

data ENV = ENV {mutationRate :: Double , crossRate :: Double,  
                populationSize :: Int, maxGen :: Int,
                pexp :: Double, pnew :: Double, treeParams :: STreeENV}


defaultENV :: ENV
defaultENV = ENV 0.1 0.4 500 500 0.7 0.05 (STreeENV 4 0.5 0.6)

-- =====================  TREE DATATYPES =================================================
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
                  

-- ======================= Genetic operators ===============================================

mutateTree :: STree -> ENV ->  IO STree
mutateTree (Arithmetic x b1 b2) env = (Arithmetic x) <$> (mutate b1 env)  <*> (mutate b2 env)
mutateTree x _ = return x


mutate :: STree -> ENV -> IO STree
mutate tree env = do
                    iprob <- randomIO
                    if iprob > (mutationRate env)
                       then (mutateTree tree env)
                       else (randomSTree . treeParams $ env)



across :: ENV -> STree -> STree ->  IO STree
across env (Arithmetic a1 ta1 ta2) (Arithmetic _ tb1 tb2) 
       = do
            tb1_ <- fmap ([tb1, tb2]!!) $ randomRIO (0,1)
            tb2_ <- fmap ([tb1, tb2]!!) $ randomRIO (0,1)
            Arithmetic a1 <$> (cross (False, env) ta1 tb1_) <*> (cross (False, env) ta2 tb2_)
across _ t1 _ = return t1


cross :: (Bool, ENV) -> STree -> STree -> IO STree
cross (top, env) t1 t2 = do
                           prob <- randomIO 
                           if (not top) && prob < (crossRate env)
                              then return t2
                              else (across env t1 t2)


crossTrees :: ENV -> STree -> STree -> IO STree
crossTrees env t1 t2 = cross (True, env) t1 t2

-- ====================   Utils ===================================================================  

selectIndex :: Double -> IO Int
selectIndex pexp = randomIO >>= (return . round . flip (/) (log pexp) . log)

-- ===================== main prog ==============================================================


genPop :: ([(Map P Int, Int)],ENV) -> [STree] -> [(Int,STree)] -> IO [(Int,STree)]
genPop (mx,env) start prevpop = if (length start == (populationSize env))  
                                  then return . (sortOn fst) . map (\x -> (evaluateFitness x mx,x)) $ start
                                  else do 
                                         p <- randomIO 
                                         n <- (if p > (pnew env) 
                                                  then do 
                                                        let pe = pexp env
                                                        t1 <- (snd . (prevpop !!)) <$> (selectIndex pe)
                                                        t2 <- (snd . (prevpop !!)) <$> (selectIndex pe) 
                                                        (crossTrees env t1 t2) >>= flip mutate env
                                                  else  randomSTree (treeParams env))
                                         genPop (mx,env) (start++[n]) prevpop
                                         
                        

circleOfLife :: ([(Map P Int, Int)],ENV) -> ([(Int,STree)], Bool) -> Int -> IO ([(Int,STree)], Bool)
circleOfLife env acc@(ppop@((s1,t1):(s2,t2):xs),stop) _ 
  | stop = return acc
  | (s1 == 0) = return ((const True) <$> acc)
  | otherwise = (genPop env [t1,t2] ppop) >>= return . (flip (,) False)


evolve :: ENV -> IO ()
evolve env
       = do
           population <- traverse (const . randomSTree $ (treeParams env)) [0 .. (populationSize env)]
           metricSet <- buildHiddenSet
           let rpopulation =  (sortOn fst) . map (\x -> (evaluateFitness x metricSet,x)) $  population :: [(Int, STree)]
           (result ,_) <-  foldM (circleOfLife (metricSet,env)) (rpopulation, False) [0 .. (maxGen env)]
           let winner = head result
           putStrLn $  "Score is " ++ (show . fst $ winner)
           putStrLn . show . snd $ winner


main :: IO ()
main = putStrLn "hello"
