module Evolve (evolve) where

import Control.Monad (foldM)
import Data.Map.Strict ((!), Map, fromList)
import Data.List (sortOn)
import System.Random (randomIO)

import Tree (randomSTree)
import Types
import Utils (selectIndex)
import GOperators (crossTrees, mutate)


genPop :: ([(Map P Int, Int)],ENV) -> [STree] -> [(Int,STree)] -> IO [(Int,STree)]
genPop (mx,env) start prevpop = if (length start == (populationSize env))  
                                  then return . (sortOn fst) . map (\x -> ((evalFunction env) x,x)) $ start
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


evolve :: [(Map P Int, Int)] -> ENV -> IO ()
evolve metricSet env
       = do
           population <- traverse (const . randomSTree $ (treeParams env)) [0 .. (populationSize env)]
           let fitnessFunction = evalFunction env 
               rpopulation =  (sortOn fst) . map (\x -> (fitnessFunction x ,x)) $  population :: [(Int, STree)]
           (result ,_) <-  foldM (circleOfLife (metricSet,env)) (rpopulation, False) [0 .. (maxGen env)]
           let winner = head result
           putStrLn $  "Score is " ++ (show . fst $ winner)
           putStrLn . show . snd $ winner


