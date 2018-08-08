module GOperators (
  mutate,
  crossTrees
) where

import System.Random (randomRIO, randomIO)

import Tree (randomSTree)
import Types 

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

