import Control.Monad.Trans.State
import System.Random

data Variable = X | Y deriving (Show, Enum)

data Expr = Value Variable 
        | Const Float
	| Mult Expr Expr 
	| Div Expr Expr 
	| Add Expr Expr
	| Sub Expr Expr deriving (Show)

functions :: [Expr -> Expr -> Expr]
functions = [Mult, Div, Add, Sub]

constructRandomTree :: Int -> State StdGen Expr 
constructRandomTree 0 = do
			  gen <- get
                          let (pick, newGen) = randomR (0,1) gen :: (Int,StdGen)
                          if pick == 0 
                            then do
                               let (pick, newGen2) = randomR (0,1) newGen :: (Int,StdGen)
                               put newGen2
                               return $ Value (toEnum pick :: Variable)
                            else do
                               let (pick, newGen2) = randomR (0,9) newGen :: (Float,StdGen)
                               put newGen2
                               return $ Const pick
                          
constructRandomTree x = do
                           gen <- get
                           let (pick, newGen) = randomR (0,3) gen
                           put newGen 
                           let start = functions !! pick
                           left <- constructRandomTree (x-1) 
                           right <- constructRandomTree (x-1)
                           return $ start left right
                            

eval :: Expr -> Float -> Float -> Float
eval (Const z) _ _ = z
eval (Value X) x _ = x
eval (Value Y) _ y = y
eval (Div left right) x y = (/) (eval left x y) (eval right x y)
eval (Sub left right) x y = (-) (eval left x y) (eval right x y)
eval (Mult left right) x y = (*) (eval left x y) (eval right x y)
eval (Add left right) x y = (+) (eval left x y) (eval right x y)





                              
                              

