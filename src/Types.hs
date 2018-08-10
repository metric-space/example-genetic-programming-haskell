module Types
  ( ENV(..)
  , STreeENV(..)
  , STree(..)
  , P(..)
  , Aop(..)
  , EvalFunc
  ) where

-- ======================    the ENVIRONMENT    =============================================
data STreeENV = STreeENV
  { maxDepth :: Int
  , fpr :: Float
  , ppr :: Float
  }

data ENV = ENV
  { mutationRate :: Float
  , crossRate :: Float
  , populationSize :: Int
  , maxGen :: Int
  , pexp :: Float
  , pnew :: Float
  , treeParams :: STreeENV
  , evalFunction :: EvalFunc
  }

-- =====================  TREE DATATYPES =================================================
-- Arithmetic operators
data Aop
  = MULT
  | ADD
  | SUB
  deriving (Eq, Enum)

-- do I really fucking the need the Ord?
data P
  = X
  | Y
  deriving (Eq, Show, Enum, Ord)

data STree
  = Arithmetic Aop
               STree
               STree
  | Constant Int
  | Param P
  deriving (Eq)

instance Show Aop where
  show MULT = "*"
  show ADD = "+"
  show SUB = "-"

instance Show STree where
  show (Constant x) = show x
  show (Param x) = show x
  show (Arithmetic x a b) =
    "(" <> show a <> " " <> show x <> " " <> show b <> ")"

type EvalFunc = STree -> Int
