{-# LANGUAGE FlexibleContexts                  #-}
module GameStrategies where
import Game
import Data.List (intercalate)

data Tree m v   = Tree v [(m,Tree m v)]
type GTree g    = Tree (Move g) (Player,GameState g)

type AlphaBeta = (Value,Value)
type Depth = Int

instance (Show m, Show v) => Show (Tree m v) where
  show (Tree x xs) = "Tree " ++ show x ++ " ["++ sub' ++"]"
    where
      sub = intercalate ",\n" (map show xs)
      sub' = if null sub
                then ""
                else "\n" ++ unlines (map ("  " ++ ) $ lines sub)


startTree :: Game g => g -> Player -> GTree g
startTree g p = tree g (p, startState g)

-- Task 2.1)
tree :: Game g => g -> (Player, GameState g) -> GTree g
tree g ps@(p,s) = Tree ps (map mTree (moves g p s))
 where
  mTree mv = (mv, tree g (not p, move g p s mv))

-- Task 2.2)
takeTree :: Depth -> Tree m v -> Tree m v
takeTree 0 (Tree v _) = Tree v []
takeTree i (Tree v x) = Tree v [(mv, t)| (mv, x') <- x, let t = takeTree ( i - 1) x']

-- Task 2.3)
minimax :: Game g => g -> GTree g -> (Maybe (Move g), Value)
minimax = undefined

-- Task 2.4)
minimaxAlphaBeta :: Game g => g -> AlphaBeta -> GTree g -> (Maybe (Move g), Value)
minimaxAlphaBeta = undefined
