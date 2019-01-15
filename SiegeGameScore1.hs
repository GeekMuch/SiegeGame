module SiegeGameScore1 where
import Game
import SiegeGame
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (partition)

infinity :: Double
infinity = 1/0

inL :: V -> Bool
inL (x,y) = x+y <= 0

inR :: V -> Bool
inR (x,y) = -x+y <= 0

cond :: V -> V -> Bool
cond g@(_, 3)  v = True
cond g@(-1, _) v = inL v
cond g@(1, _)  v = inR v
cond _         v = inL v && inR v

score1 :: Map V VertexInfo -> Map V Player -> Double
score1 g m = sum $ map s goals
  where
    goals         = [ v | (v,vi) <- M.toList g, isGoal vi]
    nonGoals      = [ v | (v,vi) <- M.toList g, not $ isGoal vi]
    potential v   = partition (\v -> M.lookup v m == Just True) $ filter (cond v) nonGoals
    s v           = case M.lookup v m of
                      Just True -> 2
                      res       -> let  (as,ns) = potential v
                                        (a,n)   = (fromIntegral $ length as, fromIntegral $ length ns)
                                   in   (if res == Just False then -1 else 0) + (a-n) / (a+n)
